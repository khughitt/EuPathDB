#!/usr/bin/env Rscript
#
# EuPath AnnotationHub Recipe
# Author: Keith Hughitt (khughitt@umd.edu)
# November 05, 2015
#
# References
# ----------
# 1. http://bioconductor.org/packages/devel/bioc/vignettes/AnnotationHub/inst/doc/AnnotationHubRecipes.html
# 2. https://github.com/Bioconductor-mirror/AnnotationHubData/tree/master/R
#
library('AnnotationForge')
library('AnnotationHubData')
library('dplyr')

# Database URLs
eupathdb_database_urls <- list(
    "AmoebaDB"="http://amoebadb.org",
    "CryptoDB"="http://cryptodb.org",
    "FungiDB"="http://fungidb.org",
    "GiardiaDB"="http://giardiadb.org",
    "MicrosporidiaDB"="http://microsporidiadb.org",
    "PiroplasmaDB"="http://piroplasmadb.org",
    "PlasmoDB"="http://plasmodb.org",
    "ToxoDB"="http://toxodb.org",
    "TrichDB"="http://trichdb.org",
    "TriTrypDB"="http://tritrypdb.org"
)

# Current version
get_eupathdb_version <- function(baseurl) {
    library(RCurl)
    version_url <- sprintf('%s/common/downloads/Current_Release/Build_number', baseurl)
    gsub('\n', '', as.character(getURLContent(version_url)))
}

# Load EuPathDB supported organisms
.load_eupathdb_organisms <- function(version) {
    # Source: http://www.eupathdb.org/eupathdb/showApplication.do
    # TODO 2015/12/06
    # Switch to using REST API to query dynamically, e.g.
    # http://www.eupathdb.org/webservices/OrganismQuestions/GenomeDataTypes.json?o-fields=all 
    organisms <- tbl_df(read.delim('../inst/extdata/eupathdb_organisms_20151206.txt',
                                   stringsAsFactors=FALSE))

    #organisms <- read.delim(system.file('extdata','eupathdb_organisms_20151206.txt',
    #                           package='EuPathDB'))

    # Fix column names
    colnames(organisms) <- gsub('\\.+', '_', sub('\\.+$', '', sub('X.', '', colnames(organisms))))

    # Get organism base URL
    organisms$sourceUrl <- as.character(sapply(organisms$Fasta_Download_Link, function(x) {
        paste(unlist(strsplit(x, '/'))[1:7], collapse='/')
    }))

    # Fix NCBI taxon ids
    organisms$NCBI_taxon_ID[organisms$NCBI_taxon_ID == 'null'] <- NA

    # RData paths
    sqlite_filenames <- paste0(gsub(" ", "_", organisms$Organism_1), 
                             "_", organisms$Project, version, ".sqlite")
    organisms$rDataPath <- file.path(sprintf("EuPathDB%s", version),
                                    organisms$Project,
                                    sqlite_filenames)

    organisms <- organisms %>% 
        mutate(
            provider=Project,
            species=Organism_1,
            gff=GFF_Download_Link,
            fasta=Fasta_Download_Link,
            gene_details=Gene_Details_Download_Link,
            taxonomyId=as.integer(NCBI_taxon_ID),
            genome=sprintf("%s %s", Project, version),
            sourceVersion=sprintf("%s %s", Project, version),
            description=sprintf("%s %s annotations for %s", Project, version, Organism_1)
        ) %>% 
        select(provider, species, gff, fasta, gene_details, taxonomyId, genome,
               sourceUrl, sourceVersion, description, rDataPath)

    as.data.frame(organisms)
}

# Create EuPathDB AnnotationHubMetadata objects
create_eupathdb_ahms <- function(currentMetadata, justRunUnitTest, BiocVersion) {
    # Base URL for current version of EuPathDB
    #base_url <- 'http://eupathdb.org/common/downloads/Current_Release/'

    # Current version
    # Starting EuPathDB 25, version numbers have been normalized across
    # all databases, so version for any of the databases can be used.
    version <- get_eupathdb_version('http://tritrypdb.org/')

    ## Make list of metadata in a helper function
    meta <- .load_eupathdb_organisms(version)

    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        Description=meta$description,
        DataProvider=meta$provider,
        Genome=meta$genome,
        # Note: For now, just pointing to GFF file. Eventually this should be
        # generalized to deal with multiple input file types
        #SourceUrl=meta$sourceUrl,
        SourceUrl=meta$gff,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=basename(meta$rDataPath),
        RDataPath=meta$rDataPath,
        MoreArgs=list(
            BiocVersion=BiocVersion,
            SourceType="GFF",
            Coordinate_1_based=TRUE,
            Maintainer="Keith Hughitt <khughitt@umd.edu>",
            RDataClass="OrganismDb",
            DispatchClass="SQLiteFile",
            RDataDateAdded=Sys.time(),
            Recipe="EuPathDB:::eupathdb_recipe",
            Tags=c("Annotation", "EuPathDB", "Eukaryote", "Pathogen",
                   "Parasite", "Amoeba", "Cryptosporidium", "Fungi", "Giardia",
                   "Microsporidia", "Piroplasma", "Plasmodium", "Toxoplasma",
                   "Trichomonas", "Leishmania", "Trypanosoma", "Kinetoplastid")
        ))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
EuPathDBGFFtoOrgDb <- function(ahm) {
    # - Question: Should AnnotationForge MarkOrgXX functions be used? Or should custom
    #   functions be created?

}

## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("EuPathDBImportPreparer", create_eupathdb_ahms)

