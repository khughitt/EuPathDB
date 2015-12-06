#!/usr/bin/env Rscript
#
# EuPath AnnotationHub Recipe
# Author: Keith Hughitt (khughitt@umd.edu)
# November 05, 2015
#
# References
# ----------
# 1. http://bioconductor.org/packages/release/bioc/vignettes/AnnotationHub/inst/doc/AnnotationHubRecipes.html
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
    organisms <- tbl_df(read.delim('../inst/extdata/eupathdb_organisms_20151206.txt',
                                   stringsAsFactors=FALSE))

    #organisms <- read.delim(system.file('extdata','eupathdb_organisms_20151206.txt',
    #                           package='EuPathDB'))

    # Fix column names
    colnames(organisms) <- sub('\\.+', '_', sub('\\.+$', '', sub('X.', '', colnames(organisms))))

    # Get organism base URL
    organisms$sourceUrl <- as.character(sapply(organisms$Fasta_Download.Link, function(x) {
        paste(unlist(strsplit(x, '/'))[1:7], collapse='/')
    }))

    # Fix NCBI taxon ids
    organisms$NCBI_taxon.ID[organisms$NCBI_taxon.ID == 'null'] <- NA

    # RData paths
    sqlite_filenames= paste0(gsub(" ", "_", organisms$Organism_1), 
                             "_", organisms$Project, version, ".sqlite")
    organisms$rDataPath <- file.path(sprintf("EuPathDB%s", version),
                                    organisms$Project,
                                    sqlite_filenames)

    organisms <- organisms %>% 
        mutate(
            species=Organism_1,
            taxonomyId=as.integer(NCBI_taxon.ID),
            genome=sprintf("%s %s", Project, version),
            sourceVersion=sprintf("%s %s", Project, version),
            description=sprintf("%s %s annotations for %s", Project, version, Organism_1)
        ) %>% 
        select(species, taxonomyId, genome, sourceUrl, sourceVersion, description, 
               rDataPath)

    as.data.frame(organisms)
}

# Create EuPathDB AnnotationHubMetadata objects
create_eupathdb_ahms <- function(currentMetadata, justRunUnitTest, BiocVersion) {
	# Base URL for current version of EuPathDB
    #base_url <- 'http://eupathdb.org/common/downloads/Current_Release/'

	# Current version
    # Starting EuPathDB 25, version numbers have been normalized across
    # all databases.
	version <- get_eupathdb_version('http://tritrypdb.org/')

    ## Make list of metadata in a helper function
    meta <- .load_eupathdb_organisms(version)

    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=basename(meta$rDataPath),
        RDataPath=meta$rDataPath,
        MoreArgs=list(
			BiocVersion=BiocVersion,
			SourceType="GFF",
			Coordinate_1_based=TRUE,
			DataProvider="EuPathDB",
			Maintainer="Keith Hughitt <khughitt@umd.edu>",
			RDataClass="EuPathDB",
			DispatchClass="SQLiteFile",
			RDataDateAdded=Sys.time(),
			Recipe="EuPathDB:::eupathdb_recipe",
			Tags=c("EuPathDB", "Pathogen", "Parasite", "Trypanosome", "Kinetoplastid", "Annotation")
		))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
eupathdb_recipe <- function(ahm){
    input_files <- metadata(ahm)$SourceFile 
    # InParanoid-specific -- need to replace...
    # https://github.com/Bioconductor-mirror/AnnotationForge/blob/master/R/makeInparanoidDbs.R
    dbname <- makeInpDb(dir=file.path(input_files,""),
                        dataDir=tempdir())
    db <- loadDb(file=dbname)
    outputPath <- file.path(metadata(ahm)$AnnotationHubRoot,
                            metadata(ahm)$RDataPath)
    saveDb(db, file=outputPath) 
    outputFile(ahm)
}

## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("EuPathDBImportPreparer", create_eupathdb_ahms)

