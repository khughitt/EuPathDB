#!/usr/bin/env Rscript
#
# EuPathDB metadata generation script
#
library('ExperimentHubData')
library('jsonlite')
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

# 2016/08/20 - for now, we will just load resources from TriTrypDB; eventually
# this will be extended to all other EuPathDB databases
query_url <- 'http://tritrypdb.org/tritrypdb/webservices/OrganismQuestions/GenomeDataTypes.json?o-fields=all'

# EuPathDB version (same for all databases)
dbversion <- readLines('http://tritrypdb.org/common/downloads/Current_Release/Build_number')

# retrieve organism metadata from EuPathDB
result <- fromJSON(query_url)
records <- result$response$recordset$records

# convert to a dataframe
dat <- data.frame(t(sapply(records$fields, function (x) x[,'value'])), stringsAsFactors=FALSE)
colnames(dat) <- records$fields[[1]]$name 

metadata <- dat %>% transmute(
    Title=sprintf('Genome annotations for %s', primary_key),
    Description=sprintf('%s %s annotations for %s', project_id, dbversion, primary_key),
    BiocVersion='3.3',
    Genome=sprintf('LmjF%s', dbversion), # TODO: generalize for all organisms
    SourceType='GFF',
    SourceUrl=URLgff,
    SourceVersion=dbversion,
    Species=primary_key,
    TaxonomyId=ncbi_tax_id,
    Coordinate_1_based=TRUE,
    DataProvider=project_id,
    Maintainer='Keith Hughitt <khughitt@umd.edu>',
    RDataClass='OrgDb',
    DispatchClass='Rda',
    ResourceName='LmjF_OrgDb.rda'
)

# save to file
write.csv(metadata, row.names=FALSE, quote=FALSE, file='../extdata/metadata.csv')

# Create EuPathDB AnnotationHubMetadata objects
Map(AnnotationHubMetadata,
    Description=metadata$Description,
    DataProvider=metadata$DataProvider,
    Genome=metadata$Genome,
    SourceUrl=metadata$SourceUrl,
    SourceVersion=metadata$SourceVersion,
    Species=metadata$Species,
    TaxonomyId=metadata$TaxonomyId,
    Title=basename(metadata$SourceUrl),
    RDataPath="tmp/path/to/file.rda",
    MoreArgs=list(
        BiocVersion=biocVersion(),
        SourceType=metadata$SourceType,
        Coordinate_1_based=metadata$Coordinate_1_based,
        Maintainer=metadata$Maintainer,
        RDataClass=metadata$RDataClass,
        DispatchClass=metadata$DispatchClass,
        RDataDateAdded=Sys.time(),
        Recipe=NA_character_,
        Tags=c("Annotation", "EuPathDB", "Eukaryote", "Pathogen",
               "Parasite", "Amoeba", "Cryptosporidium", "Fungi", "Giardia",
               "Microsporidia", "Piroplasma", "Plasmodium", "Toxoplasma",
               "Trichomonas", "Leishmania", "Trypanosoma", "Kinetoplastid")
    ))
