#!/usr/bin/env Rscript
#
# EuPathDB metadata generation script
#
library('ExperimentHubData')
library('jsonlite')
library('dplyr')

# Database URLs
eupathdb_db_urls <- list(
    "AmoebaDB"="http://amoebadb.org/amoebadb/",
    "CryptoDB"="http://cryptodb.org/cryptodb/",
    "FungiDB"="http://fungidb.org/fungidb/",
    "GiardiaDB"="http://giardiadb.org/giardiadb/",
    "MicrosporidiaDB"="http://microsporidiadb.org/microsporidiadb/",
    "PiroplasmaDB"="http://piroplasmadb.org/piroplasmadb/",
    "PlasmoDB"="http://plasmodb.org/plasmodb/",
    "ToxoDB"="http://toxodb.org/toxodb/",
    "TrichDB"="http://trichdb.org/trichdb/",
    "TriTrypDB"="http://tritrypdb.org/tritrypdb/"
)

# AnnotationHub tags
shared_tags <- c("Annotation", "EuPathDB", "Eukaryote", "Pathogen", "Parasite")

tags <- list(
    "AmoebaDB"=c(shared_tags, 'Amoeba'),
    "CryptoDB"=c(shared_tags, 'Cryptosporidium'),
    "FungiDB"=c(shared_tags, 'Fungus', 'Fungi'),
    "GiardiaDB"=c(shared_tags, 'Giardia'),
    "MicrosporidiaDB"=c(shared_tags, 'Microsporidia'),
    "PiroplasmaDB"=c(shared_tags, 'Piroplasma'),
    "PlasmoDB"=c(shared_tags, 'Plasmodium'),
    "ToxoDB"=c(shared_tags, 'Toxoplasmosis'),
    "TrichDB"=c(shared_tags, 'Trichomonas'),
    "TriTrypDB"=c(shared_tags, 'Trypanosome', 'Kinetoplastid', 'Leishmania')
)

# 2016/08/20 - for now, we will just load resources from TriTrypDB; eventually
# this will be extended to all other EuPathDB databases
api_request <- 'webservices/OrganismQuestions/GenomeDataTypes.json?o-fields=all' 
query_url <- paste0(eupathdb_db_urls[['TriTrypDB']], api_request)

# EuPathDB version (same for all databases)
dbversion <- readLines('http://tritrypdb.org/common/downloads/Current_Release/Build_number')

# retrieve organism metadata from EuPathDB
result <- fromJSON(query_url)
records <- result$response$recordset$records

# convert to a dataframe
dat <- data.frame(t(sapply(records$fields, function (x) x[,'value'])), 
                  stringsAsFactors=FALSE)
colnames(dat) <- records$fields[[1]]$name 

# reformat to match expectations
# NOTE: there are currently two hard-coded fields which contain "Lmjf" (L.
# major Friedlin); these are placeholders for generic text once a mapping from
# species names to short identifiers.
metadata <- dat %>% transmute(
    Title=sprintf('Genome annotations for %s', primary_key),
    Description=sprintf('%s %s annotations for %s', project_id, dbversion, primary_key),
    BiocVersion='3.3',
    Genome=sprintf('LmjF%s', dbversion),
    SourceType='GFF',
    SourceUrl=URLgff,
    SourceVersion=dbversion,
    Species=primary_key,
    TaxonomyId=ncbi_tax_id,
    Coordinate_1_based=TRUE,
    DataProvider=project_id,
    Maintainer='Keith Hughitt <khughitt@umd.edu>',
    RDataClass='TxDb',
    DispatchClass='Rda',
    ResourceName='LmjF_TxDb.rda'
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
        SourceType='GFF',
        Coordinate_1_based=TRUE,
        Maintainer='Keith Hughitt <khughitt@umd.edu>',
        RDataClass='TxDb',
        DispatchClass='Rda',
        RDataDateAdded=Sys.time(),
        Recipe=NA_character_,
        Tags=tags[['TriTrypDB']]
    ))
