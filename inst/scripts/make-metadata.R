#!/usr/bin/env Rscript
#
# EuPathDB metadata[i,] generation script
#
library('ExperimentHubData')
library('AnnotationHubData')
library('jsonlite')
library('dplyr')

# Get EuPathDB version (same for all databases)
dbversion <- readLines('http://tritrypdb.org/common/downloads/Current_Release/Build_number')
message('===========================================')
message(sprintf('EuPathDB version: %s', dbversion))
message('===========================================')

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

# construct API request URL
base_url <- 'http://eupathdb.org/eupathdb/webservices/'
query_string <- 'OrganismQuestions/GenomeDataTypes.json?o-fields=all' 
request_url <- paste0(base_url, query_string)

# retrieve organism metadata from EuPathDB
message(sprintf("- Querying %s", request_url))
result <- fromJSON(request_url)
records <- result$response$recordset$records

# convert to a dataframe
dat <- data.frame(t(sapply(records$fields, function (x) x[,'value'])), 
                  stringsAsFactors=FALSE)
colnames(dat) <- records$fields[[1]]$name 

message(sprintf("- Found metadata for %d organisms", nrow(dat)))

# reformat to match expectations
# NOTE: there are currently two hard-coded fields which contain "LmjF" (L.
# major Friedlin); these are placeholders for generic text once a mapping from
# species names to short identifiers.
metadata <- dat %>% transmute(
    Title=sprintf('Genome annotations for %s', organism),
    Description=sprintf('%s %s annotations for %s', project_id, dbversion, organism),
    BiocVersion='3.4',
    Genome=sprintf('LmjF%s', dbversion),
    SourceType='GFF',
    SourceUrl=URLgff,
    SourceVersion=dbversion,
    Species=organism,
    TaxonomyId=ncbi_tax_id,
    Coordinate_1_based=TRUE,
    DataProvider=project_id,
    Maintainer='Keith Hughitt <khughitt@umd.edu>',
    RDataClass='TxDb',
    DispatchClass='Rda',
    ResourceName='LmjF_TxDb.rda'
)

# replace missing taxonomy ids with NAs
metadata$TaxonomyId[metadata$TaxonomyId == ''] <- NA

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
    Tags=tags[dat$project_id],
    TaxonomyId=metadata$TaxonomyId,
    Title=basename(metadata$SourceUrl),
    RDataPath="tmp/path/to/file.rda",
    MoreArgs=list(
        BiocVersion=BiocInstaller::biocVersion(),
        SourceType='GFF',
        Coordinate_1_based=TRUE,
        Maintainer='Keith Hughitt <khughitt@umd.edu>',
        RDataClass='TxDb',
        DispatchClass='Rda',
        RDataDateAdded=Sys.time(),
        Recipe=NA_character_
    ))
