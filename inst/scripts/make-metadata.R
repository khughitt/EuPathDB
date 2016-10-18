#!/usr/bin/env Rscript
#
# EuPathDB metadata[i,] generation script
#
library('jsonlite')
library('dplyr')
library('httr')

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
tag_strings <- lapply(tags, function(x) { paste(x, collapse=',') })

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

# shared metadata
shared_metadata <- dat %>% transmute(
    BiocVersion='3.4',
    Genome=sub('.gff', '', basename(URLgff)),
    NumGenes=genecount,
    NumOrthologs=orthologcount,
    SourceType='GFF',
    SourceUrl=URLgff,
    SourceVersion=dbversion,
    Species=organism,
    TaxonomyId=ncbi_tax_id,
    Coordinate_1_based=TRUE,
    DataProvider=project_id,
    Maintainer='Keith Hughitt <khughitt@umd.edu>'
)

# Add project-specific tags for each entry
shared_metadata$Tags <- sapply(shared_metadata$DataProvider, 
                               function(x) { tag_strings[[x]] })

# replace missing taxonomy ids with NAs
shared_metadata$TaxonomyId[shared_metadata$TaxonomyId == ''] <- NA

# overide missing taxonomy ids for strains where it can be assigned; ideally
# OrgDb and GRanges objects should not depend on taxonomy id information since
# this precludes the inclusion of a lot of prokaryotic resources.
known_taxon_ids <- data.frame(
    species=c('Ordospora colligata OC4', 
              'Trypanosoma cruzi CL Brener Esmeraldo-like',
              'Trypanosoma cruzi CL Brener Non-Esmeraldo-like'),
    taxonomy_id=c('1354746', '353153', '353153')
)

taxon_mask <- shared_metadata$Species %in% known_taxon_ids$species
ind <- match(shared_metadata[taxon_mask,'Species'], known_taxon_ids$species)
shared_metadata[taxon_mask,]$TaxonomyId <- as.character(known_taxon_ids$taxonomy_id[ind])

# exclude remaining species which are missing taxonomy information from
# metadata; cannot construct GRanges/OrgDb instances for them since they are
# have no known taxonomy id, and are not in available.species()
na_ind <- is.na(shared_metadata$TaxonomyId)
message(sprintf("- Excluding %d organisms for which no taxonomy id could be assigned (%d remaining)",
                sum(na_ind), sum(!na_ind)))
shared_metadata <- shared_metadata[!na_ind,]

# convert remaining taxonomy ids to numeric
shared_metadata$TaxonomyId <- as.numeric(shared_metadata$TaxonomyId)

# remove any organisms for which no GFF is available
gff_exists <- sapply(shared_metadata$SourceUrl, function(url) { HEAD(url)$status_code == 200 })

message(sprintf("- Excluding %d organisms for which no GFF file is available (%d remaining)",
        sum(!gff_exists), sum(gff_exists)))
shared_metadata <- shared_metadata[gff_exists,]

# generate separate metadata table for OrgDB and GRanges targets
granges_metadata <- shared_metadata %>% mutate(
    Title=sprintf('Transcript information for %s', Species),
    Description=sprintf('%s %s transcript information for %s', DataProvider, SourceVersion, Species),
    RDataClass='GRanges',
    DispatchClass='GRanges',
    ResourceName=sprintf('GRanges.%s.%s%s.rda', gsub('[ /.]+', '_', Species), 
                         tolower(DataProvider), SourceVersion, 'rda')
)

orgdb_metadata <- shared_metadata %>% mutate(
    Title=sprintf('Genome wide annotations for %s', Species),
    Description=sprintf('%s %s annotations for %s', DataProvider, SourceVersion, Species),
    RDataClass='OrgDb',
    DispatchClass='SQLiteFile',
    ResourceName=sprintf('org.%s.%s.db.sqlite', gsub('[ /.]+', '_', Species), 
                         tolower(substring(DataProvider, 1, nchar(DataProvider) - 2)))
)

# save to file
write.csv(granges_metadata, row.names=FALSE, quote=TRUE, file='../extdata/granges_metadata.csv')
write.csv(orgdb_metadata, row.names=FALSE, quote=TRUE, file='../extdata/orgdb_metadata.csv')

