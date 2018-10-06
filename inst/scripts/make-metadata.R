#!/usr/bin/env Rscript
#
# EuPathDB AnnotationHub metadata generation script
#
library(jsonlite)
library(dplyr)
library(httr)
library(GenomeInfoDbData)

# Get EuPathDB version (same for all databases)
dbversion <- readLines('http://tritrypdb.org/common/downloads/Current_Release/Build_number')

message('===========================================')
message(sprintf('EuPathDB version: %s', dbversion))
message('===========================================')

# AnnotationHub tags
shared_tags <- c("Annotation", "EuPathDB", "Eukaryote", "Pathogen", "Parasite") 

tags <- list(
    "AmoebaDB" = c(shared_tags, 'Amoeba'),
    "CryptoDB" = c(shared_tags, 'Cryptosporidium'),
    "FungiDB" = c(shared_tags, 'Fungus', 'Fungi'),
    "GiardiaDB" = c(shared_tags, 'Giardia'),
    "MicrosporidiaDB" = c(shared_tags, 'Microsporidia'),
    "PiroplasmaDB" = c(shared_tags, 'Piroplasma'),
    "PlasmoDB" = c(shared_tags, 'Plasmodium'),
    "ToxoDB" = c(shared_tags, 'Toxoplasmosis'),
    "TrichDB" = c(shared_tags, 'Trichomonas'),
    "TriTrypDB" = c(shared_tags, 'Trypanosome', 'Kinetoplastid', 'Leishmania')
)
tag_strings <- lapply(tags, function(x) { paste(x, collapse = ',') })

# construct API request URL
base_url <- 'https://eupathdb.org/eupathdb/webservices/'
query_string <- 'OrganismQuestions/GenomeDataTypes.json?o-fields=all' 
request_url <- paste0(base_url, query_string)

# retrieve organism metadata from EuPathDB
message(sprintf("- Querying %s", request_url))
result <- fromJSON(request_url)
records <- result$response$recordset$records

# convert to a dataframe
dat <- data.frame(t(sapply(records$fields, function (x) x[, 'value'])), 
                  stringsAsFactors = FALSE)
colnames(dat) <- records$fields[[1]]$name 

message(sprintf("- Found metadata for %d organisms", nrow(dat)))

# shared metadata
shared_metadata <- dat %>% transmute(
    BiocVersion = as.character(BiocManager::version()),
    Genome = sub('.gff', '', basename(URLgff)),
    GenomeSizeMB = as.numeric(trimws(megabps)),
    NumGenes = genecount,
    NumOrthologs = orthologcount,
    SourceType = 'GFF',
    SourceUrl = URLgff,
    SourceVersion = dbversion,
    Species = species,
    Organism = organism,
    Strain = trimws(strain),
    TaxonomyId = ncbi_tax_id,
    Coordinate_1_based = TRUE,
    DataProvider = project_id,
    Maintainer = 'Keith Hughitt <keith.hughitt@nih.gov>'
)

# Add project-specific tags for each entry
shared_metadata$Tags <- sapply(shared_metadata$DataProvider, function(x) { tag_strings[[x]] })

# load species taxonomy mapping
data(specData)
specData$genus_species <- sprintf("%s %s", specData$genus, specData$species)

# Note (2018-10-06), there are currently a few EuPathDB entries where "Species" includes
# some additional strain information where it is not expected.. to be safe, we will manually
# parse out genus / species for the Organism field.
shared_metadata$Species <- unlist(lapply(lapply(strsplit(shared_metadata$Organism, ' '), '[', 1:2), paste, collapse = ' '))

# entries missing taxonomy information
missing_tax_inds <- is.na(shared_metadata$TaxonomyId)

# for strains / isolates with no assigned taxonomy id, use their species-level taxonomy id
missing_taxon_ids <- shared_metadata$Species[missing_tax_inds]

# EuPathDB uses "sp" whereas mapping uses "sp."
missing_taxon_ids <- gsub(' sp$', ' sp.', missing_taxon_ids)

matched_tax_ids <- specData$tax_id[match(missing_taxon_ids, specData$genus_species)]

# manually fix problematic mapping entries, where possible
matched_tax_ids[missing_taxon_ids == 'Candida auris'] <- specData$tax_id[match('[Candida] auris', specData$genus_species)]

shared_metadata$TaxonomyId[missing_tax_inds] <- matched_tax_ids

# exclude remaining entries which are missing taxonomy information from
# metadata; cannot construct GRanges/OrgDb instances for them since they are
# have no known taxonomy id, and are not in available.species()
na_ind <- is.na(shared_metadata$TaxonomyId)

message(sprintf("- Excluding %d organisms for which no taxonomy id could be assigned (%d remaining)",
                sum(na_ind), sum(!na_ind)))
shared_metadata <- shared_metadata[!na_ind, ]

# convert remaining taxonomy ids to numeric
shared_metadata$TaxonomyId <- as.numeric(shared_metadata$TaxonomyId)

# remove any organisms for which no GFF is available
gff_exists <- sapply(shared_metadata$SourceUrl, function(url) { HEAD(url)$status_code == 200 })

message(sprintf("- Excluding %d organisms for which no GFF file is available (%d remaining)",
        sum(!gff_exists), sum(gff_exists)))
shared_metadata <- shared_metadata[gff_exists, ]

# generate separate metadata table for OrgDB and GRanges targets
granges_metadata <- shared_metadata %>% mutate(
    Title = sprintf('%s transcript information', Organism),
    Description = sprintf('%s %s transcript information for %s', DataProvider, SourceVersion, Organism),
    RDataClass = 'GRanges',
    DispatchClass = 'GRanges',
    ResourceName = sprintf('GRanges.%s.%s%s.rda', gsub('[ /.]+', '_', Organism), 
                         tolower(DataProvider), SourceVersion, 'rda')
) %>% mutate(
    RDataPath = file.path('EuPathDB', 'GRanges', BiocVersion, ResourceName)
)

orgdb_metadata <- shared_metadata %>% mutate(
    Title = sprintf('%s genome wide annotations', Organism),
    Description = sprintf('%s %s genome annotations for %s', DataProvider, SourceVersion, Organism),
    RDataClass = 'OrgDb',
    DispatchClass = 'SQLiteFile',
    ResourceName = sprintf('org.%s.%s.db.sqlite', gsub('[ /.]+', '_', Organism), 
                         tolower(substring(DataProvider, 1, nchar(DataProvider) - 2)))
) %>% mutate(
    RDataPath = file.path('EuPathDB', 'OrgDb', BiocVersion, ResourceName)
)

# save to file
write.csv(granges_metadata, row.names = FALSE, quote = TRUE, file = '../extdata/granges_metadata.csv')
write.csv(orgdb_metadata, row.names = FALSE, quote = TRUE, file = '../extdata/orgdb_metadata.csv')

