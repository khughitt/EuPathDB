## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----eval = FALSE--------------------------------------------------------
#  install.packages("BiocManager")
#  BiocManager::install("AnnotationHub")

## ------------------------------------------------------------------------
library("EuPathDB")
library('AnnotationHub')

# create an AnnotationHub connection
ah <- AnnotationHub()

# search for all EuPathDB resources
meta <- query(ah, "EuPathDB")

length(meta)
head(meta)

# types of EuPathDB data available
table(meta$rdataclass)

# distribution of resources by specific databases
table(meta$dataprovider)

# list of organisms for which resources are available
length(unique(meta$species))
head(unique(meta$species))

## ------------------------------------------------------------------------
res <- query(ah, c('Leishmania major strain Friedlin', 'OrgDb', 'EuPathDB'))
res

## ------------------------------------------------------------------------
orgdb <- res[['AH65089']]
class(orgdb)

## ------------------------------------------------------------------------
# list available fields to retrieve
columns(orgdb)

# create a vector containing all gene ids for the organism
gids <- keys(orgdb, keytype='GID')
head(gids)

# retrieve the chromosome, description, and biotype for each gene
dat <- select(orgdb, keys=gids, keytype='GID', columns=c('CHR', 'TYPE', 'GENEDESCRIPTION'))

head(dat)

table(dat$TYPE)
table(dat$CHR)

# create a gene / GO term mapping
gene_go_mapping <- select(orgdb, keys=gids, keytype='GID', columns=c('GO_ID', 'GO_TERM_NAME', 'ONTOLOGY'))
head(gene_go_mapping)

# retrieve KEGG, etc. pathway annotations
gene_pathway_mapping <- select(orgdb, keys=gids, keytype='GID', columns=c('PATHWAY', 'PATHWAY_SOURCE'))
table(gene_pathway_mapping$PATHWAY_SOURCE)
head(gene_pathway_mapping)

## ------------------------------------------------------------------------
# query AnnotationHub
res <- query(ah, c('Leishmania major strain Friedlin', 'GRanges', 'EuPathDB'))
res

# retrieve a GRanges instance associated with the result record
gr <- res[['AH65354']]
gr

## ------------------------------------------------------------------------
# chromosome names
seqnames(gr)

# strand information
strand(gr)

# feature widths
head(width(gr))

## ------------------------------------------------------------------------
# list of location types in the resource
table(gr$type)

## ------------------------------------------------------------------------
# get the first three ranges
gr[1:3]

# get all gene entries on chromosome 4
gr[gr$type == 'gene' & seqnames(gr) == 'LmjF.04']

## ----example_install-----------------------------------------------------
## Note that some but not all web services have moved to https...
tri_meta <- download_eupath_metadata(webservice="tritrypdb")

crit_name <- check_eupath_species(species="Crith", metadata=tri_meta)
crit_name

## ----pkg, eval=FALSE-----------------------------------------------------
#  orgdb_pkg <- make_eupath_orgdb(species=crit_name[["Species"]], entry=crit_name, reinstall=TRUE)
#  txdb_pkg <- make_eupath_txdb(species=crit_name[["Species"]], entry=crit_name)
#  bsgenome_pkg <- make_eupath_bsgenome(species=crit_name[["Species"]], entry=crit_name)
#  organ_pkg <- make_eupath_organismdbi(species=crit_name[["Species"]], entry=crit_name)

## ----extract-------------------------------------------------------------
orgdb_pkg <- get_eupath_pkgnames(crit_name[["Species"]])[["orgdb"]]
## Get the name of the Crithidia package that got created by make_eupath_orgdb.
orgdb_pkg
## Thus we see the v41 (as of late 2018), a number which presumably will continue increasing.
## We can set the version parameter to change this if we have a previous version installed.

## Now get the set of available columns from it:
library(orgdb_pkg, character=TRUE)
avail_columns <- AnnotationDbi::columns(get0(orgdb_pkg))
avail_columns

## ----extract_data--------------------------------------------------------
## The columns which begin with strings like 'PATHWAY' or 'INTERPRO' are actually separate
## sql tables in the orgdb database, and as such will lead to a hugely redundant data table
## if we select them.

chosen_columns_idx <- grepl(x=avail_columns, pattern="^ANNOT")
chosen_columns <- avail_columns[chosen_columns_idx]

## Now we have a set of columns of interest, let us get a data table/data frame.
crit_annot <- load_orgdb_annotations(orgdb=orgdb_pkg, keytype="gid", fields=chosen_columns)
## load_orgdb_annotations will fill out separate dataframes for each annotation type,
## genes, exons, transcripts, etc.  In this case, we only want the genes
## (The eupathdb does not provide much information for the others.)
crit_genes <- crit_annot[["genes"]]
dim(crit_genes)
head(crit_genes)
## Yay! We have data about Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^GO")
chosen_columns <- avail_columns[chosen_columns_idx]
crit_go <- load_orgdb_go(orgdb_pkg, keytype="gid", columns=chosen_columns)
head(crit_go, n=1)
## Yay Gene ontology data for Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^INTERPRO")
chosen_columns <- avail_columns[chosen_columns_idx]
crit_interpro <- load_orgdb_go(orgdb_pkg, keytype="gid", columns=chosen_columns)
head(crit_interpro)
## Interpro data for Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^PATHWAY")
chosen_columns <- avail_columns[chosen_columns_idx]
crit_path <- load_orgdb_go(orgdb_pkg, keytype="gid", columns=chosen_columns)
head(crit_path)
## Pathway data for Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^ORTHOLOG")
chosen_columns <- avail_columns[chosen_columns_idx]
chosen_columns <- c(chosen_columns, "ORGANISM")
crit_ortho <- load_orgdb_go(orgdb_pkg, keytype="gid", columns=chosen_columns)
head(crit_ortho)
## Orthologs!

## ----shortcuts-----------------------------------------------------------
## The function load_eupath_annotations() provides a shortcut to the above.
crit_annotv2 <- load_eupath_annotations(species="Crith")
dim(crit_annotv2)
## This provides the same information as the results of the select up above.

## ----orthologs-----------------------------------------------------------
crit_ortho <- extract_eupath_orthologs("Crith")
dim(crit_ortho)
head(crit_ortho)
summary(crit_ortho)

## ------------------------------------------------------------------------
sessionInfo()

