## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----install_pkgs, eval=FALSE--------------------------------------------
#  install.packages("BiocManager")
#  BiocManager::install("AnnotationHub")

## ----load_ah-------------------------------------------------------------
library("EuPathDB")
library("AnnotationHub")

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

## ----query_major---------------------------------------------------------
res <- query(ah, c('Leishmania major strain Friedlin', 'OrgDb', 'EuPathDB'))
res

## ----get_major-----------------------------------------------------------
orgdb_uid <- names(res@.db_uid)
orgdb <- res[[orgdb_uid]]
class(orgdb)

## ----query_major_columns-------------------------------------------------
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
gene_go_mapping <- select(orgdb, keys=gids, keytype='GID',
                          columns=c('GO_ID', 'GO_TERM_NAME', 'ONTOLOGY'))
head(gene_go_mapping)

# retrieve KEGG, etc. pathway annotations
gene_pathway_mapping <- select(orgdb, keys=gids, keytype='GID',
                               columns=c('PATHWAY', 'PATHWAY_SOURCE'))
table(gene_pathway_mapping$PATHWAY_SOURCE)
head(gene_pathway_mapping)

## ----query_granges-------------------------------------------------------
# query AnnotationHub
res <- query(ah, c('Leishmania major strain Friedlin', 'GRanges', 'EuPathDB'))
res

# retrieve a GRanges instance associated with the result record
gr <- res[['AH65354']]
summary(gr)
head(gr)

## ----extract_granges-----------------------------------------------------
# chromosome names
seqnames(gr)

# strand information
strand(gr)

# feature widths
head(width(gr))

## ----get_types-----------------------------------------------------------
# list of location types in the resource
table(gr$type)
table(gr@strand)

## ----chr4----------------------------------------------------------------
# get the first three ranges
gr[1:3]

# get all gene entries on chromosome 4
chr4_genes <- gr[gr$type == 'gene' & seqnames(gr) == 'LmjF.04']
summary(chr4_genes)
## Hey, checkit, there are 130 genes on chromosome 4.

## ----example_install-----------------------------------------------------
## Note that some but not all web services have moved to https...
## tri_meta <- download_eupathdb_metadata(webservice="tritrypdb")

sc_entry <- get_eupathdb_entry(species="cerevisiae", webservice="fungidb")
sc_name <- sc_entry[["Species"]]
sc_entry

## ----pkg, eval=FALSE-----------------------------------------------------
#  orgdb_pkg <- make_eupathdb_orgdb(sc_entry, reinstall=TRUE)
#  txdb_pkg <- make_eupathdb_txdb(sc_entry)
#  bsgenome_pkg <- make_eupathdb_bsgenome(sc_entry)
#  organ_pkg <- make_eupathdb_organismdbi(sc_entry)

## ----lmajor, eval=FALSE--------------------------------------------------
#  lm_entry <- get_eupathdb_entry(species="Friedlin", webservice="tritrypdb")
#  lm_orgdb <- make_eupathdb_orgdb(lm_entry, reinstall=TRUE)

## ----extract-------------------------------------------------------------
orgdb_pkg <- get_eupathdb_pkgnames(sc_entry)
sc_orgdb <- orgdb_pkg$orgdb
## Here is the name of the current yeast package.
sc_orgdb

## We can set the version parameter to change this if we have a previous version installed.

## Now get the set of available columns from it:
library(sc_orgdb, character=TRUE)
avail_columns <- AnnotationDbi::columns(get0(sc_orgdb))
head(avail_columns)
## There are lots of columns!
length(avail_columns)

## ----extract_data--------------------------------------------------------
## The columns which begin with strings like 'PATHWAY' or 'INTERPRO' are actually separate
## sql tables in the orgdb database, and as such will lead to a hugely redundant data table
## if we select them.
chosen_columns_idx <- grepl(x=avail_columns, pattern="^ANNOT")
chosen_columns <- avail_columns[chosen_columns_idx]

## Now we have a set of columns of interest, let us get a data table/data frame.
sc_annot <- load_orgdb_annotations(orgdb=sc_orgdb, keytype="gid", fields=chosen_columns)
## load_orgdb_annotations will fill out separate dataframes for each annotation type,
## genes, exons, transcripts, etc.  In this case, we only want the genes
## (The eupathdb does not provide much information for the others.)
sc_genes <- sc_annot[["genes"]]
dim(sc_genes)
head(sc_genes)
## Yay! We have data about S. cerevisiae!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^GO")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_go <- load_orgdb_go(sc_orgdb, columns=chosen_columns)
head(sc_go)
## Yay Gene ontology data for Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^INTERPRO")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_interpro <- load_orgdb_go(sc_orgdb, columns=chosen_columns)
head(sc_interpro)
## Interpro data for Crithidia!

chosen_columns_idx <- grepl(x=avail_columns, pattern="^PATHWAY")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_path <- load_orgdb_go(sc_orgdb, columns=chosen_columns)
head(sc_path)
## Pathway data for Crithidia!

## This does not work for the moment because of some oddities with
## the various tables at the eupathdb.  I have an email query with them
## regarding it.
##chosen_columns_idx <- grepl(x=avail_columns, pattern="^ORTHOLOG")
##chosen_columns <- avail_columns[chosen_columns_idx]
##chosen_columns <- c(chosen_columns, "ORGANISM")
##crit_ortho <- load_orgdb_go(sc_orgdb, columns=chosen_columns)
##head(crit_ortho)
## Orthologs!

## ----shortcuts-----------------------------------------------------------
## The function load_eupathdb_annotations() provides a shortcut to the above.
major_annot <- load_eupathdb_annotations(species="major")
dim(major_annot)
## This provides the same information as the results of the select up above.

## ----orthologs, eval=FALSE-----------------------------------------------
#  ## A recent EuPathDB update makes it possible to use the 'OrthologsLite' table rather than
#  ## Orthologs, which is much faster (by like 1000x), but I am quickly realizing much more
#  ## limited in the information it returns, and only exists for a subset of the eupathdb
#  ## projects.  As a result, I might just drop its usage and force the much slower queries
#  ## to the more complete table...
#  major_entry <- get_eupathdb_entry(species="major", webservice="tritrypdb")
#  major_pkg <- get_eupathdb_pkgnames(major_entry)
#  major_orgdb <- major_pkg$orgdb
#  lm_ortho <- extract_eupathdb_orthologs(major_orgdb)
#  dim(lm_ortho)
#  head(lm_ortho)
#  summary(lm_ortho)

## ------------------------------------------------------------------------
sessionInfo()

