## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----01example_install---------------------------------------------------
library(EuPathDB)
## Note that some but not all web services have moved to https...
## tri_meta <- download_eupath_metadata(webservice="tritrypdb")

sc_entry <- get_eupath_entry(species="cerevisiae", webservice="fungidb")
sc_name <- sc_entry[["Species"]]
sc_entry

## ----02pkg, eval=FALSE---------------------------------------------------
#  orgdb_pkg <- make_eupath_orgdb(sc_entry, reinstall=TRUE)
#  txdb_pkg <- make_eupath_txdb(sc_entry)
#  bsgenome_pkg <- make_eupath_bsgenome(sc_entry)
#  organ_pkg <- make_eupath_organismdbi(sc_entry)

## ----03lmajor, eval=FALSE------------------------------------------------
#  lm_entry <- get_eupath_entry(species="Friedlin", webservice="tritrypdb")
#  lm_orgdb <- make_eupath_orgdb(lm_entry, reinstall=TRUE)

## ----04extract-----------------------------------------------------------
orgdb_pkg <- get_eupath_pkgnames(sc_entry)
sc_orgdb <- orgdb_pkg$orgdb
## Here is the name of the current yeast package.
sc_orgdb
## Thus we see the v41 (as of late 2018), a number which presumably will continue increasing.
## We can set the version parameter to change this if we have a previous version installed.

## Now get the set of available columns from it:
library(sc_orgdb, character=TRUE)
avail_columns <- AnnotationDbi::columns(get0(sc_orgdb))
head(avail_columns)
## There are lots of columns!
length(avail_columns)

## ----05extract_data------------------------------------------------------
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

## ----06shortcuts---------------------------------------------------------
## The function load_eupath_annotations() provides a shortcut to the above.
major_annot <- load_eupath_annotations(species="major")
dim(major_annot)
## This provides the same information as the results of the select up above.

## ----07orthologs, eval=FALSE---------------------------------------------
#  ## A recent EuPathDB update makes it possible to use the 'OrthologsLite' table rather than
#  ## Orthologs, which is much faster (by like 1000x), but I am quickly realizing much more
#  ## limited in the information it returns, and only exists for a subset of the eupathdb
#  ## projects.  As a result, I might just drop its usage and force the much slower queries
#  ## to the more complete table...
#  major_entry <- get_eupath_entry(species="major", webservice="tritrypdb")
#  major_pkg <- get_eupath_pkgnames(major_entry)
#  major_orgdb <- major_pkg$orgdb
#  lm_ortho <- extract_eupath_orthologs(major_orgdb)
#  dim(lm_ortho)
#  head(lm_ortho)
#  summary(lm_ortho)

## ----08sessioninfo-------------------------------------------------------
sessionInfo()

