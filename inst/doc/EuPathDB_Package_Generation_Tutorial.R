## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----01example_install---------------------------------------------------
library(EuPathDB)

## Ask for the version 46 data from fungidb for species with 'cerevisiae' in the name.
sc_entry <- get_eupath_entry(species = "cerevisiae", webservice = "fungidb", eu_version = "v46")
sc_name <- sc_entry[["Species"]]
sc_entry

## ----02pkg---------------------------------------------------------------
orgdb_pkg <- make_eupath_orgdb(sc_entry)
txdb_pkg <- make_eupath_txdb(sc_entry)
bsgenome_pkg <- make_eupath_bsgenome(sc_entry)
organ_pkg <- make_eupath_organismdbi(sc_entry)

## ----04extract-----------------------------------------------------------
orgdb_pkg <- get_eupath_pkgnames(sc_entry)
sc_orgdb <- orgdb_pkg$orgdb
## Here is the name of the current yeast package.
sc_orgdb

## We can set the version parameter to change this if we have a previous version installed.

## Now get the set of available columns from it:
library(sc_orgdb, character = TRUE)
pkg <- get0(sc_orgdb)
avail_columns <- AnnotationDbi::columns(pkg)
head(avail_columns)
## There are lots of columns!
length(avail_columns)

## ----05extract_data------------------------------------------------------
## The columns which begin with strings like 'PATHWAY' or 'INTERPRO' are actually separate
## sql tables in the orgdb database, and as such will lead to a hugely redundant data table
## if we select them.
chosen_columns_idx <- grepl(x = avail_columns, pattern = "^ANNOT")
chosen_columns <- avail_columns[chosen_columns_idx]

## Now we have a set of columns of interest, let us get a data table/data frame.
sc_annot <- load_orgdb_annotations(orgdb = sc_orgdb, keytype = "gid", fields = chosen_columns)
## load_orgdb_annotations will fill out separate dataframes for each annotation type,
## genes, exons, transcripts, etc.  In this case, we only want the genes
## (The eupathdb does not provide much information for the others.)
sc_genes <- sc_annot[["genes"]]
dim(sc_genes)
head(sc_genes)
## Yay! We have data about S. cerevisiae!

chosen_columns_idx <- grepl(x = avail_columns, pattern = "^GO")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_go <- load_orgdb_go(sc_orgdb, columns = chosen_columns)
head(sc_go)
## Yay Gene ontology data for Crithidia!

chosen_columns_idx <- grepl(x = avail_columns, pattern = "^INTERPRO")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_interpro <- load_orgdb_go(sc_orgdb, columns = chosen_columns)
head(sc_interpro)
## Interpro data for Crithidia!

chosen_columns_idx <- grepl(x = avail_columns, pattern = "^PATHWAY")
chosen_columns <- avail_columns[chosen_columns_idx]
sc_path <- load_orgdb_go(sc_orgdb, columns = chosen_columns)
head(sc_path)

## ----06shortcuts---------------------------------------------------------
## The function load_eupath_annotations() provides a shortcut to the above.
sc_annot <- load_eupath_annotations(species = "S288c", eu_version = "v46", webservice = "fungidb")
dim(sc_annot)

## ----07orthologs---------------------------------------------------------
sc_ortho <- extract_eupath_orthologs(sc_orgdb)
dim(sc_ortho)
head(sc_ortho)
summary(sc_ortho)

## ----08sessioninfo-------------------------------------------------------
sessionInfo()

