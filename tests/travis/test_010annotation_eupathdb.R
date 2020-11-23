start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("010annotation_eupathdb.R
	  12\n")
## 2018-02: Hey, there is a new eupathdb release!  Some stuff has changed!
## 2017-12, exported functions in annotation_eupathdb:
##  make_eupath_bsgenome(), make_eupath_organismdbi() download_eupath_metadata(),
##  make_eupath_orgdb(), make_eupath_txdb(), get_eupath_gff_table(),
##  get_eupath_gene_types(), get_eupath_go_term_table(), get_eupath_pathway_table(),
##  get_eupath_interpro_table(), get_eupath_ortholog_table() get_eupath_text()
##  Most of these are implicitly tested via make_eupath_organismdbi().

testing <- download_eupath_metadata(webservice="tritrypdb")
expected <- 60
actual <- nrow(testing[["valid"]])
## 01
test_that("Is the eupathdb valid metadata the expected size?", {
  expect_gt(expected, actual)
})

lmajor_entry <- get_eupath_entry(species="Friedlin", metadata=testing)
expected <- "Leishmania major MHOM/IL/81/Friedlin"
actual <- lmajor_entry[["TaxonomyName"]]
test_that("Do we get an lmajor entry?", {
  expect_equal(expected, actual)
})

lmajor_orgdb <- make_eupath_orgdb(entry=lmajor_entry, install=FALSE, overwrite=TRUE)
pkg <- AnnotationDbi::loadDb(lmajor_orgdb[["db_path"]])
annot_df <- load_orgdb_annotations(orgdb=pkg)[["genes"]]
expected <- 8
actual <- ncol(annot_df)
test_that("Do we get the expected 8 columns from the lmajor orgdb?", {
  expect_equal(expected, actual)
})

expected <- 9400
actual <- nrow(annot_df)
test_that("Do we get the expected rows from the lmajor orgdb?", {
  expect_gt(actual, expected)
})

lmajor_txdb <- make_eupath_txdb(entry=lmajor_entry, install=FALSE)
lmajor_txdb <- lmajor_txdb[["object"]]
test_that("Do we get some txdb information?", {
  expect_equal("TxDb", class(lmajor_txdb[1]) }
})

lmajor_granges <- make_eupath_granges(entry=lmajor_entry)
load(file=lmajor_granges[["rda"]])
granges_data <- get0(lmajor_granges$variable)
granges_df <- as.data.frame(granges_data)
granges_col <- ncol(granges_df)
granges_row <- nrow(granges_df)
test_that("Do we get some granges information?", {
  expect_equal(15, granges_col)
  expect_gt(granges_row, 37300)
})

lmajor_bsgenome <- make_eupath_bsgenome(entry=lmajor_entry, install=FALSE)


end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
