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

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
