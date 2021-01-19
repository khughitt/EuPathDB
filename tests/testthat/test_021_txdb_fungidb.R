start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("011_txdb_fungidb
	  12\n")

webservice <- "fungidb"
wanted <- "pombe"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
entry <- get_eupath_entry(species=wanted, metadata=testing)

txdb <- make_eupath_txdb(entry=entry, install=FALSE)
actual <- as.character(txdb[["txdb_name"]])
expected <- "TxDb.Schizosaccharomyces.pombe.972h.FungiDB.v49"
test_that("Do we get some txdb information?", {
  expect_equal(expected, actual)
})

wanted <- "versicolor"
entry <- get_eupath_entry(species=wanted, metadata=testing)
txdb <- make_eupath_txdb(entry=entry, install=FALSE)
actual <- as.character(txdb[["txdb_name"]])
expected <-  "TxDb.Aspergillus.versicolor.CBS.583.65.FungiDB.v49"
test_that("Do we get some txdb information?", {
  expect_equal(actual, expected)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
