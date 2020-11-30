start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("011_txdb_tritrypdb
	  12\n")

webservice <- "tritrypdb"
wanted <- "Friedlin"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
entry <- get_eupath_entry(species=wanted, metadata=testing)

txdb <- make_eupath_txdb(entry=entry, install=FALSE)
actual <- as.character(txdb[["txdb_name"]])
expected <- "TxDb.Leishmania.major.Friedlin.TriTrypDB.v49"
test_that("Do we get some txdb information?", {
  expect_equal(expected, actual)
})

wanted <- "panamensis"
entry <- get_eupath_entry(species=wanted, metadata=testing)
txdb <- make_eupath_txdb(entry=entry, install=FALSE)
actual <- as.character(txdb[["txdb_name"]])
expected <- "TxDb.Leishmania.panamensis.MHOMCOL81L13.TriTrypDB.v49"
test_that("Do we get some txdb information?", {
  expect_equal(actual, expected)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
