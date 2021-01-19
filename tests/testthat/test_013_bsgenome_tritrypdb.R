start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("013_bsgenome_tritrypdb
	  12\n")

webservice <- "tritrypdb"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
wanted <- "Friedlin"

entry <- get_eupath_entry(species=wanted, metadata=testing)
bsgenome <- make_eupath_bsgenome(entry=entry, install=FALSE)
expected <- "BSGenome.Leishmania.major.Friedlin.v49"
actual <- as.character(bsgenome[["bsgenome_name"]])
test_that("Did we get some bsgenome information?", {
  expect_equal(expected, actual)
}

wanted <- "panamensis"
entry <- get_eupath_entry(species=wanted, metadata=testing)
bsgenome <- make_eupath_bsgenome(entry=entry, install=FALSE)
expected <- "BSGenome.Leishmania.panamensis.MHOMCOL81L13.v49"
actual <- as.character(bsgenome[["bsgenome_name"]])
test_that("Did we get some bsgenome information?", {
  expect_equal(expected, actual)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
