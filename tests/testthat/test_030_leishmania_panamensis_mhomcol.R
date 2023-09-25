start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("030_leishmania_panamensis_mhomcol")

## These tests are intended to ensure that the creation of new orgdbs are sane.
## Since I am primarily interested in the tritrypdb, I will focus there.

webservice <- "tritrypdb"
meta <- download_eupath_metadata(webservice=webservice)
valid_metadata <- meta[["valid"]]

wanted <- "MHOM/COL"
entry <- get_eupath_entry(species=wanted, metadata=valid_metadata)

orgdb_result <- make_eupath_orgdb(entry)
actual <- orgdb_result[["orgdb_name"]]
expected <- grepl(x = actual, pattern = "MHOMCOL81")
test_that("Did make_eupath_orgdb return an expected result?", {
  expect_true(expected)
})

library(actual, character.only=TRUE)

orgdb_annot <- load_eupath_annotations(entry)
test_that("Do we get some expected annotation data?", {
  expect_gt(ncol(orgdb_annot), 90)
  expect_gt(nrow(orgdb_annot), 8000)
})

orgdb_go <- load_eupath_go(entry)
test_that("Do we get some expected GO data?", {
  expect_equal(ncol(orgdb_go), 4)
  expect_gt(nrow(orgdb_go), 20000)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 030_leishmania_panamensis in ", elapsed,  " seconds."))
