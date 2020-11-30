start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("020_orgdb_fungidb
  12\n")

## These tests are intended to ensure that the creation of new orgdbs are sane.
## Since I am primarily interested in the tritrypdb, I will focus there.

webservice <- "fungidb"
meta <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
all_metadata <- meta[["valid"]]

eu_version <- get_versions()[["eu_version"]]

wanted <- "pombe"
entry <- get_eupath_entry(species=wanted, metadata=meta)

orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE, verbose=TRUE)
expected <- paste0("org.Spombe.972h.", eu_version, ".eg.db")
actual <- as.character(orgdb_result)
test_that("Did make_eupath_orgdb return an expected result?", {
  expect_equal(actual, expected)
})

library(expected, character.only=TRUE)

orgdb_annot <- load_eupath_annotations(entry)
test_that("Do we get some expected annotation data?", {
  expect_gt(ncol(orgdb_annot), 90)
  expect_gt(nrow(orgdb_annot), 12000)
})

orgdb_go <- load_eupath_go(entry)
test_that("Do we get some expected GO data?", {
  expect_equal(ncol(orgdb_go), 4)
  expect_gt(nrow(orgdb_go), 97000)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 020_orgdb_fungidb in ", elapsed,  " seconds."))
