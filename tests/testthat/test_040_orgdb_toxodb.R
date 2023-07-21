start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("040_orgdb_toxodb")

webservice <- "toxodb"
meta <- download_eupath_metadata(webservice = webservice)
all_metadata <- meta[["valid"]]

## Let us use the test suite to create the packages for
## every species in a given eupathdb subproject.

## These are therefore theoretically duplicates of the scripts in
## inst/scripts/make-*.R
species <- all_metadata[["Species"]]
end <- nrow(all_metadata)
install <- FALSE
copy_s3 <- FALSE
version <- NULL

for (it in seq_along(species)) {
  entry <- all_metadata[it, ]
  sp <- species[it]
  message("Starting generation of ", sp, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  bsgenome_result <- make_eupath_bsgenome(entry, eu_version = version, copy_s3 = copy_s3,
                                          install = install, reinstall = FALSE)
  orgdb_result <- make_eupath_orgdb(entry, eu_version = version, copy_s3 = copy_s3,
                                    install = install, reinstall = FALSE)
  txdb_result <- make_eupath_txdb(entry, eu_version = version, copy_s3 = copy_s3,
                                  install = install, reinstall = FALSE)
  organ_result <- make_eupath_organismdbi(entry, eu_version = version, copy_s3 = copy_s3,
                                          install = install, reinstall = FALSE)

} ## End iterating over every entry in the eupathdb metadata.




entry <- get_eupath_entry(species=wanted, metadata=all_metadata)
orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE, verbose=TRUE)
expected <- paste0("org.Lmajor.Friedlin.", eu_version, ".eg.db")
actual <- as.character(orgdb_result)
test_that("Did make_eupath_orgdb return an expected result?", {
  expect_equal(actual, expected)
})
library(expected, character.only=TRUE)
orgdb_annot <- load_eupath_annotations(entry)
test_that("Do we get some expected annotation data?", {
  expect_gt(ncol(orgdb_annot), 100)
  expect_gt(nrow(orgdb_annot), 9400)
})
orgdb_go <- load_eupath_go(entry)
test_that("Do we get some expected GO data?", {
  expect_equal(ncol(orgdb_go), 4)
  expect_gt(nrow(orgdb_go), 35800)
})

wanted <- "panamensis MHOM/COL"
entry <- get_eupath_entry(species=wanted, metadata=meta)
orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE, verbose=TRUE)
expected <- paste0("org.Lpanamensis.MHOMCOL81L13.", eu_version, ".eg.db")
actual <- as.character(orgdb_result)
test_that("Did make_eupath_orgdb return an expected result?", {
  expect_equal(actual, expected)
})
library(expected, character.only=TRUE)
orgdb_annot <- load_eupath_annotations(entry)
test_that("Do we get some expected annotation data?", {
  expect_gt(ncol(orgdb_annot), 100)
  expect_gt(nrow(orgdb_annot), 8700)
})
orgdb_go <- load_eupath_go(entry)
test_that("Do we get some expected GO data?", {
  expect_equal(ncol(orgdb_go), 4)
  expect_gt(nrow(orgdb_go), 19600)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 020_orgdb_tritrypdb in ", elapsed,  " seconds."))
