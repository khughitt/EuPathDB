#!/usr/bin/env Rscript
library(testthat)
library(EuPathDB)

## This test is intended to work through a query from cparsania
## https://github.com/khughitt/EuPathDB/issues/5#issuecomment-468121838

devtools::load_all("~/scratch/git/eupathdb_forked")
returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
all_metadata <- download_eupath_metadata(bioc_version="3.9", overwrite=TRUE)

end <- nrow(all_metadata)
for (it in 1:end) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  Sys.sleep(1)
  pkgnames <- get_eupath_pkgnames(entry)
  expected <- c("BiocVersion", "Genome", "NumGenes", "NumOrthologs",
                "SourceType", "SourceUrl", "SourceVersion", "Species",
                "TaxonomyId", "Coordinate_1_based", "DataProvider", "Maintainer",
                "Tags", "Title", "Description", "RDataClass", "DispatchClass",
                "ResourceName", "RDataPath")
  actual <- colnames(entry)
  test_that("Is there eupathdb metadata?", {
    expect_equal(expected, actual)
  })
  species <- entry[["Species"]]
  bsgenome_result <- make_eupath_bsgenome(entry)
  expected <- "bsgenome_name"
  actual <- names(bsgenome_result)
  testthat::test_that("Does make_eupath_bsgenome return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["bsgenome"]][[species]] <- bsgenome_result
  orgdb_result <- make_eupath_orgdb(entry)
  if (is.null(orgdb_result)) {
    next
  }
  actual <- orgdb_result[["orgdb_name"]]
  expected <- pkgnames[["orgdb"]]
  test_that("Does make_eupath_orgdb return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["orgdb"]] <- orgdb_result
  txdb_result <- make_eupath_txdb(entry)
  actual <- txdb_result[["txdb_name"]]
  expected <- pkgnames[["txdb"]]
  test_that("Does make_eupath_txdb return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  organ_result <- make_eupath_organismdbi(entry)
  actual <- organ_result[["organdb_name"]]
  expected <- pkgnames[["organismdbi"]]
  test_that("Does make_eupath_organismdbi return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["organismdbi"]] <- organ_result
  print(showConnections())
} ## End iterating over every entry in the eupathdb metadata.
