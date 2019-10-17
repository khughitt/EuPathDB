#!/usr/bin/env Rscript
source("config.R")
webservice <- "eupathdb"
meta <- download_eupath_metadata(
  bioc_version=bioc_version, overwrite=TRUE, webservice=webservice,
  verbose=TRUE, eu_version=eu_version, write_csv=TRUE)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

start <- 1
for (it in start:end) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, eu_version=eu_version, copy_s3=TRUE)
    expected <- "bsgenome_name"
    actual <- names(bsgenome_result)
    testthat::test_that("Does make_eupath_bsgenome return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, eu_version=eu_version, copy_s3=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the OrgDB.")
    } else {
      actual <- orgdb_result[["orgdb_name"]]
      expected <- pkgnames[["orgdb"]]
      test_that("Does make_eupath_orgdb return something sensible?", {
        expect_equal(expected, actual)
      })
      results[["orgdb"]][[species]] <- orgdb_result
    }
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, eu_version=eu_version, copy_s3=TRUE)
    if (is.null(txdb_result)) {
      message("Unable to create the txdb package.")
    } else {
      actual <- txdb_result[["txdb_name"]]
      expected <- pkgnames[["txdb"]]
      test_that("Does make_eupath_txdb return something sensible?", {
        expect_equal(expected, actual)
      })
      results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
    }
  }
  if (isTRUE(organdb)) {
    organ_result <- make_eupath_organismdbi(entry, eu_version=eu_version, copy_s3=TRUE)
    actual <- organ_result[["organdb_name"]]
    expected <- pkgnames[["organismdbi"]]
    test_that("Does make_eupath_organismdbi return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.

## check_csv checks each metadata csv file to see that the files exist.
## check_files checks the list of files in each directory to see that they all have
## entries in the csv.
if (isTRUE(bsgenome)) {
  bs_csv <- check_csv(file_type="BSgenome", bioc_version=bioc_version, eu_version=eu_version)
  bs_files <- check_files("BSgenome", bioc_version=bioc_version, eu_version=eu_version)
  csv_copy_path <- file.path(path.package("EuPathDB"), "inst", "extdata", bs_csv)
  copied <- file.copy(bs_csv, csv_copy_path)
  expect_true(copied)
  bs_checked <- AnnotationHubData::makeAnnotationHubMetadata(path.package("EuPathDB"), bs_csv)
  save(list=c("bs_checked"), file="bsgenome_metadata.rda")
}
if (isTRUE(orgdb)) {
  org_csv <- check_csv(file_type="OrgDb", bioc_version=bioc_version, eu_version=eu_version)
  org_files <- check_files("OrgDb", bioc_version=bioc_version, eu_version=eu_version)
  csv_copy_path <- file.path(path.package("EuPathDB"), "inst", "extdata", org_csv)
  copied <- file.copy(org_csv, csv_copy_path)
  expect_true(copied)
  org_checked <- AnnotationHubData::makeAnnotationHubMetadata(path.package("EuPathDB"), org_csv)
  save(list=c("org_checked"), file="orgdb_metadata.rda")
}
if (isTRUE(txdb)) {
  txdb_csv <- check_csv(file_type="TxDb", bioc_version=bioc_version, eu_version=eu_version)
  tx_files <- check_files("TxDb", bioc_version=bioc_version, eu_version=eu_version)
  csv_copy_path <- file.path(path.package("EuPathDB"), "inst", "extdata", txdb_csv)
  copied <- file.copy(txdb_csv, csv_copy_path)
  expect_true(copied)
  tx_checked <- AnnotationHubData::makeAnnotationHubMetadata(path.package("EuPathDB"), txdb_csv)
  save(list=c("tx_checked"), file="txdb_metadata.rda")
}
if (isTRUE(organdb)) {
  organ_csv <- check_csv(file_type="OrganismDbi", bioc_version=bioc_version, eu_version=eu_version)
  organ_files <- check_files("OrganismDbi", bioc_version=bioc_version, eu_version=eu_version)
  csv_copy_path <- file.path(path.package("EuPathDB"), "inst", "extdata", organ_csv)
  copied <- file.copy(organ_csv, csv_copy_path)
  expect_true(copied)
  organ_checked <- AnnotationHubData::makeAnnotationHubMetadata(path.package("EuPathDB"), organ_csv)
  save(list=c("organ_checked"), file="organdb_metadata.rda")
}
if (isTRUE(granges)) {
  grange_csv <- check_csv(file_type="GRanges", bioc_version=bioc_version, eu_version=eu_version)
  grange_files <- check_files("GRanges", bioc_version=bioc_version, eu_version=eu_version)
  csv_copy_path <- file.path(path.package("EuPathDB"), "inst", "extdata", grange_csv)
  copied <- file.copy(grange_csv, csv_copy_path)
  expect_true(copied)
  grange_checked <- AnnotationHubData::makeAnnotationHubMetadata(path.package("EuPathDB"), grange_csv)
  save(list=c("grange_checked"), file="granges_metadata.rda")
}
