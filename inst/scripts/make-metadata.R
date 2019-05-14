#!/usr/bin/env Rscript
library(testthat)
library(EuPathDB)

## This script invokes download_eupath_metadata() with arguments to write out
## metadata csv files for AnnotationHubData.

returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
all_metadata <- download_eupath_metadata(bioc_version="3.9", version="42",
                                         write_csv=TRUE,
                                         overwrite=TRUE)
valid_metadata <- all_metadata[["valid"]]
invalid_metadata <- all_metadata[["invalid"]]

## Lets take a moment to check the csv files.
## I am not certain that the RDataFiles exist, lets see if that is true.
bioc_version <- valid_metadata[1, "BiocVersion"]
eupath_version <- valid_metadata[1, "SourceVersion"]

## I am not sure what format BSgenome will eventually be included into the hub,
## so leave it out for now.
## types <- c("BSgenome", "GRanges", "OrganismDbi", "OrgDb", "TxDb")
types <- c("GRanges", "OrganismDbi", "OrgDb", "TxDb")
for (type in types) {
  meta <- glue::glue("{type}_bioc_v{bioc_version}_eupathdb_v{eupath_version}_metadata.csv")
  path <- path.package("EuPathDB")
  message("Testing the csv file: ", meta)
  test_metadata <- AnnotationHubData::makeAnnotationHubMetadata(path, fileName=meta)
  expect_equal(class(test_metadata)[1], "list")
}
