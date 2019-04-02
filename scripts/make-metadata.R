#!/usr/bin/env Rscript
library(testthat)
library(EuPathDB)

## This script invokes download_eupath_metadata() with arguments to write out
## metadata csv files for AnnotationHubData.

devtools::load_all("~/scratch/git/eupathdb_forked")
returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
all_metadata <- download_eupath_metadata(bioc_version="3.9",
                                         write_csv=TRUE,
                                         overwrite=TRUE)

## Lets take a moment to check the csv files.
## I am not certain that the RDataFiles exist, lets see if that is true.
bioc_version <- all_metadata[1, "BiocVersion"]
eupath_version <- all_metadata[1, "SourceVersion"]
types <- c("BSgenome", "GRanges", "OrganismDbi", "OrgDb", "TxDb")
for (type in types) {
  meta <- glue::glue("{type}_bioc_v{bioc_version}_eupathdb_v{eupath_version}_metadata.csv")
  test_metadata <- AnnotationHubData::makeAnnotationHubMetadata(
                                        path.package("EuPathDB", fileName=meta)
}
