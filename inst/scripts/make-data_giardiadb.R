#!/usr/bin/env Rscript
library(testthat)
library(EuPathDB)
library(parallel)
library(doParallel)
library(iterators)
library(foreach)

## This test is intended to work through a query from cparsania
## https://github.com/khughitt/EuPathDB/issues/5#issuecomment-468121838

returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
unlink("*.csv")
## Of all the things to parallelize, this should be #1.
## Once I work out these other oddities, this will be priority.
meta <- download_eupath_metadata(bioc_version="3.9", overwrite=TRUE,
                                 webservice="giardiadb", write_csv=TRUE)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

## I am going to gently parallelize this.  For perhaps the stupidest reason possible.
## Something in import.gff, rsqlite, and bsgenome are not letting go of file handles.
## Therefore after a few species this script is doomed to fail.
## I am thinking that because parallel pushes the body of the foreach() loop into a separate
## process, then those descriptors should get released when the child R process closes.
## I am pretty sure this is the exact _wrong_ reason for parallel programming, but I am not going
## to spend my time diagnosing problems in rtracklayer, bsgenome, and/or rsqlite.

## I also had to change /etc/security/limits.conf
## *     soft   nofile  81920
## *     hard   nofile  409600

res <- NULL
results <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "organismdbi" = list(),
  "txdb" = list(),
  "granges" = list())
##cl <- parallel::makeCluster(2)
##doParallel::registerDoParallel(cl)
pkglist <- c("EuPathDB", "testthat")
##res <- foreach(it=1:end, .packages=pkglist) %dopar% {

bsgenome <- FALSE
orgdb <- TRUE
txdb <- TRUE
organdb <- FALSE
start <- 1
for (it in start:end) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, copy_s3=TRUE)
    expected <- "bsgenome_name"
    actual <- names(bsgenome_result)
    testthat::test_that("Does make_eupath_bsgenome return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the other packages.")
      next
    }
    actual <- orgdb_result[["orgdb_name"]]
    expected <- pkgnames[["orgdb"]]
    test_that("Does make_eupath_orgdb return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, copy_s3=TRUE)
    actual <- txdb_result[["txdb_name"]]
    expected <- pkgnames[["txdb"]]
    test_that("Does make_eupath_txdb return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  }
  if (isTRUE(organdb)) {
    organ_result <- make_eupath_organismdbi(entry, copy_s3=TRUE)
    actual <- organ_result[["organdb_name"]]
    expected <- pkgnames[["organismdbi"]]
    test_that("Does make_eupath_organismdbi return something sensible?", {
      expect_equal(expected, actual)
    })
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.
##parallel::stopCluster(cl)
