start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("test_001_anidulans.R
  12\n")

## This test is intended to work through a query from cparsania
## https://github.com/khughitt/EuPathDB/issues/5#issuecomment-468121838

devtools::load_all("~/scratch/git/eupathdb_forked")

returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
fungi_metadata <- download_eupath_metadata(webservice="fungidb")

end <- nrow(fungi_metadata)
for (it in 1:nrow(fungi_metadata)) {
  entry <- fungi_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
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
  test_that("Does make_eupath_bsgenome return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["bsgenome"]][[species]] <- bsgenome_result
  ## orgdb_result <- make_eupath_orgdb(entry, reinstall=TRUE)
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
  granges_result <- rtracklayer::import.gff3(txdb_result[["gff"]])
  actual <- grepl(pattern="^GRanges object", x=summary(granges_result))
  test_that("Did we get a granges object from the txdb?", {
    expect_true(actual)
  })
  returns[["granges"]] <- granges_result
  organ_result <- make_eupath_organismdbi(entry)
  actual <- organ_result[["organdb_name"]]
  expected <- pkgnames[["organismdbi"]]
  test_that("Does make_eupath_organismdbi return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["organismdbi"]] <- organ_result
} ## End iterating over every entry in the eupathdb metadata.

savefile <- "fungidb_result.rda"
save(returns, file=savefile)

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
