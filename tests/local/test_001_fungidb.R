start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("test_001_anidulans.R
  12\n")

## This test is intended to work through a query from cparsania
## https://github.com/khughitt/EuPathDB/issues/5#issuecomment-468121838

returns <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
fungi_metadata <- download_eupath_metadata(webservice="fungidb")

for (it in 1:nrow(fungi_metadata)) {
  entry <- fungi_metadata[it, ]
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
  expected <- "bsgnome_name"
  actual <- names(bsgenome_result)
  test_that("Does make_eupath_bsgenome return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["bsgenome"]][[species]] <- bsgenome_result

  txdb_result <- make_eupath_txdb(entry)
  expected <- "txdb_name"
  actual <- names(txdb_result)
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

  orgdb_result <- make_eupath_orgdb(entry)
  expected <- "orgdb_name"
  actual <- names(txdb_result)
  test_that("Does make_eupath_orgdb return something sensible?", {
    expect_equal(expected, actual)
  })
  returns[["orgdb"]] <- orgdb_result

  organ_result <- make_eupath_organismdbi(entry)
  returns[["organismdbi"]] <- organ_result
} ## End iterating over every entry in the eupathdb metadata.

savefile <- "fungidb_result.rda"
save(returns, file=savefile)

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
