start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("001_metadata_fungidb
  12\n")

meta <- download_eupath_metadata(overwrite=TRUE, webservice="fungidb")
all_metadata <- meta[["valid"]]

actual <- nrow(all_metadata)
expected <- 150
test_that("Do we get sufficient metadata?", {
  expect_gt(nrow(all_metadata), expected)
})

wanted <- "pombe"
entry <- get_eupath_entry(species=wanted, metadata=meta)

test_that("Did we get expected metadata for Schizosaccharomyces pombe?", {
  expect_equal(entry[["DataProvider"]], "FungiDB")
  expect_equal(entry[["TaxonUnmodified"]], "Schizosaccharomyces pombe 972h-")
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 002_metadata_fungidb in ", elapsed,  " seconds."))
