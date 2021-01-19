start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("001_metadata_tritrypdb
  12\n")

webservice <- "tritrypdb"
meta <- download_eupath_metadata(
  overwrite=TRUE, webservice=webservice)
all_metadata <- meta[["valid"]]

expected <- 50
test_that("Do we get sufficient metadata?", {
  expect_gt(nrow(all_metadata), expected)
})

wanted <- "Friedlin"
entry <- get_eupath_entry(species=wanted, metadata=meta)

test_that("Did we get expected metadata for Leishmania major Friedlin?", {
  expect_equal(entry[["DataProvider"]], "TriTrypDB")
  expect_equal(entry[["TaxonUnmodified"]], "Leishmania major strain Friedlin")
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010_create.R in ", elapsed,  " seconds."))
