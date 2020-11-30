start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("012_granges_tritrypdb
	  12\n")

webservice <- "tritrypdb"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
wanted <- "Friedlin"

entry <- get_eupath_entry(species=wanted, metadata=testing)
granges <- make_eupath_granges(entry=entry)
load(file=granges[["rda"]])
data <- get0(granges$variable)
df <- as.data.frame(data)
col <- ncol(df)
row <- nrow(df)
test_that("Do we get some granges information?", {
  expect_equal(15, col)
  expect_gt(row, 37300)
})

wanted <- "panamensis"
entry <- get_eupath_entry(species=wanted, metadata=testing)
granges <- make_eupath_granges(entry=entry)
load(file=granges[["rda"]])
data <- get0(granges$variable)
df <- as.data.frame(data)
col <- ncol(df)
row <- nrow(df)
test_that("Do we get some granges information?", {
  expect_equal(15, col)
  expect_gt(row, 35100)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 012_granges_tritrypdb in ", elapsed,  " seconds."))
