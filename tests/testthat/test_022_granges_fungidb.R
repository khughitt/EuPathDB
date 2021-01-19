start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("012_granges_fungidb
	  12\n")

webservice <- "fungidb"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
wanted <- "pombe"

entry <- get_eupath_entry(species=wanted, metadata=testing)
granges <- make_eupath_granges(entry=entry)
load(file=granges[["rda"]])
data <- get0(granges$variable)
df <- as.data.frame(data)
col <- ncol(df)
row <- nrow(df)
test_that("Do we get some granges information?", {
  expect_equal(15, col)
  expect_gt(row, 46000)
})

wanted <- "versicolor"
entry <- get_eupath_entry(species=wanted, metadata=testing)
granges <- make_eupath_granges(entry=entry)
load(file=granges[["rda"]])
data <- get0(granges$variable)
df <- as.data.frame(data)
col <- ncol(df)
row <- nrow(df)
test_that("Do we get some granges information?", {
  expect_gt(col, 13)
  expect_gt(row, 122000)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 022_granges_fungidb in ", elapsed,  " seconds."))
