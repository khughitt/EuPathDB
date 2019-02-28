start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("test_001_anidulans.R
  12\n")

## This test is intended to work through a query from cparsania
## https://github.com/khughitt/EuPathDB/issues/5#issuecomment-468121838

## I have a suspicion that the problem is the curl installation on his computer.
## But that is a pretty arbitrary guess.

nidulans <- make_eupath_organismdbi(species="nidulans", webservice="fungidb")

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
