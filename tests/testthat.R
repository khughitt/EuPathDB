#!/usr/bin/env Rscript
library(methods)
library(testthat)
library(EuPathDB)

all <- data.frame()
result <- 0

message("Beginning test_dir('orgdb')")
test_result <- try(testthat::test_dir("tests/orgdb", reporter="summary"))
if (class(test_result) == "try-error") {
  result <- result + 1
  test_result <- data.frame()
}
