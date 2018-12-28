#!/usr/bin/env Rscript
library(methods)
library(testthat)
library(EuPathDb)

all <- data.frame()
result <- 0 

message("Beginning test_dir('travis')")
test_result <- try(testthat::test_dir("tests/travis", reporter="summary"))
if (class(test_result) == "try-error") {
  result <- result + 1
  test_result <- data.frame()
}

