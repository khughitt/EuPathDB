start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
library(RSQLite)

setwd("~/scratch/eupathdb/EuPathDB/GRanges/3.12")

file_list <- list.files()
file_end <- length(file_list)
start <- 1
for (f in start:file_end) {
  file <- file_list[f]
  message(f, "/", file_end, ": testing ", file, ".")
  ## Test that we can connect to the file.
  con <- attach(file)
  expected_class <- "environment"
  expect_equal(class(con)[1], expected_class)
  ## Test that there is some data...
  ## And that they have data.
  min_rows <- 25
  env_data <- ls(con)
  data <- con[[env_data]]
  found_rows <- nrow(as.data.frame(data))
  expect_gt(found_rows, min_rows)
  detached <- paste0("file:", file)
  detach(name=detached, character.only=TRUE)
}

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 015_txdb.R in ", elapsed,  " seconds."))
