start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
library(RSQLite)

setwd("~/scratch/eupathdb/EuPathDB/OrgDb/3.9")
file_list <- list.files()
file_end <- length(file_list)
start <- 1
for (f in start:file_end) {
  file <- file_list[f]
  message(f, "/", file_end, ": testing ", file, ".")
  ## Test that we can connect to the file.
  con <- dbConnect(SQLite(), file)
  expected_class <- "SQLiteConnection"
  expect_equal(class(con)[1], expected_class)
  ## Test that we have some tables.
  required_tables <- c("chromosome", "gene_info", "genes")
  found_tables <- dbListTables(con)
  for (t in required_tables) {
    expect_true(t %in% found_tables)
  }
  ## And that they have data.
  min_rows <- 25
  data <- dbReadTable(con, "gene_info")
  found_rows <- nrow(data)
  expect_gt(found_rows, min_rows)
  dbDisconnect(con)
}

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 010annotation_eupathdb.R in ", elapsed,  " seconds."))
