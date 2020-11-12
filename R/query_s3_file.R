#' Perform what should be a completely silly final check on the file which is to be copied to s3.
#'
#' This function really should not be needed.  But damn.  This will do a final check that the
#' data in the s3 staging directory is loadable in R and return the md5 sum of the file.
#' Thus the md5 sum may be added to the metadata.
#'
#' @param row Metadata row to query.
#' @param file_type Currently I have 3 file types of interest.
#' @param file_column Name of the column with the locations of the files of
#'  interest.
#' @return MD5 checksum of the resulting file, or NULL.
#' @export
query_s3_file <- function(row, file_type = "OrgDb", file_column = "OrgdbFile") {
  ret <- NULL
  file <- row[[file_column]]
  if (file_type == "GRanges") {
    ret <- query_s3_granges(file)
  } else if (file_type == "OrgDb") {
    ret <- query_s3_orgdb(file)
  } else if (file_type == "TxDb") {
    ret <- query_s3_txdb(file)
  } else {
    stop("I do not yet understand this file type.")
  }
  return(ret)
}

#' Perform what should be a completely silly final check on the file which is to be copied to s3.
#'
#' This function really should not be needed.  But damn.  This will do a final check that the
#' data in the s3 staging directory is loadable in R and return the md5 sum of the file.
#' Thus the md5 sum may be added to the metadata.
#'
#' @param file Filename to query.
#' @return MD5 sum of the file or NULL.
query_s3_granges <- function(file) {
  con <- try(attach(file), silent = TRUE)
  if (class(con)[1] == "try-error") {
    return(NULL)
  }
  expected_class <- "environment"
  testthat::expect_equal(class(con)[1], expected_class)
  ## Test that there is some data...
  ## And that they have data.
  min_rows <- 25
  env_data <- ls(con)
  data <- con[[env_data]]
  found_rows <- nrow(as.data.frame(data))
  testthat::expect_gt(found_rows, min_rows)
  detached <- paste0("file:", file)
  detach(name=detached, character.only = TRUE)
  md <- as.character(tools::md5sum(file))
  return(md)
}

#' Perform what should be a completely silly final check on the file which is to be copied to s3.
#'
#' This function really should not be needed.  But damn.  This will do a final check that the
#' data in the s3 staging directory is loadable in R and return the md5 sum of the file.
#' Thus the md5 sum may be added to the metadata.
#'
#' @param file Filename to query.
#' @return MD5 sum of the file or NULL.
query_s3_orgdb <- function(file) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  expected_class <- "SQLiteConnection"
  testthat::expect_equal(class(con)[1], expected_class)
  ## Test that we have some tables.
  required_tables <- c("chromosome", "gene_info", "genes")
  found_tables <- RSQLite::dbListTables(con)
  found_idx <- required_tables %in% found_tables
  if (length(found_idx) == 0 | sum(found_idx) != length(required_tables)) {
    message("Incomplete data for ", file, ".")
    return(NULL)
  }
  ## And that they have data.
  min_rows <- 25
  data <- RSQLite::dbReadTable(con, "gene_info")
  found_rows <- nrow(data)
  testthat::expect_gt(found_rows, min_rows)
  RSQLite::dbDisconnect(con)
  ## If we get here, then the data should be usable
  ## return the md5sum of the file.
  md <- as.character(tools::md5sum(file))
  return(md)
}

#' Perform what should be a completely silly final check on the file which is to be copied to s3.
#'
#' This function really should not be needed.  But damn.  This will do a final check that the
#' data in the s3 staging directory is loadable in R and return the md5 sum of the file.
#' Thus the md5 sum may be added to the metadata.
#'
#' @param file Filename to query.
#' @return MD5 sum of the file or NULL.
query_s3_txdb <- function(file) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  expected_class <- "SQLiteConnection"
  testthat::expect_equal(class(con)[1], expected_class)
  ## Test that we have some tables.
  required_tables <- c("cds", "gene", "exon")
  found_tables <- RSQLite::dbListTables(con)
  found_idx <- required_tables %in% found_tables
  if (length(found_idx) == 0 | sum(found_idx) != length(required_tables)) {
    message("Incomplete data for ", file, ".")
    return(NULL)
  }
  ## And that they have data.
  min_rows <- 25
  data <- RSQLite::dbReadTable(con, "cds")
  found_rows <- nrow(data)
  if (found_rows < min_rows) {
    message("Insufficient rows observed for ", file, ".")
    return(NULL)
  }
  ## If we get here, then the data should be usable
  ## return the md5sum of the file.
  md <- as.character(tools::md5sum(file))
  return(md)
}
