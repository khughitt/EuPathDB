#' Perform what should be a completely silly final check on the file which is to be copied to s3.
#' This function really should not be needed.  But damn.  This will do a final check that the
#' data in the s3 staging directory is loadable in R and return the md5 sum of the file.
#' Thus the md5 sum may be added to the metadata.
#'
#' @param row Metadata row to query.
#' @param file_type Currently I have 3 file types of interest.
#' @param file_column Column of the metadata to use to get the filename.
query_s3_file <- function(row, file_type="OrgDb", file_column="OrgdbFile") {
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

#' As yet another test, this function will download all the AH data one species at a time.
#'
#' @param testing Use the annotationHub TESTING service rather than production.
#' @param file_type Type of data to query.
#' @param cachedir Place to put the downloaded files, useful for if one's homedirectory is too small.
query_s3_ah <- function(testing=TRUE, file_type="OrgDb", cachedir="~/scratch/eupathdb/cache") {
  testing <- AnnotationHub::setAnnotationHubOption("TESTING", testing)
  cache <- AnnotationHub::setAnnotationHubOption("CACHE", cachedir)
  ah <- AnnotationHub::AnnotationHub()
  entries <- AnnotationHub::query(x=ah, pattern=c("EuPathDB", file_type))
  sad <- c()
  happy <- c()
  start <- 1
  end <- length(entries)
  ids <- names(entries)
  material <- as.data.frame(AnnotationHub::mcols(entries))
  rdata <- material[["rdatapath"]]
  for (e in start:end) {
    Sys.sleep(1)  ## An attempt to avoid another cache corruption.
    id <- ids[e]
    rda <- rdata[e]
    message(e, "/", end, ": downloading ", id, " from AnnotationHub.")
    downloaded <- try(entries[[e]])
    if (class(downloaded)[1] == "try-error") {
      message("There was a failure for ", rda)
      sad <- c(sad, rda)
    } else {
      if (file_type == "OrgDb") {
        if (length(AnnotationDbi::columns(downloaded)) < 30) {
          message("There are too few columns for ", rda)
          sad <- c(sad, rda)
        } else if (length(AnnotationDbi::keys(downloaded)) < 25) {
          message("There are too few keys for ", rda)
          sad <- c(sad, rda)
        } else {
          happy <- c(happy, rda)
        }
      } else if (file_type == "TxDb") {
        if (length(AnnotationDbi::columns(downloaded)) < 15) {
          message("There are too few columns for ", rda)
          sad <- c(sad, rda)
        } else if (length(AnnotationDbi::keys(downloaded)) < 25) {
          message("There are too few keys for ", rda)
          sad <- c(sad, rda)
        } else {
          happy <- c(happy, rda)
        }
      } else if (file_type == "GRanges") {
        stuff <- as.data.frame(downloaded)
        if (ncol(stuff) < 10) {
          message("There are too few columns for ", rda)
          sad <- c(sad, rda)
        } else if (nrow(stuff) < 25) {
          message("There are too few keys for ", rda)
          sad <- c(sad, rda)
        } else {
          happy <- c(happy, rda)
        }
      } else {
        stop("I do not know this file type: ", file_type)
      }
    } ## Ending the else
  } ## Ending the for loop
}

query_s3_granges <- function(file) {
  con <- attach(file)
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
  detach(name=detached, character.only=TRUE)
  md <- as.character(tools::md5sum(file))
  return(md)
}

query_s3_orgdb <- function(file) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  expected_class <- "SQLiteConnection"
  testthat::expect_equal(class(con)[1], expected_class)
  ## Test that we have some tables.
  required_tables <- c("chromosome", "gene_info", "genes")
  found_tables <- RSQLite::dbListTables(con)
  for (t in required_tables) {
    testthat::expect_true(t %in% found_tables)
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

query_s3_txdb <- function(file) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  expected_class <- "SQLiteConnection"
  testthat::expect_equal(class(con)[1], expected_class)
  ## Test that we have some tables.
  required_tables <- c("cds", "gene", "exon")
  found_tables <- RSQLite::dbListTables(con)
  for (t in required_tables) {
    testthat::expect_true(t %in% found_tables)
  }
  ## And that they have data.
  min_rows <- 25
  data <- RSQLite::dbReadTable(con, "cds")
  found_rows <- nrow(data)
  testthat::expect_gt(found_rows, min_rows)
  RSQLite::dbDisconnect(con)
  ## If we get here, then the data should be usable
  ## return the md5sum of the file.
  md <- as.character(tools::md5sum(file))
  return(md)
}
