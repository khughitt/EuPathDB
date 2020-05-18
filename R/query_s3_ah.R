
#' As yet another test, this function will download all the AH data one species at a time.
#'
#' This may be run after the data has been uploaded to the annotationhub to
#' ensure that all the uploaded files are actually functional.
#'
#' @param testing Use the annotationHub TESTING service rather than production.
#' @param file_type Type of data to query.
#' @param cachedir Place to put the downloaded files, useful for if one's homedirectory is too small.
query_s3_ah <- function(testing=TRUE, file_type="OrgDb", cachedir="~/scratch/eupathdb/cache",
                        csv="inst/extdata/OrgDb_biocv3.10_eupathdbv46_metadata.csv") {
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
    message(e, "/", end, ": downloading ", id, " from AnnotationHub: ", rda, ".")
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
  retlist <- list(
    "happy" = happy,
    "sad" = sad)
  return(retlist)
}
