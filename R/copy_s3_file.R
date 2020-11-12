#' Copy the relevant file for each data type into a place which is easy for pickup by s3.
#'
#' @param src_dir Source directory for the package top be copied.
#' @param s3_file Where is the final file to be located?
#' @param type Which type of package is this?
copy_s3_file <- function(src_dir, s3_file, type = "bsgenome") {
  final_dir <- dirname(s3_file)
  if (!file.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE)
  }
  source_file <- ""
  if (type == "orgdb") {
    base_file <- basename(s3_file)
    source_file <- file.path(src_dir, "inst", "extdata", base_file)
  } else if (type == "txdb") {
    base_file <- basename(s3_file)
    source_file <- file.path(src_dir, base_file)
    file.exists(source_file)
  } else if (type == "organismdbi") {
    source_file <- file.path(src_dir, "data", "graphInfo.rda")
  } else if (type == "granges") {
    source_file <- src_dir
  } else if (type == "bsgenome") {
    source_file <- file.path(src_dir, "inst", "extdata", "single_sequences.2bit")
  }
  copied <- file.copy(source_file, s3_file, overwrite = TRUE)
  return(copied)
}
