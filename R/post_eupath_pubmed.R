#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_pubmed_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
  rda <- check_rda("pubmed", entry, build_dir, overwrite)
  if (!is.null(rda)) {
    return(rda)
  }

  result <- post_eupath_table(entry, tables = "PubMed", table_name = "pubmed")
  if (nrow(result) > 0) {
    colnames(result) <- c("GID", "PUBMED_ID", "PUBMED_DOI",
                          "PUBMED_TITLE", "PUBMED_AUTHORS")
  }
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
