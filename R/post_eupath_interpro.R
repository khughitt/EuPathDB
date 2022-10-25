#' Use the post interface to get interpro data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_interpro_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
  rda <- check_rda("interpro", entry, build_dir, overwrite)
  if (!is.null(rda)) {
    return(rda)
  }

  result <- post_eupath_table(entry, tables = "InterPro", table_name="interpro")
  colnames(result) <- gsub(x = colnames(result), pattern = "INTERPRO_INTERPRO",
                           replacement = "INTERPRO")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
