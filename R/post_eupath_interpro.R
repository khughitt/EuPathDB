#' Use the post interface to get interpro data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_interpro_table <- function(entry, working_species,
                                       overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("interpro", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning GOslim data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, working_species,
                              tables = "InterPro", table_name="interpro")
  colnames(result) <- gsub(x = colnames(result), pattern = "INTERPRO_INTERPRO",
                           replacement = "INTERPRO")
  factor_columns <- c("INTERPRO_NAME")
  for (f in factor_columns) {
    if (!is.null(result[[f]])) {
      result[[f]] <- as.factor(result[[f]])
    }
  }
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
