#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_linkout_table <- function(entry, working_species, overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("linkout", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning GOslim data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  species <- entry[["TaxonUnmodified"]]
  result <- post_eupath_table(entry, working_species,
                              tables = "GeneLinkouts", table_name = "linkout")
  colnames(result) <- gsub(x = colnames(result), pattern = "LINKOUT_X_",
                           replacement = "LINKOUT_")

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
