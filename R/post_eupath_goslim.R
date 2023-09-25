#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
#' @return  A big honking table.
post_eupath_goslim_table <- function(entry, working_species, overwrite = FALSE,
                                     verbose = FALSE) {
  rda <- check_rda("goslim", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning GOslim data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, working_species,
                              tables = "GOSlim", table_name = "goslim")
  colnames(result) <- gsub(x = colnames(result), pattern = "GOSLIM_X_",
                           replacement = "GOSLIM_")
  colnames(result) <- gsub(x = colnames(result), pattern = "GO_SLIM_",
                           replacement = "")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
