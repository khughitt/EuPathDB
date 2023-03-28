#' Use the post interface to get pathway data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite If trying again, overwrite the savefile?
#' @return A big honking table.
post_eupath_pathway_table <- function(entry = NULL, overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("pathway", entry, build_dir, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning pathway data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, tables = "MetabolicPathways", table_name = "pathway")
  colnames(result) <- gsub(x=colnames(result), pattern="^PATHWAY_PATHWAY$",
                           replacement="PATHWAY_ID")
  colnames(result) <- gsub(x=colnames(result), pattern="PATHWAY_PATHWAY",
                           replacement="PATHWAY")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file=savefile)
  return(result)
}
