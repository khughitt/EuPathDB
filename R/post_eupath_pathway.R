#' Use the post interface to get pathway data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite If trying again, overwrite the savefile?
#' @return A big honking table.
post_eupath_pathway_table <- function(entry, working_species,
                                      overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("pathway", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning pathway data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, working_species,
                              tables = "MetabolicPathways", table_name = "pathway")
  colnames(result) <- gsub(x=colnames(result), pattern="^PATHWAY_PATHWAY$",
                           replacement="PATHWAY_ID")
  colnames(result) <- gsub(x=colnames(result), pattern="PATHWAY_PATHWAY",
                           replacement="PATHWAY")

  factor_columns <- c("PATHWAY_SOURCE", "PATHWAY_EXACT_EC_NUMBER_MATCH")
  for (f in factor_columns) {
    if (!is.null(result[[f]])) {
      result[[f]] <- as.factor(result[[f]])
    }
  }

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file=savefile)
  return(result)
}
