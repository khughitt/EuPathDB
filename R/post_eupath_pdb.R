#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_pdb_table <- function(entry = NULL, overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("pdb", entry, build_dir, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning PDB data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, tables = "PdbSimilarities", table_name = "pdb")
  colnames(result) <- gsub(x = colnames(result), pattern = "PDB_PDB_", replacement = "PDB_")

  ## Fix the brain dead p value column.
  result[["PDB_PVALUE_NUM"]] <- gsub(pattern = " x 10<sup>(.*)</sup>", replacement = "e\\1",
                                     x = result[["PDB_PVALUE"]])
  empty_pvalue_idx <- result[["PDB_PVALUE"]] == ""
  result[empty_pvalue_idx, "PDB_PVALUE_NUM"] <- 1
  result[["PDB_PVALUE_NUM"]] <- as.numeric(result[["PDB_PVALUE_NUM"]])

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
