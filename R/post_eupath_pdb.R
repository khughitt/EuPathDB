#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_pdb_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_pdb_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("  Delete the file ", savefile, " to regenerate.")
      result <- new.env()
      load(savefile, envir = result)
      result <- result[["result"]]
      return(result)
    }
  }
  result <- post_eupath_table(entry, tables = "PdbSimilarities", table_name = "pdb")
  ## Fix the brain dead p value column.
  result[["PDB_P_VALUE"]] <- gsub(pattern = " x 10<sup>(.*)</sup>", replacement = "e\\1",
                                  x = result[["PDB_P_VALUE"]])
  result[["PDB_P_VALUE"]] <- as.numeric(result[["PDB_P_VALUE"]])

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
