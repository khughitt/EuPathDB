#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
#' @return A big honking table.
post_eupath_go_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }

  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }

  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_go_table.rda"))
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

  result <- post_eupath_table(entry, tables = "GOTerms", table_name = "godb")
  colnames(result) <- gsub(x = colnames(result), pattern = "GO_GO", replacement = "GO")
  colnames(result) <- gsub(x = colnames(result), pattern = "^GO$", replacement = "GO_GOID")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
