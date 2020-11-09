#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
#' @return  A big honking table.
post_eupath_goslim_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }

  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_goslim_table.rda"))
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

  result <- post_eupath_table(entry, tables = "GOSlim", table_name = "goslim")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
