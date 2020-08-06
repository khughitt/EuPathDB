#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
<<<<<<< HEAD:R/post_eupath_linkout.R
post_eupath_linkout_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
=======
post_eupathdb_linkout_table <- function(entry=NULL, workdir="EuPathDB", overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_linkout.R
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_linkout_table.rda"))
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

  species <- entry[["TaxonUnmodified"]]
  result <- post_eupath_table(entry, tables = "GeneLinkouts", table_name = "linkout")

<<<<<<< HEAD:R/post_eupath_linkout.R
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
=======
  result <- post_eupathdb_table(query_body, entry, table_name="linkout")

  message("  Saving ", savefile)
  save(result, file=savefile)
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_linkout.R
  return(result)
}
