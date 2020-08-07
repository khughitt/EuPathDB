#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
#' @return  A big honking table.
<<<<<<< HEAD:R/post_eupath_goslim.R
post_eupath_goslim_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
=======
post_eupathdb_goslim_table <- function(entry=NULL, workdir="EuPathDB", overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_goslim.R
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
      info(savefile, " already exists! Delete this file if you wish to regenerate it.")
      result <- new.env()
      load(savefile, envir = result)
      result <- result[["result"]]
      return(result)
    }
  }

<<<<<<< HEAD:R/post_eupath_goslim.R
  result <- post_eupath_table(entry, tables = "GOSlim", table_name = "goslim")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
=======
  species <- entry[["TaxonUnmodified"]]
  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = "GOTerms",
        "includeEmptyTables" = jsonlite::unbox("true"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  result <- post_eupathdb_table(query_body, entry, table_name="goslim")
  info("Saving ", savefile)
  save(result, file=savefile)
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_goslim.R
  return(result)
}
