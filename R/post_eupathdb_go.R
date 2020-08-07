#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
<<<<<<< HEAD:R/post_eupath_go.R
#' @return A big honking table.
post_eupath_go_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
=======
#' @return  A big honking table.
post_eupathdb_go_table <- function(entry=NULL, workdir="EuPathDB", overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_go.R
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
      info(savefile, " already exists! Delete this file if you wish to regenerate it.")
      result <- new.env()
      load(savefile, envir = result)
      result <- result[["result"]]
      return(result)
    }
  }

<<<<<<< HEAD:R/post_eupath_go.R
  result <- post_eupath_table(entry, tables = "GOTerms", table_name = "godb")
  colnames(result) <- gsub(x = colnames(result), pattern = "GO_GO", replacement = "GO")
  colnames(result) <- gsub(x = colnames(result), pattern = "^GO$", replacement = "GO_GOID")
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

  result <- post_eupathdb_table(query_body, entry, table_name="go")
  colnames(result) <- gsub(x=colnames(result), pattern="GO_GO", replacement="GO")
  message("  Saving ", savefile)
  save(result, file=savefile)
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_go.R
  return(result)
}
