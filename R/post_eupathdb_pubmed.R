#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
<<<<<<< HEAD
<<<<<<< HEAD:R/post_eupath_pubmed.R
post_eupath_pubmed_table <- function(entry = NULL, build_dir = "EuPathDB", overwrite = FALSE) {
=======
post_eupathdb_pubmed_table <- function(entry=NULL, workdir="EuPathDB", overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_pubmed.R
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(build_dir, "rda")
=======
post_eupathdb_pubmed_table <- function(entry, workdir = "EuPathDB", overwrite = FALSE) {
  # create output directory if needed
  rdadir <- file.path(workdir, "rda")

>>>>>>> a0cb0dd (Continuing refactoring)
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }

  # check for existing rda file
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_pubmed_table.rda"))

  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      info(sprintf("Loading previously generated pubmed table from %s...", savefile))
      result <- new.env()
      load(savefile, envir = result)
      result <- result[["result"]]
      return(result)
    }
  }
<<<<<<< HEAD:R/post_eupath_pubmed.R
  result <- post_eupath_table(entry, tables = "PubMed", table_name = "pubmed")
=======

  species <- entry[["TaxonUnmodified"]]

  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina
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
        "tables" = "PubMed",
        "includeEmptyTables" = jsonlite::unbox("true"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  result <- post_eupathdb_table(query_body, entry, table_name="pubmed")
<<<<<<< HEAD
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_pubmed.R
=======

  # set column names for non-empty results
>>>>>>> a0cb0dd (Continuing refactoring)
  if (nrow(result) > 0) {
    colnames(result) <- c("GID", "PUBMED_ID", "PUBMED_DOI", "PUBMED_TITLE", "PUBMED_AUTHORS")
  }
<<<<<<< HEAD
<<<<<<< HEAD
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
=======
  info("Saving ", savefile)
  save(result, file=savefile)
>>>>>>> fc81572 (Some more refactoring / fixes)
=======

  info("Saving ", savefile)
  save(result, file = savefile)

>>>>>>> a0cb0dd (Continuing refactoring)
  return(result)
}
