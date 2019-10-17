#' Use the post interface to get linkout data.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
#' @return  A big honking table.
post_eupath_pubmed_table <- function(entry=NULL, workdir="EuPathDB", overwrite=FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(workdir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive=TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_pubmed_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("  Delete the file ", savefile, " to regenerate.")
      result <- new.env()
      load(savefile, envir=result)
      result <- result[["result"]]
      return(result)
    }
  }

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

  result <- post_eupath_table(query_body, entry, table_name="pubmed")
  if (nrow(result) > 0) {
    colnames(result) <- c("GID", "PUBMED_ID", "PUBMED_DOI",
                          "PUBMED_TITLE", "PUBMED_AUTHORS")
  }
  message("  Saving ", savefile)
  save(result, file=savefile)
  return(result)
}
