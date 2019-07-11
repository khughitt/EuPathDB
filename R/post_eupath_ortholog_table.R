#' Use the post interface to get ortholog data.
#'
#' Unfortunately, this function more often then not leads to a crash of the
#' eupathdb webservers.  As a result, I wrote a GET version of this which
#' iterates one gene at a time.
#'
#' @param entry The full annotation entry.
#' @param dir Location to which to save an intermediate savefile.
#' @param table This defaults to the 'OrthologsLite' table, but that does not
#'   exist at all eupathdb subprojects.
#' @param overwrite Overwrite incomplete savefiles?
#' @return A big honking table.
post_eupath_ortholog_table <- function(entry=NULL, dir="EuPathDB", table="OrthologsLite",
                                       gene_ids=NULL, overwrite=FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive=TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
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

  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina
  species <- entry[["TaxonUnmodified"]]
  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list(),
      "wdk_weight" = jsonlite::unbox(10)
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = table,
        "includeEmptyTables" = jsonlite::unbox("false"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  result <- post_eupath_table(query_body, entry, table_name="orthologs")
  if (nrow(result) == 0) {
    message("Failed to download the OrthologsLite table, attempting to download 1 gene at a time.")
    result <- get_orthologs_all_genes(entry, dir=dir,
                                      gene_ids=gene_ids, overwrite=overwrite)
  } else {
    colnames(result) <- c("GID", "GENE_ID", "ORTHOLOGS_GID", "ORTHOLOGS_ORGANISM",
                          "ORTHOLOGS_PRODUCT", "ORTHOLOGS_SYNTENIC")
    ## The GENE_ID column is redundant, drop it.
    result[["GENE_ID"]] <- NULL
  }

  message("  Saving ", savefile)
  save(result, file=savefile)
  return(result)
}
