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
                                       overwrite=FALSE) {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("We can save some time by reading the savefile.")
      message("Delete the file ", savefile, " to regenerate.")
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
  if (table == "OrthologsLite") {
    ## Because the orthologslite table changes some column names...
    ## Damn, the new eupathDB is timing out on me.
    colnames(result) <- gsub(x=colnames(result), pattern="ORTHOLOGS_ORTHOLOG",
                             replacement="ORTHOLOGS_ID")
    colnames(result) <- gsub(x=colnames(result), pattern="ORTHOLOGS_GENE_ID_1",
                             replacement="ORTHOLOGS_GENE_ID")
  } else {
    colnames(result) <- gsub(x=colnames(result), pattern="ORTHOLOGS_ORTHOLOG",
                             replacement="ORTHOLOGS_ID")
    colnames(result) <- gsub(x=colnames(result), pattern="ORTHOLOGS_GENE_ID_1",
                             replacement="ORTHOLOGS_GENE_ID")
  }
  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}
