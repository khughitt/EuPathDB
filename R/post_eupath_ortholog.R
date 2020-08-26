#' Use the post interface to get ortholog data.
#'
#' The folks at the EuPathDB kindly implemented the table 'OrthologsLite'
#' which makes it possible for me to use this function without trouble.
#'
#' @param entry The full annotation entry.
#' @param workdir Location to which to save an intermediate savefile.
#' @param table This defaults to the 'OrthologsLite' table, but that does not
#'  exist at all eupathdb subprojects.
#' @param gene_ids When provided, ask only for the orthologs for these genes.
#' @param overwrite Overwrite incomplete savefiles?
#' @return A big honking table.
post_eupath_ortholog_table <- function(ortholog_table = NULL, entry = NULL, workdir = "EuPathDB",
                                       table = "OrthologsLite", gene_ids = NULL,
                                       overwrite = FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(workdir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
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

  result <- post_eupath_table(query_body, entry, table_name = "orthologs")
  colnames(result) <- c("GID", "GENE_ID", "ORTHOLOGS_GID", "ORTHOLOGS_ORGANISM",
                        "ORTHOLOGS_PRODUCT", "ORTHOLOGS_SYNTENIC")
  result[["GENE_ID"]] <- NULL

  ## I provided a 2 column table of GIDs and MCL names, merge that now.
  GID <- NULL ## Stop R CMD Check
  result <- merge(ortholog_table, result, by = "GID")
  counts <- result %>%
    group_by(GID) %>%
    summarise("ORTHOLOGS_COUNT" = n())
  result <- merge(result, counts, by = "GID")

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
