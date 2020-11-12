#' Use the post interface to get ortholog data.
#'
#' The folks at the EuPathDB kindly implemented the table 'OrthologsLite'
#' which makes it possible for me to use this function without trouble.
#'
#' @param entry The full annotation entry.
#' @param ortholog_table This should no longer be needed, it is only OrthologsLite now.
#' @param build_dir Location to which to save an intermediate savefile.
#' @param gene_ids When provided, ask only for the orthologs for these genes.
#' @param overwrite Overwrite incomplete savefiles?
#' @return A big honking table.
post_eupath_ortholog_table <- function(entry = NULL, ortholog_table = NULL, build_dir = "EuPathDB",
                                       gene_ids = NULL, overwrite = FALSE) {
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(build_dir, "rda")
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

  result <- post_eupath_table(entry, tables = "OrthologsLite", table_name = "orthologs")
  colnames(result) <- c("GID", "GENE_ID", "ORTHOLOGS_GID", "ORTHOLOGS_ORGANISM",
                        "ORTHOLOGS_PRODUCT", "ORTHOLOGS_SYNTENIC")
  result[["GENE_ID"]] <- NULL

  ## I provided a 2 column table of GIDs and MCL names, merge that now.
  GID <- NULL ## Stop R CMD Check
  new_result <- merge(ortholog_table, result, by = "GID")
  counts <- result %>%
    group_by(GID) %>%
    summarise("ORTHOLOGS_COUNT" = n())
  result <- merge(result, counts, by = "GID")

  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
