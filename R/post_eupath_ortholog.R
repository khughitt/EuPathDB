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
post_eupath_ortholog_table <- function(entry = NULL, ortholog_table = NULL,
                                       gene_ids = NULL, overwrite = FALSE, verbose = TRUE) {

  rda <- check_rda("ortholog", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning ortholog data from a previous savefile.")
    }
    return(rda[["result"]])
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
