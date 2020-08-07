#' Use the post interface to get ortholog data.
#'
#' The folks at the EuPathDB kindly implemented the table 'OrthologsLite'
#' which makes it possible for me to use this function without trouble.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to which to save an intermediate savefile.
#' @param table This defaults to the 'OrthologsLite' table, but that does not
#'  exist at all eupathdb subprojects.
#' @param gene_ids When provided, ask only for the orthologs for these genes.
#' @param overwrite Overwrite incomplete savefiles?
#' @return A big honking table.
<<<<<<< HEAD
<<<<<<< HEAD:R/post_eupath_ortholog.R
post_eupath_ortholog_table <- function(entry = NULL, ortholog_table = NULL, build_dir = "EuPathDB",
                                       gene_ids = NULL, overwrite = FALSE) {
=======
post_eupathdb_ortholog_table <- function(entry=NULL, workdir="EuPathDB", table="OrthologsLite",
                                       gene_ids=NULL, overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_ortholog.R
=======
post_eupathdb_ortholog_table <- function(entry = NULL, workdir = "EuPathDB", 
                                         table = "OrthologsLite",
                                         gene_ids = NULL, overwrite = FALSE) {
>>>>>>> e0e10d7 (Improvements to logging; few fixes related to previous refactoring)
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
<<<<<<< HEAD
    created <- dir.create(rdadir, recursive = TRUE)
=======
    dir.create(rdadir, recursive = TRUE)
>>>>>>> e0e10d7 (Improvements to logging; few fixes related to previous refactoring)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      file.remove(savefile)
    } else {
      info(savefile, " already exists! Delete this file if you wish to regenerate it.")
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

<<<<<<< HEAD
<<<<<<< HEAD:R/post_eupath_ortholog.R
  ## I provided a 2 column table of GIDs and MCL names, merge that now.
  GID <- NULL ## Stop R CMD Check
  new_result <- merge(ortholog_table, result, by = "GID")
  counts <- result %>%
    group_by(GID) %>%
    summarise("ORTHOLOGS_COUNT" = n())
  result <- merge(result, counts, by = "GID")
=======
  result <- post_eupathdb_table(query_body, entry, table_name="orthologs")
=======
  result <- post_eupathdb_table(query_body, entry, table_name = "orthologs")

>>>>>>> e0e10d7 (Improvements to logging; few fixes related to previous refactoring)
  if (nrow(result) == 0) {
    warn("Failed to download the OrthologsLite table for: ", entry$Taxon)
    warn("Attempting to download 1 gene at a time.")
    result <- get_orthologs_all_genes(entry, workdir = workdir,
                                      gene_ids = gene_ids, overwrite = overwrite)
  } else {
    colnames(result) <- c("GID", "GENE_ID", "ORTHOLOGS_GID", "ORTHOLOGS_ORGANISM",
                          "ORTHOLOGS_PRODUCT", "ORTHOLOGS_SYNTENIC")
    ## The GENE_ID column is redundant, drop it.
    result[["GENE_ID"]] <- NULL
  }
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_ortholog.R

<<<<<<< HEAD
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
=======
  message("  Saving ", savefile)
>>>>>>> e0e10d7 (Improvements to logging; few fixes related to previous refactoring)
  save(result, file = savefile)
  return(result)
}
