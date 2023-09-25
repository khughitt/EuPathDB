#' Use the POST interface to get GO data from the EuPathDB.
#'
#' @param entry The full annotation entry.
#' @param build_dir Location to write savefiles.
#' @param overwrite Overwrite intermediate savefiles in case of incomplete install?
#' @return A big honking table.
post_eupath_go_table <- function(entry, working_species, overwrite = FALSE, verbose = FALSE) {
  rda <- check_rda("go", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning GO data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  result <- post_eupath_table(entry, working_species, tables = "GOTerms", table_name = "godb")
  ## 202210: There appears to be a problem in the current GOTerms table, it has
  ## a new EOF in the middle which leads to the GO IDs to get scrambled.
  ## The first instance of the GO table getting scrambled is at row 159,780.
  ## At that row, the GODB_EVIDENCE_CODE gets scrambled to 5.8S rRNA.

  ## It appears that a good work around is to use read.csv() with the argument
  ## 'quote = ""' -- with the caveat that doing so leads to some strangeness in
  ## the returned column names.
  ## It also appears that readr::read_csv can get around this?
  colnames(result) <- gsub(x = colnames(result), pattern = "^GODB_", replacement = "GO_")
  colnames(result) <- gsub(x = colnames(result), pattern = "^GO_GO_", replacement = "GO_")
  message("  Saving ", savefile, " with ", nrow(result), " rows.")
  save(result, file = savefile)
  return(result)
}
