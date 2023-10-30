#' Get rid of spurious NA entries in a table from the eupathdb.
#'
#' If these are not removed, creating the sqlite will fail.
#'
#' @param table dataframe of SQLite-bound data
#' @param name column prefix, just used for printing for the moment.
remove_eupath_nas <- function(table, name = "annot", verbose = FALSE) {
  ## At this point, there should be no NA values in the gene_table, there is
  ## logic in post_eupath_annotations() which should preclude this possibility,
  ## however this has been proven untrue.
  ## Therefore, I will here set any remaining NAs to either 0 or "" depending on
  ## cast.
  gene_cols <- colnames(table)
  for (col in 1:length(gene_cols)) {
    na_idx <- is.na(table[[col]])
    na_sum <- sum(na_idx)
    if (na_sum > 0) {
      column_class <- class(table[[col]])[1]
      if (isTRUE(verbose)) {
        message("    I found ", na_sum, " NAs in the ", gene_cols[col],
                " column of type ", column_class, " from the table: ", name,
                " table, removing them now.")
      }
      if (column_class == "character") {
        table[na_idx, col] <- ""
      } else if (column_class == "factor") {
        ##table[na_idx, col] <- 0
        table[[col]] <- forcats::fct_na_value_to_level(table[[col]], "")
      } else if (column_class == "numeric") {
        table[na_idx, col] <- 0
      } else {
        ## There should only really be characters, factors, and numbers...
        table[na_idx, col] <- 0
      }
    }
  }
  return(table)
}
