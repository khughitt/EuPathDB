#' Convert a potentially non-unique vector from kegg into a normalized data
#' frame.
#'
#' This function seeks to reformat data from KEGGREST into something which is
#' rather easier to use.
#'
#' This could probably benefit from a tidyr-ish revisitation.
#'
#' @param vector  Information from KEGGREST
#' @param final_colname  Column name for the new information
#' @param flatten Flatten nested data?
#' @return  A normalized data frame of gene IDs to whatever.
#' @author atb
kegg_vector_to_df <- function(vector, final_colname="first", flatten=TRUE) {
  final_df <- data.frame(stringsAsFactors=FALSE)
  if (isTRUE(flatten)) {
    sorted <- order(names(vector))
    sorted_vector <- vector[sorted]
    sorted_names <- names(sorted_vector)
    duplicated_names <- duplicated(sorted_names)
    unique_vector <- sorted_vector[!duplicated_names]
    unique_df <- as.data.frame(unique_vector, stringsAsFactors=FALSE)
    colnames(unique_df) <- final_colname
    ## Next line should not be needed with stringsAsFactors=FALSE
    ##unique_df[[final_colname]] <- as.character(unique_df[[final_colname]])
    duplicated_vector <- sorted_vector[duplicated_names]
    ## Now append the duplicated entries to the existing data frame.
    if (length(duplicated_vector) > 0) {
      for (c in 1:length(duplicated_vector)) {
        append_name <- names(duplicated_vector)[c]
        append_entry <- as.character(duplicated_vector[c])
        unique_df[append_name, final_colname] <- glue::glue(
          "{unique_df[append_name, final_colname]}, {append_entry}")
      }
    }
    final_df <- unique_df
    rm(unique_df)
    final_df[["GID"]] <- rownames(final_df)
    colnames(final_df) <- c(final_colname, "GID")
  } else {
    final_df <- as.data.frame(vector, stringsAsFactors=FALSE)
    final_df[["GID"]] <- names(vector)
    colnames(final_df) <- c(final_colname, "GID")
    na_set <- is.na(final_df)
    final_df[na_set] <- ""
  }
  final_df[["GID"]] <- gsub(pattern="^.*:", replacement="", x=final_df[["GID"]], perl=TRUE)
  return(final_df)
}
