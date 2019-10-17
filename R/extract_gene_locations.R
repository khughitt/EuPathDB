#' Clean up the gene location field from eupathdb derived gene location data.
#'
#' The eupathdb encodes its location data for genes in a somewhat peculiar
#' format: chromosome:start..end(strand), but I would prefer to have these
#' snippets of information as separate columns so that I can do things like
#' trivially perform rpkm().
#'
#' @param annot_df Data frame resulting from load_orgdb_annotations()
#' @param location_column Name of the column to extract the start/end/length/etc from.
#' @return Somewhat nicer data frame.
#' @author atb
#' @export
extract_gene_locations <- function(annot_df, location_column="annot_gene_location_text") {
  newdf <- annot_df %>%
    tidyr::separate(location_column,
                    c("chromosome", "location"), ":")
  newdf <- newdf %>%
    tidyr::separate("location", c("start", "end"), "\\.\\.")
  newdf[["start"]] <- as.numeric(gsub(pattern="\\,", replacement="", x=newdf[["start"]]))
  newdf <- newdf %>%
    tidyr::separate("end", c("end", "strand"), "\\(")
  newdf[["end"]] <- as.numeric(gsub(pattern="\\,", replacement="", x=newdf[["end"]]))
  newdf[["strand"]] <- as.factor(gsub(pattern="\\)", replacement="", x=newdf[["strand"]]))
  newdf[["length"]] <- abs(newdf[["start"]] - newdf[["end"]])
  return(newdf)
}
