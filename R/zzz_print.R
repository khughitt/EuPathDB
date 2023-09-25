#' Print a representation of some downloaded metadata.
#'
#' @param x The metadata!
#' @export
print.downloaded_metadata <- function(x) {
  message_string <- glue("Metadata downloaded from x[['webservice']] comprising
{nrow(x[['valid']])} valid entries and {nrow(x[['invalid']])} entries which could
not be cross referenced to the genomeinfodb and/or the taxonomy db.

The invalid entries are:")
  message(message_string)
  message(x[["invalid"]][["TaxonUnmodified"]])
}
