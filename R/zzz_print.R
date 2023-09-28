#' Print a representation of some downloaded metadata.
#'
#' @param x The metadata!
#' @export
print.downloaded_metadata <- function(x) {
  message_string <- glue("Metadata downloaded from {x[['webservice']]} comprising
{nrow(x[['valid']])} valid entries and {nrow(x[['invalid']])} entries which could
not be cross referenced to the genomeinfodb and/or the taxonomy db.

The invalid entries are:")
  message(message_string)
  message(x[["invalid"]][["TaxonUnmodified"]])
  return(invisible(x))
}

#' Print a representation of a created eupath orgdb instance.
#'
#' @param x Orgdb dataset
#' @export
print.eupath_orgdb <- function(x) {
  message_string <- glue("The built package is named {x[['pkgname']]},
the sqlite file is at {x[['db_path']], and the tar is at
{x[['package_file']]}.")
  message(message_string)
  return(invisible(x))
}
