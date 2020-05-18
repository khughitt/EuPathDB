#' Search the eupathdb metadata for a given species substring.
#'
#' When querying the eupathdb, it can be difficult to hit the desired species.
#' This is confounded by the fact that there are very similar named species
#' across different EupathDB projects.  Thus function seeks to make it a bit
#' easier to find the actual dataset desired.  If the specific species is not
#' found, look for a reasonable approximation. stop() if nothing is found.
#'
#' @param species String containing some reasonably unique text in the desired
#'   species name.
#' @param webservice The EuPathDB webservice to query.
#' @param column Which column to use for getting the species name?
#' @param metadata Optional dataframe of already downloaded metadata.
#' @param ... Parameters passed to download_eupath_metadata()
#' @return  A single row from the eupathdb metadata.
#' @author atb
#' @export
get_eupath_entry <- function(species="Leishmania major", webservice="eupathdb",
                             column="TaxonUnmodified", metadata=NULL, ...) {
  if (is.null(metadata)) {
    metadata <- download_eupath_metadata(webservice=webservice, ...)
  }
  valid_metadata <- metadata[["valid"]]
  all_species <- valid_metadata[[column]]
  entry <- NULL
  grep_hits <- grepl(species, all_species)
  grepped_hits <- all_species[grep_hits]
  found_species <- sum(grep_hits)
  if (found_species == 0) {
    message("Here are the possible species: ", toString(all_species))
    stop("Did not find your species.")
  } else if (found_species == 1) {
    hit_idx <- valid_metadata[[column]] == grepped_hits
    entry <- valid_metadata[hit_idx, ]
    message("Found: ", entry[[column]])
  } else if (found_species > 1) {
    species <- grepped_hits[[1]]
    hit_idx <- valid_metadata[[column]] == species
    entry <- valid_metadata[hit_idx, ]
    message("Found the following hits: ", toString(grepped_hits), ", choosing the first.")
    message("Using: ", entry[1, column], ".")
  } else {
    stop("It should not be possible to have negative hits.")
  }
  return(entry)
}

## EOF
