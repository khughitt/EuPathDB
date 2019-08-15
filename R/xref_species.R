#' Cross reference the taxonomy data from AnnotationHubData::getSpeciesList()
#'
#' Previously, the logic of this function resided in download_eupath_metadata(),
#' but I want to be able to test and poke at it separately to more effectively
#' ensure as many taxa as possible pass.  Therefore, I split it into its own
#' function. The secondary function of this is to set the 'Species' column as
#' appropriately as possible.
#'
#' @param valid Dataframe of entries which have thus far been deemed 'valid' by my tests.
#' @param invalid Dataframe of entries which failed.
#' @return Likely smaller data frame of valid information and larger dataframe of invalid.
xref_species <- function(valid, invalid, verbose=FALSE) {
  testing_metadata <- valid
  valid_metadata <- data.frame()
  invalid_metadata <- data.frame()
  annotationhub_species <- AnnotationHubData::getSpeciesList()
  valid_idx <- testing_metadata[["TaxonUnmodified"]] %in% annotationhub_species

  if (isTRUE(verbose)) {
    message("Adding ", sum(valid_idx), " species without changing anything out of ",
            nrow(testing_metadata), ".")
  }
  if (sum(valid_idx) > 0) {
    ## Add the stuff which was found to the set of valid entries.
    valid_metadata <- testing_metadata[which(valid_idx), ]
    ## Remove them from the set to be tested.
    testing_metadata <- testing_metadata[which(!valid_idx), ]
    ## Set the 'Species' column to taxonunmodified
    valid_metadata[["Species"]] <- valid_metadata[["TaxonUnmodified"]]
  }

  ## The remainder of testing_metadata needs to be queried further.
  if (isTRUE(verbose)) {
    message("Now there are: ", nrow(testing_metadata), " rows left.")
  }
  ## We have previously filled in the 'Species' column with information from GenomeInfoDb.
  ## So let us query that now and see if we pick up more valid entries.
  valid_idx <- testing_metadata[["Species"]] %in% annotationhub_species
  if (isTRUE(verbose)) {
    message("Added ", sum(valid_idx), " species after using GenomeInfoDb.")
  }
  if (sum(valid_idx) > 0) {
    ## Pull out the new valid entries
    new_metadata <- testing_metadata[which(valid_idx), ]
    ## Add those to the valid metadata.
    valid_metadata <- rbind(valid_metadata, new_metadata)
    ## Add whatever is left to the set of invalid metadata.
    invalid_metadata <- testing_metadata[which(!valid_idx), ]
  } else {
    invalid_metadata <- testing_metadata
  }
  if (nrow(invalid_metadata) > 0) {
    message("Unable to find species names for ", nrow(invalid_metadata), " species.")
    message(toString(invalid_metadata[["Species"]]))
  }

  ## Now do a final check to see what is up with some weirdos which somehow snuck through.
  valid_idx <- valid_metadata[["Species"]] %in% annotationhub_species
  ## I am going to get a little crazy here.
  invalid_idx <- ! valid_idx
  if (sum(invalid_idx) > 0) {
    warning("How in the flying hell are these still here: ",
            toString(valid_metadata[invalid_idx, "Species"]), ".")
  }
  valid_metadata <- valid_metadata[valid_idx, ]
  invalid_metadata <- rbind(invalid, invalid_metadata)

  retlist <- list(
    "valid" = valid_metadata,
    "invalid" = invalid_metadata)
  return(retlist)
}
