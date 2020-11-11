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
#' @param verbose Print some information about what is found?
#' @param taxon_column metadata column with the taxonomy information.
#' @param species_column metadata column with my generated species name.
#' @return Likely smaller data frame of valid information and larger dataframe of invalid.
xref_species <- function(valid, invalid, verbose=FALSE,
                         taxon_column = "TaxonUnmodified", species_column = "GenusSpecies") {
  testing_metadata <- valid
  valid_metadata <- data.frame()
  invalid_metadata <- data.frame()
  annotationhub_species <- AnnotationHubData::getSpeciesList()
  valid_idx <- testing_metadata[[taxon_column]] %in% annotationhub_species

  ## In this process I am adding a new column 'TaxonXref' which is the set of
  ## Species or SpeciesStrain names that we can successfully match against the
  ## set of things in getSpeciesList().
  ## In the ideal world, this would always be identical to the taxonomy names
  ## provided by the EuPathDB.  This is not an ideal world, so some of them
  ## will get filled in with just the species names provided by the eupathdb.
  ## If we cannot match even that, then the entries will get moved into the
  ## pile of invalid entries for future examination and probably deletion.

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
    valid_metadata[["TaxonXref"]] <- valid_metadata[["TaxonUnmodified"]]
  }

  ## The remainder of testing_metadata needs to be queried further.
  if (isTRUE(verbose)) {
    message("Now there are: ", nrow(testing_metadata), " rows left.")
  }
  ## We have previously filled in the 'Species' column with information from GenomeInfoDb.
  ## So let us query that now and see if we pick up more valid entries.
  valid_idx <- testing_metadata[[species_column]] %in% annotationhub_species
  if (isTRUE(verbose)) {
    message("Adding ", sum(valid_idx), " species after using GenomeInfoDb against ",
            species_column, ".")
  }
  if (sum(valid_idx) > 0) {
    ## Pull out the new valid entries
    new_metadata <- testing_metadata[which(valid_idx), ]
    new_metadata[["TaxonXref"]] <- new_metadata[[species_column]]
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
  valid_idx <- valid_metadata[[species_column]] %in% annotationhub_species
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
