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
xref_ah_species <- function(metadatum, ah_species, verbose = FALSE,
                            metadata_taxon_column = "TaxonUnmodified",
                            metadata_species_column = "GenusSpecies",
                            gidb_species_column = "GIDB_Genus_Species",
                            xref_column = "TaxonXref"
                            ) {

  ## In this process I am adding a new column 'TaxonXref' which is the set of
  ## Species or SpeciesStrain names that we can successfully match against the
  ## set of things in getSpeciesList().
  ## In the ideal world, this would always be identical to the taxonomy names
  ## provided by the EuPathDB.  This is not an ideal world, so some of them
  ## will get filled in with just the species names provided by the eupathdb.
  ## If we cannot match even that, then the entries will get moved into the
  ## pile of invalid entries for future examination and probably deletion.
  retlist <- list(
    "ID" = NULL,
    "status" = "unmatched")

  initial_valid <- metadatum[[metadata_taxon_column]] %in% ah_species
  if (isTRUE(initial_valid)) {
    retlist[["ID"]] <- metadatum[[metadata_taxon_column]]
    retlist[["status"]] <- "exact_taxon"
    return(retlist)
  }

  ## We have previously filled in the 'Species' column with information from GenomeInfoDb.
  ## So let us query that now and see if we pick up more valid entries.
  valid <- metadatum[[gidb_species_column]] %in% ah_species
  if (isTRUE(valid)) {
    if (isTRUE(verbose)) {
      message("Found a match between the GIDB genus/species and the AH species list.")
    }
    retlist[["ID"]] <- metadatum[[gidb_species_column]]
    retlist[["status"]] <- "exact_gidb_species"
    return(retlist)
  } else {
    retlist[["ID"]] <- NULL
    retlist[["status"]] <- "unexpected_gidb"
    return(retlist)
  }

  ## If we get here, see if AH has genus/species.
  valid <- metadatum[["GenusSpecies"]] %in% ah_species

}

#' Cross reference information from the taxonDB vs. the downloaded metadata.
#'
#' @param metadatum The downloaded metadata for an individual species.
#' @param all_taxa_ids The taxonDB result.
#' @param taxon_number_column The metadatum column which should contain the relevant number.
#' @param verbose Be a chatty catty?
xref_gidb_species <- function(metadatum, all_taxa_ids,
                              taxon_number_column = "TaxonomyID",
                              verbose = verbose) {
  id <- metadatum[[taxon_number_column]]
  retlist <- list(
    "ID" = id,
    "status" = "unmatched")
  if (is.null(id)) {
    return(retlist)
  }
  if (is.na(id)) {
    return(retlist)
  }
  if (is.na(id)) {
    return(NULL)
  }

  gs <- NULL
  found <- all_taxa_ids[["tax_id"]] == id
  if (sum(found) == 0) {
    retlist[["ID"]] <- NULL
    retlist[["status"]] <- "mismatched"
  } else if (sum(found) == 1) {
    gs <- paste0(all_taxa_ids[found, "genus"], " ", all_taxa_ids[found, "species"])
    retlist[["ID"]] <- gs
    retlist[["status"]] <- "exact_match"
  } else {
    message("This should not happen, returning the first match.")
    matched <- all_taxa_ids[found, ]
    gs <- paste0(all_taxa_ids[1, "genus"], " ", all_taxa_ids[1, "species"])
    retlist[["ID"]] <- gs
    retlist[["status"]] <- "multi_match"
  }
  return(retlist)
}
