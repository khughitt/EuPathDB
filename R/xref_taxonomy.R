

## FIXME: This is too damn complicated, the taxonomy IDs shouldn't be this difficult to match up.


#' Cross reference the taxonomy data from GenomeInfoDb with EuPathDB metadata.
#'
#' Previously this tried to set species and taxonomy number, but I am
#' having it only do the number now in the hopes that it simplifies
#' things somewhat.
#'
#' @param metadata Information provided by downloading the metadata from a eupathdb sub project.
#' @param verbose Print some information about what is found as this runs?
#' @param species_column Because the species column name has changed.
#' @param taxon_column Because the taxonomy column name has changed.
#' @return List containing entries which pass and fail after xrefing against loadTaxonomyDb().
xref_taxonomy_number <- function(metadatum, all_taxa_ids, verbose = FALSE,
                                 taxon_number_column = "TaxonomyID"
                                 metadata_taxon_column = "TaxonUnmodified") {
  ## If we don't have a taxonomy number, iterate over the species names
  ## To try and find a suitable match
  chosen_id <- NULL
  if (is.na(metadatum[metadata_taxon_column])) {
    chosen_id <- search_na_taxon(metadatum, all_taxa_ids,
                                 taxon_number_column = taxon_number_column,
                                 metadata_taxon_column = metadata_taxon_column)
    } else {
      ## Now let us check the cases when the eupathdb
      ## _does_ provide a taxonomy ID number.
      id_idx <- all_taxa_ids[["tax_id"]] == metadatum[taxon_number_column]
      if (sum(id_idx) == 0) {
        ## No taxonomy ID was found, not sure yet what to do here.
        ## We will want to search the taxonomy ID using genus/species.
        if (isTRUE(verbose)) {
          message("Did not match a taxonomy ID when one was provided by the eupathdb!")
          message("Trying to match based on taxonomy names.")
        }
        chosen_id <- search_na_taxon(metadatum, all_taxa_ids,
                                     taxon_number_column = taxon_number_column,
                                     metadata_taxon_column = metadata_taxon_column)
      } else if (sum(id_idx) == 1) {
        ## There is a successful match, do nothing.
        chosen_id <- metadatum[taxon_number_column]
        if (isTRUE(verbose)) {
          message("Successful single match for metadata: ", metadatum[metadata_taxon_column], ".")
        }
      }
    } else {
        id_gs <- glue::glue("{all_taxa_ids[id_idx, 'genus']} {all_taxa_ids[id_idx, 'species']}")
        if (isTRUE(verbose)) {
          message("More than 1 match for metadata: ", metadata[it, metadata_taxon_column], "\n",
                  "             vs. genomeinfodb: ", id_gs)
          message("Setting TaxonCanonical to the first match.")
        }
        metadatum["TaxonCanonical"] <- id_gs[1]
        matched_idx <- c(matched_idx, it)
      }
    } ## End the if() querying if there is an NA in the taxonomy ID.
    ## Put the row back into the metadata with the new taxonomy ID information.
    metadata[it, ] <- metadatum
  } ## End of the for loop.

  ## At this point, the lengths of my matched idx and unmatched idx should be
  ## equal to the number of entries in the original metadata.
  total_idx <- length(matched_idx) + length(unmatched_idx)
  ## testthat::expect_equal(nrow(metadata), total_idx)
  matched_metadata <- metadata[matched_idx, ]
  unmatched_metadata <- metadata[unmatched_idx, ]
  retlist <- list(
    "matched_metadata" = matched_metadata,
    "unmatched_metadata" = unmatched_metadata)
  return(retlist)
}


#' Try harder to fill in the taxonomy ID number.
search_na_taxon <- function(metadatum, all_taxa_ids, metadata_taxon_column = "TaxonUnmodified",
                            taxon_number_column = "TaxonomyID") {
  ## Calling make_taxon_names() again because we don't save all the
  ## combinations of species/strain in the metadata
  species_info <- make_taxon_names(metadatum, column = metadata_taxon_column, spaces = "space")
  starting_taxon <- species_info["unmodified"]
  taxon_id <- NULL

  ## First identify genera in all_taxa_ids which are shared with this entry.
  found_genus_taxa_idx <- which(all_taxa_ids[["genus"]] %in% metadatum[["Genus"]])
  if (length(found_genus_taxa_idx) > 0) {
    ## Assuming we got more than 1 hit, narrow the search to species which match.
    ## Note that some organisms in tha taxonomy database have species set to what
    ## the eupathdb appears to call the combination of species+strain
    ## while others are just the species.
    ## Thus we should check for both, but for the moment I think I will check for
    ## only the most specific of the two and see how far that gets me.
    genus_taxa <- all_taxa_ids[found_genus_taxa_idx, ]
    found_species_strain_taxa <- which(
      all_taxa_ids[["genus"]] %in% metadatum[["Genus"]] &
        all_taxa_ids[["species"]] %in% metadatum[["species_strain"]])
    found_species_taxa <- which(
      all_taxa_ids[["genus"]] %in% metadatum[["Genus"]] &
        all_taxa_ids[["species"]] %in% metadatum[["species"]])
    species_strain_taxa <- all_taxa_ids[found_species_strain_taxa, ]
    species_taxa <- all_taxa_ids[found_species_taxa, ]

    ## If we still have more than 1 hit, I will arbitrarily choose the first.
    if (length(found_species_strain_taxa) == 1) {
      taxon_id <- species_strain_taxa["tax_id"]
      if (isTRUE(verbose)) {
        message("Found an exact match for the combination genus/species/strain.")
      }
    } else if (length(found_species_strain_taxa) == 0 &&
                 length(found_species_taxa) > 0) {
      if (length(found_species_taxa) == 1) {
        taxon_id <- species_taxa["tax_id"]
        if (isTRUE(verbose)) {
          message("Found an exact match for the combination genus/species not strain.")
        }
      } else if (length(found_species_taxa) > 1) {
        taxon_id <- species_taxa[1, "tax_id"]
        if (isTRUE(verbose)) {
          message("Found multiple species matches for ", starting_taxon,
                  ", chose the first: ", taxon_id, ".")
        }
      } ## End multiple species matches
    } else {
      if (isTRUE(verbose)) {
        message("Found a genus, but not species for ", starting_taxon,
                ", not adding taxon ID number.")
      }
    }
  } else {
    if (isTRUE(verbose)) {
      message("Did not find a genus id for ", starting_taxon, ".")
    }
  }
  return(taxon_id)
}
