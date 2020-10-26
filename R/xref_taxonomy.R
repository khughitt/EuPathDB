#' Cross reference the taxonomy data from GenomeInfoDb with EuPathDB metadata.
#'
#' Previously, the logic of this function resided in download_eupath_metadata(),
#' but I want to be able to test and poke at it separately to more effectively
#' ensure as many taxa as possible pass.  Therefore, I split it into its own
#' function. The secondary function of this is to set the 'Species' column as
#' appropriately as possible.
#'
#' The downside is that there is now yet another for loop in this codebase
#' iterating over the metadata.  Ideally, we should be collapsing some of these,
#' on the other hand it will be nice to have the metadata separated by taxa which
#' do and do not match GenomeInfoDb.
#'
#' @param metadata Information provided by downloading the metadata from a eupathdb sub project.
#' @param verbose Print some information about what is found as this runs?
#' @return List containing entries which pass and fail after xrefing against loadTaxonomyDb().
xref_taxonomy <- function(metadata, verbose = FALSE,
                          species_column = "SpeciesName", taxon_column = "TaxonomyId") {
  all_taxa_ids <- GenomeInfoDb::loadTaxonomyDb()
  matched_idx <- c()
  unmatched_idx <- c()

  for (it in 1:nrow(metadata)) {
    metadatum <- metadata[it, ]
    species_info <- make_taxon_names(metadatum, column = species_column)
    if (is.na(metadata[it, "TaxonomyId"])) {
      ## First identify genera in all_taxa_ids which are shared with this entry.
      found_genus_taxa_idx <- which(all_taxa_ids[["genus"]] %in% species_info[["genus"]])
      if (length(found_genus_taxa_idx) > 0) {
        ## Assuming we got more than 1 hit, narrow the search to species which match.
        subset_taxa <- all_taxa_ids[found_genus_taxa_idx, ]
        found_species_taxa_idx <- which(subset_taxa[["species"]] %in% species_info[["species"]])
        ## If we still have more than 1 hit, I will arbitrarily choose the first.
        if (length(found_species_taxa_idx) > 0) {
          taxa_ids <- subset_taxa[found_species_taxa_idx, ]
          taxon_id <- taxa_ids[1, "tax_id"]
          if (isTRUE(verbose)) {
            message("Setting the taxonomy id from GenomeInfoDb for ", metadata[it, species_column], ".")
          }
          metadata[it, "TaxonomyId"] <- taxon_id
          matched_idx <- c(matched_idx, it)
        } else {
          if (isTRUE(verbose)) {
            message("Did not find a taxonomy id for ",
                    metadata[it, species_column], ".")
          }
          unmatched_idx <- c(unmatched_idx, it)
        }
      } else {
        if (isTRUE(verbose)) {
          message("Did not find a genus id for ", metadata[it, species_column], ".")
        }
        unmatched_idx <- c(unmatched_idx, it)
      }
    } else {
      ## Now let us check the cases when the eupathdb _does_provide a taxonomy ID.
      id_idx <- all_taxa_ids[["tax_id"]] == metadata[it, taxon_column]
      if (sum(id_idx) == 0) {
        ## No taxonomy ID was found, not sure yet what to do here.
        ## We will want to search the taxonomy ID using genus/species.
        if (isTRUE(verbose)) {
          message("Did not match a taxonomy ID when one was provided by the eupathdb!")
        }
        unmatched_idx <- c(unmatched_idx, it)
      } else if (sum(id_idx) == 1) {
        ## There is a successful match, do nothing.
        id_gs <- glue::glue("{all_taxa_ids[id_idx, 'genus']} {all_taxa_ids[id_idx, 'species']}")
        if (isTRUE(verbose)) {
          message("Successful match for metadata: ", metadata[it, "TaxonUnmodified"], "\n",
                  "             vs. genomeinfodb: ", id_gs)
        }
        ## metadata[it, "Species"] <- id_gs
        matched_idx <- c(matched_idx, it)
      } else {
        id_gs <- glue::glue("{all_taxa_ids[id_idx, 'genus']} {all_taxa_ids[id_idx, 'species']}")
        if (isTRUE(verbose)) {
          message("More than 1 match for metadata: ", metadata[it, "TaxonUnmodified"], "\n",
                  "             vs. genomeinfodb: ", id_gs)
        }
        metadata[it, species_column] <- id_gs[1]
        matched_idx <- c(matched_idx, it)
      }
    } ## End the if() querying if there is an NA in the taxonomy ID.
  } ## End of the for loop.

  ## At this point, the lengths of my matched idx and unmatched idx should be
  ## equal to the number of entries in the original metadata.
  total_idx <- length(matched_idx) + length(unmatched_idx)
  testthat::expect_equal(nrow(metadata), total_idx)
  matched_metadata <- metadata[matched_idx, ]
  unmatched_metadata <- metadata[unmatched_idx, ]
  retlist <- list(
    "matched_metadata" = matched_metadata,
    "unmatched_metadata" = unmatched_metadata)
  return(retlist)
}
