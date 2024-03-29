% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xref_species.R
\name{xref_ah_species}
\alias{xref_ah_species}
\title{Cross reference the taxonomy data from AnnotationHubData::getSpeciesList()}
\usage{
xref_ah_species(
  metadatum,
  ah_species,
  verbose = FALSE,
  metadata_taxon_column = "TaxonUnmodified",
  metadata_species_column = "GenusSpecies",
  gidb_species_column = "GIDB_Genus_Species",
  xref_column = "TaxonXref"
)
}
\arguments{
\item{verbose}{Print some information about what is found?}

\item{valid}{Dataframe of entries which have thus far been deemed 'valid' by my tests.}

\item{invalid}{Dataframe of entries which failed.}

\item{taxon_column}{metadata column with the taxonomy information.}

\item{species_column}{metadata column with my generated species name.}
}
\value{
Likely smaller data frame of valid information and larger dataframe of invalid.
}
\description{
Previously, the logic of this function resided in download_eupath_metadata(),
but I want to be able to test and poke at it separately to more effectively
ensure as many taxa as possible pass.  Therefore, I split it into its own
function. The secondary function of this is to set the 'Species' column as
appropriately as possible.
}
