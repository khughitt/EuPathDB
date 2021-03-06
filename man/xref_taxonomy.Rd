% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xref_taxonomy.R
\name{xref_taxonomy}
\alias{xref_taxonomy}
\title{Cross reference the taxonomy data from GenomeInfoDb with EuPathDB metadata.}
\usage{
xref_taxonomy(
  metadata,
  verbose = FALSE,
  species_column = "SpeciesName",
  taxon_column = "TaxonomyID"
)
}
\arguments{
\item{metadata}{Information provided by downloading the metadata from a eupathdb sub project.}

\item{verbose}{Print some information about what is found as this runs?}

\item{species_column}{Because the species column name has changed.}

\item{taxon_column}{Because the taxonomy column name has changed.}
}
\value{
List containing entries which pass and fail after xrefing against loadTaxonomyDb().
}
\description{
Previously, the logic of this function resided in download_eupath_metadata(),
but I want to be able to test and poke at it separately to more effectively
ensure as many taxa as possible pass.  Therefore, I split it into its own
function. The secondary function of this is to set the 'Species' column as
appropriately as possible.
}
\details{
The downside is that there is now yet another for loop in this codebase
iterating over the metadata.  Ideally, we should be collapsing some of these,
on the other hand it will be nice to have the metadata separated by taxa which
do and do not match GenomeInfoDb.
}
