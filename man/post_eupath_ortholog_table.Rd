% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_eupath_ortholog.R
\name{post_eupath_ortholog_table}
\alias{post_eupath_ortholog_table}
\title{Use the post interface to get ortholog data.}
\usage{
post_eupath_ortholog_table(
  entry = NULL,
  ortholog_table = NULL,
  build_dir = "EuPathDB",
  gene_ids = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{entry}{The full annotation entry.}

\item{ortholog_table}{This should no longer be needed, it is only OrthologsLite now.}

\item{build_dir}{Location to which to save an intermediate savefile.}

\item{gene_ids}{When provided, ask only for the orthologs for these genes.}

\item{overwrite}{Overwrite incomplete savefiles?}
}
\value{
A big honking table.
}
\description{
The folks at the EuPathDB kindly implemented the table 'OrthologsLite'
which makes it possible for me to use this function without trouble.
}
