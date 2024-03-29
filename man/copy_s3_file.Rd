% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_s3_file.R
\name{copy_s3_file}
\alias{copy_s3_file}
\title{Copy the relevant file for each data type into a place which is easy for pickup by s3.}
\usage{
copy_s3_file(src_dir, s3_file, type = "bsgenome", move = FALSE)
}
\arguments{
\item{src_dir}{Source directory for the package top be copied.}

\item{s3_file}{Where is the final file to be located?}

\item{type}{Which type of package is this?}
}
\description{
Copy the relevant file for each data type into a place which is easy for pickup by s3.
}
