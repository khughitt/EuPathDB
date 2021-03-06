% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_s3_file.R
\name{query_s3_granges}
\alias{query_s3_granges}
\title{Perform what should be a completely silly final check on the file which is to be copied to s3.}
\usage{
query_s3_granges(file)
}
\arguments{
\item{file}{Filename to query.}
}
\value{
MD5 sum of the file or NULL.
}
\description{
This function really should not be needed.  But damn.  This will do a final check that the
data in the s3 staging directory is loadable in R and return the md5 sum of the file.
Thus the md5 sum may be added to the metadata.
}
