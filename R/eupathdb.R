#' EuPathDB: Access EuPathDB annotations using AnnotationHub
#'
#' EuPathDB provides an R interface for retrieving annotation resources from
#' the EuPathDB databases: AmoebaDB, CryptoDB, FungiDB, GiardiaDB,
#' MicrosporidiaDB, PiroplasmaDB, PlasmoDB, ToxoDB, TrichDB, and TriTrypDB
#' using the Bioconductor AnnotationHub framework.
#'
#' There are currently two types of Bioconductor resources which can be
#' retrieved for 194 supported organisms from the various EuPathDB databases:
#'
#' \itemize{
#' \item OrgDB resources
#' \item GRanges resources
#' }
#'
#' The OrgDB resources provides gene level information including chromosome,
#' location, name, description, orthologs, and associated GO terms.
#'
#' The GRanges resources provide transcript-level information such as known
#' exons and their corresponding locations.
#'
#' Each of these resources are generated using information obtained from the
#' EuPathDB GFF files along with queries made through the various EuPathDB web
#' APIs.
#'
#' For examples of how EuPathDB can be used to query and interact with
#' EuPathDB.org resources, take a look at the vignette:
#' \code{browseVignettes(package="EuPathDB")}
#'
#' Use \code{availableEuPathDB()} to get a vector of available organisms.
#'
#' @docType package
#' @name EuPathDB
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @import GenomeInfoDbData
#' @import rvest
#' @import xml2
#' @import utils
#' @importFrom data.table data.table
#' @importFrom dplyr filter group_by n summarise
#' @importFrom foreach foreach
#' @importFrom glue glue glue_data
#' @seealso \code{\link{AnnotationHub}}
#' @seealso \code{\link{GRanges}}
#' @seealso  \url{http://eupathdb.org/eupathdb/}
#' @author Keith Hughitt and Ashton Belew
NULL

#' @title Get started with EuPathDB
#' @return Used for its side-effect of opening the package vignette. A
#'         vector of experiment identifiers.
#' @author Keith Hughitt
#' @aliases availableEuPathDB
#' @examples start_eupathdb()
#' @export
start_eupathdb <- function() {
  ## I renamed this function to handle an R check where it looks for man pages with mismatched case
  ## with respect to the functions within it.  There is a roxygen clue for EuPathDb, so having
  ## a function with the same name confuses R check.
  utils::vignette("EuPathDB", package="EuPathDB")
  metadata_file <- system.file("extdata/granges_metadata.csv", package="EuPathDB")
  ret <- sort(read.csv(metadata_file,
                       stringsAsFactors = FALSE)[["SpeciesFull"]])
  return(ret)
}

#' Pipe operator
#'
#' Shamelessly scabbed from Hadley: https://github.com/sckott/analogsea/issues/32
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' dopar
#'
#' Shamelessly scabbed from Hadley: https://github.com/sckott/analogsea/issues/32
#'
#' @name %dopar%
#' @rdname dopar
#' @keywords internal
#' @export
#' @importFrom foreach %dopar%
NULL
