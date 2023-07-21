#' Set the build directory when creating packages and downloading data.
#' @export
build_dir <- "build"

<<<<<<< HEAD
=======
#' Boolean to see if packages should be built by devtools.
#' @export
build <- TRUE

>>>>>>> refs/remotes/origin/master
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

#' R CMD check is super annoying about :::.
#'
#' In a fit of pique, I did a google search to see if anyone else has been
#' annoyed in the same way as I.  I was not surprised to see that Yihui
#' Xie was, and in his email to r-devel in 2013 he proposed a game of
#' hide-and-seek; a game which I am repeating here.
#'
#' This just implements ::: as an infix operator that will not trip check.
#'
#' @param pkg on the left hand side
#' @param fun on the right hand side
`%:::%` <- function(pkg, fun) {
  get(fun, envir = asNamespace(pkg), inherits = FALSE)
}

getMaintainer <- "GenomicFeatures" %:::% ".getMaintainer"
getMetaDataValue <- "GenomicFeatures" %:::% ".getMetaDataValue"
getTxDbVersion <- "GenomicFeatures" %:::% ".getTxDbVersion"
normAuthor <- "GenomicFeatures" %:::% ".normAuthor"

## EOF
