#' EuPathDB: Access EuPathDB annotations using AnnotationHub
#'
#' EuPathDB provides an R interface for retrieving annotation resources from
#' the EuPathDB databases: AmoebaDB, CryptoDB, FungiDB, GiardiaDB,
#' MicrosporidiaDB, PiroplasmaDB, PlasmoDB, ToxoDB, TrichDB, and TriTrypDB
#' using the Bioconductor AnnotationHub framework.
#'
#' There are currently two types of Bioconductor resources which can be
#' retrieved for 153 supported organisms from the various EuPathDB databases:
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
#' @docType package
#' @name EuPathDB
#' @seealso \code{\link{AnnotationHub}}
#' @seealso \code{\link{GRanges}}
#' @seealso  \url{http://eupathdb.org/eupathdb/}
NULL

##' @title Get started with EuPathDB
##' @return Used for its side-effect of opening the package vignette. A
##'         vector of experiment identifiers.
##' @author Keith Hughitt
##' @aliases availableEuPathDB
##' @examples availableEuPathDB
EuPathDB <- function() {
    vignette("EuPathDB", package="EuPathDB")
}

