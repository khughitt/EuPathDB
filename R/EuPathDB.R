##' @title Get started with EuPathDB
##' @return Used for its side-effect of opening the package vignette. A
##'         vector of experiment identifiers.
##' @author Keith Hughitt
##' @aliases availableEuPathDB
##' @examples availableEuPathDB
EuPathDB <- function() {
    vignette("EuPathDB", package = "EuPathDB")
}

