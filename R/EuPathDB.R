##' @title Get started with EuPathDB
##' @return Used for its side-effect of opening the package vignette. A
##'         vector of experiment identifiers.
##' @author Keith Hughitt
##' @aliases availableEuPathDB
##' @examples availableEuPathDB
EuPathDB <- function() 
    vignette("EuPathDB",
             package = "EuPathDB")

##' @rdname EuPathDB
availableEuPathDB <- c("TriTrypDB")

ahroot <- "/var/FastRWeb/web"
BiocVersion <- as.character(BiocInstaller:::biocVersion())
