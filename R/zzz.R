.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        sprintf("\nThis is EuPathDB version %s\n Read 'EuPathDB()' to get started.\n",
                packageVersion("EuPathDB")))

    if (interactive() && .Platform$OS.type == "windows" && .Platform$GUI == "Rgui") {
        addVigs2WinMenu("EuPathDB")
    }
}

#' EupathDb: Some tools to create orgdb packages from the eupathdb.
#'
#' It should make them suitable for annotationhub, but I have not yet figured
#' out how Keith has linked them.
#'
#' @docType package
#' @name EupathDb
#' @importFrom data.table data.table
#' @importFrom dplyr filter group_by n summarise
#' @importFrom foreach foreach
#' @importFrom glue glue glue_data
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
