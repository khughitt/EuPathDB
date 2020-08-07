library(crayon)

#' Info-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
info <- function(...) {
  message("[" %+% crayon::cyan("INFO") %+% "] ", ...)
}

#' Warning-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
warn <- function(...) {
  message("[" %+% crayon::magenta("WARN") %+% "] ", ...)
}

#' Error-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
error <- function(...) {
  message("[" %+% crayon::yellow("ERROR") %+% "] ", ...)
}
