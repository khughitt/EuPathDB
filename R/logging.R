#' Info-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
info <- function(...) {
  message("[",  crayon::cyan("INFO"), "] ", ...)
}

#' Warning-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
warn <- function(...) {
  message("[", crayon::magenta("WARN"), "] ", ...)
  warning(...)
}

#' Error-level logging function.
#'
#' @param ... One or more strings to be logged.
#' @export
error <- function(...) {
  message("[", crayon::yellow("ERROR"), "] ", ...)
  stop(...)
}

#' Silence
#'
#' Some libraries/functions just won't shut up.  Ergo, silence, peasant!
#' This is a simpler silence peasant.
#'
#' @param ... Some code to shut up.
#' @param wrap  Wrap the invocation and try again if it failed?
#' @return Whatever the code would have returned.
#' @export
sm <- function(..., wrap = FALSE) {
  ret <- NULL
  output <- capture.output(type = "output", {
    if (isTRUE(wrap)) {
      ret <- try(suppressWarnings(suppressMessages(...)), silent = TRUE)
    } else {
      ret <- suppressWarnings(suppressMessages(...))
    }
  })
  return(ret)
}
