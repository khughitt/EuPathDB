#"
#" helper functions for logging to the console
#"
library(crayon)

info <- function(...) {
  message("[" %+% cyan("INFO") %+% "] ", ...)
}

warn <- function(...) {
  message("[" %+% magenta("WARN") %+% "] ", ...)
}

error <- function(...) {
  message("[" %+% yellow("ERROR") %+% "] ", ...)
}
