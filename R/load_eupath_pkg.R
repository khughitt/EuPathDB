#' Loads a pkg into the current R environment.
#'
#' @param name Package name.
#' @param webservice From where to get the package name.
load_eupath_pkg <- function(name, webservice = "eupathdb") {
  first_try <- try(do.call("library", as.list(name)), silent = TRUE)
  if (class(first_try) == "try-error") {
    metadata <- download_eupath_metadata(webservice = webservice)
    entry <- get_eupath_entry(name)
    pkg_names <- get_eupath_pkgnames(entry)
    first_pkg <- pkg_names[["orgdb"]]
    tt <- try(do.call("library", as.list(first_pkg)), silent = TRUE)
    if (class(tt) == "try-error") {
      message("Did not find the package: ",
              first_pkg,
              ". Will not be able to do reciprocal hits.")
      message("Perhaps try invoking make_eupath_organismdbi().")
      pkg <- NULL
    } else {
      message("Loaded: ", first_pkg)
      pkg <- get(first_pkg)
    }
    return(pkg)
  } else {
    pkg <- get(name)
    return(pkg)
  }
}  ## End internal function 'load_pkg()'
