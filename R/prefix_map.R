#' A few webservices at the eupathdb are not what one would expect.
#'
#' This maps the service name to the correct hostname of the webserver.
#'
#' @param prefix Webservice to query.
prefix_map <- function(prefix) {
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  if (prefix %in% names(prefix_mapping)) {
    prefix <- prefix_mapping[[prefix]]
  }
  return(prefix)
}
