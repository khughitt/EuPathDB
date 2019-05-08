#' Invoke download_eupath_metadata() using all the sub-projects of the EuPathDB.
#'
#' @param webservice Assume all services are desired.
get_all_metadata <- function(webservice="all") {
  metadata <- data.frame()
  if (webservice == "all") {
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb", "toxodb",
                  "trichdb", "tritrypdb")
    metadata <- data.frame()
    for (p in projects) {
      project_metadata <- download_eupath_metadata(webservice=p)
      metadata <- rbind(metadata, project_metadata)
    }
  } else {
    metadata <- download_eupath_metadata(webservice=webservice)
  }
  return(metadata)
}