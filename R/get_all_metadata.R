#' Invoke download_eupathdb_metadata() using all the sub-projects of the EuPathDB.
#'
#' This just iterates over a list of existing EuPathDB web resources and
#' attempts to download the metadata from them.
#'
#' @param webservice Assume all services are desired.
#' @return Dataframe of the various species metadata.
get_all_metadata <- function(webservice="all") {
  metadata <- data.frame()
  if (webservice == "all") {
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb", "toxodb",
                  "trichdb", "tritrypdb")
    metadata <- data.frame()
    for (p in projects) {
      project_metadata <- download_eupathdb_metadata(webservice=p)
      metadata <- rbind(metadata, project_metadata)
    }
  } else {
    metadata <- download_eupathdb_metadata(webservice=webservice)
  }
  return(metadata)
}
