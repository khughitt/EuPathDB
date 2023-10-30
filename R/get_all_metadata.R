#' Invoke download_eupathdb_metadata() using all the sub-projects of the EuPathDB.
#'
#' This just iterates over a list of existing EuPathDB web resources and
#' attempts to download the metadata from them.
#'
#' @param ... Arguments passed from above.
#' @return Dataframe of the various species metadata.
#' @export
get_all_metadata <- function(overwrite = TRUE, bioc_version = NULL,
                             eu_version = NULL, verbose = FALSE,
                             build_dir = "build") {
  ##projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
  ##              "microsporidiadb", "piroplasmadb", "plasmodb",
  ##              "schistodb", "toxodb", "trichdb", "tritrypdb")
  ## Schistodb does not appear to have started using the new database schema yet.
  projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                "microsporidiadb", "piroplasmadb", "plasmodb",
                "toxodb", "trichdb", "tritrypdb")
  for (i in seq_along(length(projects))) {
    webservice <- projects[i]
    results[[webservice]] <- download_eupath_metadata(
      webservice = webservice, overwrite = overwrite, bioc_version = bioc_version,
      eu_version = eu_version, verbose = verbose, build_dir = build_dir)
  }

  for (r in results) {
    valid_metadata <- rbind(valid_metadata, r[["valid"]])
    invalid_metadata <- rbind(invalid_metadata, r[["invalid"]])
  }

  if (isTRUE(write_csv)) {
    message("Writing metadata csv files.")
    written <- write_eupath_metadata(metadata = valid_metadata, webservice = "eupathdb",
                                     file_type = "valid", bioc_version = bioc_version,
                                     eu_version = eu_version, overwrite = overwrite)
  }
  class(retlist) <- "eupath_metadata"
  retlist <- list(
    "valid" = valid_metadata,
    "invalid" = invalid_metadata)
  return(retlist)
}
