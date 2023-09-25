#' Standardize the writing of csv metadata.
#'
#' This function effectively splits the metadata from a single data frame to a set of
#' individual files, one for each data type created.
#'
#' @param metadata Set of metadata.
#' @param webservice EupathDB subproject, or the set of all projects named
#'  'eupathdb'.
#' @param file_type Either valid or invalid, defines the final output filenames.
#' @param bioc_version Version of Bioconductor used for this set of metadata.
#' @param eu_version Version of the EuPathDB used for this set of metadata.
#' @return List containing the filenames written.
#' @export
write_eupath_metadata <- function(metadata, webservice, file_type = "valid",
                                  bioc_version = NULL, eu_version = NULL,
                                  overwrite = FALSE, output_csv = NULL) {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]
  file_lst <- get_metadata_filename(webservice, bioc_version, eu_version,
                                    file_type = file_type)
  ## If we are not overwriting the data, and it already exists, then move on.
  if (file.exists(file_lst[[1]]) && isFALSE(overwrite)) {
    return(file_lst)
  }
  ## create output directory, if needed
  out_dir <- file.path(build_dir, "metadata")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, mode = "0755")
  }

  metadata <- metadata %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(stringr::str_trim)

  ## Write a csv of everything
  all_metadata <- data.frame()
  distinct_metadata <- data.frame()
  write_metadata <- metadata
  if (file.exists(file_lst[["all"]])) {
    message("Appending to an existing file: ", file_lst[["all"]])
    all_metadata <- readr::read_csv(file_lst[["all"]], col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    distinct()
  file_lst[["all_rows"]] <- nrow(distinct_metadata)
  readr::write_csv(x = distinct_metadata, file = file_lst[["all"]],
                   append = FALSE, col_names = TRUE)

  ## Set up the Granges data
  write_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
transcript information for {.data[['Taxon']]}"),
RDataClass = "GRanges",
DispatchClass = "GRanges",
ResourceName = .data[["GrangesPkg"]],
RDataPath = .data[["GrangesFile"]])
  if (file.exists(file_lst[["granges"]])) {
    message("Appending to an existing file: ", file_lst[["granges"]])
    all_metadata <- readr::read_csv(file_lst[["granges"]],
                                    col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    distinct()
  file_lst[["granges_rows"]] <- nrow(distinct_metadata)
  readr::write_csv(x = distinct_metadata, file = file_lst[["granges"]],
                   append = FALSE, col_names = TRUE)

  ## Set up the orgdb data
  write_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
annotations for {.data[['Taxon']]}"),
RDataClass = "OrgDb",
DispatchClass = "SQLiteFile",
ResourceName = .data[["OrgdbPkg"]],
RDataPath = .data[["OrgdbFile"]])
  if (file.exists(file_lst[["orgdb"]])) {
    message("Appending to an existing file: ", file_lst[["orgdb"]])
    all_metadata <- readr::read_csv(file_lst[["orgdb"]], col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    distinct()
  file_lst[["orgdb_rows"]] <- nrow(distinct_metadata)
  readr::write_csv(x = distinct_metadata, file = file_lst[["orgdb"]],
                   append = FALSE, col_names = TRUE)

  ## Set up the txdb data
  write_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Transcript information for {.data[['Taxon']]}"),
RDataClass = "TxDb",
DispatchClass = "SQLiteFile",
ResourceName = .data[["TxdbPkg"]],
RDataPath = .data[["TxdbFile"]])
  if (file.exists(file_lst[["txdb"]])) {
    message("Appending to an existing file: ", file_lst[["txdb"]])
    all_metadata <- readr::read_csv(file_lst[["txdb"]], col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    dplyr::distinct()
  file_lst[["txdb_rows"]] <- nrow(distinct_metadata)
  readr::write_csv(x = distinct_metadata, file = file_lst[["txdb"]],
                   append = FALSE, col_names = TRUE)

  ## And the organismdbi data
  write_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Combined information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Combined information for {.data[['Taxon']]}"),
RDataClass = "OrganismDBI",
DispatchClass = "SQLiteFile",
ResourceName = .data[["OrganismdbiPkg"]],
RDataPath = .data[["OrganismdbiFile"]])
  if (file.exists(file_lst[["organdb"]])) {
    message("Appending to an existing file: ", file_lst[["organdb"]])
    all_metadata <- readr::read_csv(file_lst[["organdb"]], col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    dplyr::distinct()

  readr::write_csv(x = distinct_metadata, file = file_lst[["organdb"]],
                   append = FALSE, col_names = TRUE)

  ## Finally, BSGenome
  write_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Genome for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Genome for {.data[['Taxon']]}"),
RDataClass = "BSGenome",
DispatchClass = "2bit",
ResourceName = .data[["BsgenomePkg"]],
RDataPath = .data[["BsgenomeFile"]])
  if (file.exists(file_lst[["bsgenome"]])) {
    message("Appending to an existing file: ", file_lst[["bsgenome"]])
    all_metadata <- readr::read_csv(file_lst[["bsgenome"]],
                                    col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::bind_rows(write_metadata)
  } else {
    all_metadata <- write_metadata
  }
  distinct_metadata <- all_metadata %>%
    dplyr::distinct()
  file_lst[["bsgenome_rows"]] <- nrow(distinct_metadata)
  readr::write_csv(x = distinct_metadata, file = file_lst[["bsgenome"]],
                   append = FALSE, col_names = TRUE)

  return(file_lst)
}
