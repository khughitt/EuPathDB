#' Standardize the writing of csv metadata.
#'
#' This function effectively splits the metadata from a single data frame to a set of
#' individual files, one for each data type created.
#'
#' @param metadata Set of metadata.
#' @param service EupathDB subproject, or the set of all projects named
#'  'eupathdb'.
#' @param type Either valid or invalid, defines the final output filenames.
#' @param bioc_version Version of Bioconductor used for this set of metadata.
#' @param eu_version Version of the EuPathDB used for this set of metadata.
#' @return List containing the filenames written.
<<<<<<< HEAD:R/write_eupath_metadata.R
write_eupath_metadata <- function(metadata, webservice, file_type = "valid",
                                  bioc_version = NULL, eu_version = NULL,
                                  build_dir = "EuPathDB") {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]

=======
write_eupathdb_metadata <- function(metadata, service="eupathdb", type="valid",
                                  bioc_version="3.12", eu_version="46") {

  db_version <- NULL
  if (is.null(eu_version)) {
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
    eu_version <- gsub(x=db_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  } else {
    eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
    db_version <- gsub(x=eu_version, pattern="^v", replacement="")
    ## eupathdb_version
  }

  if (is.null(bioc_version)) {
    bioc_version <- as.character(BiocManager::version())
  } else {
    bioc_version <- as.character(bioc_version)
  }

  eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/write_eupathdb_metadata.R
  file_lst <- list(
    "granges" = glue::glue("GRanges_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv"),
    "orgdb" = glue::glue("OrgDb_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv"),
    "txdb" = glue::glue("TxDb_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv"),
    "organdb" = glue::glue("OrganismDbi_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv"),
    "bsgenome" = glue::glue("BSgenome_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv"))
  if (file_type == "invalid") {
    file_lst <- list(
      "granges" = glue::glue("GRanges_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv"),
      "orgdb" = glue::glue("OrgDb_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv"),
      "txdb" = glue::glue("TxDb_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv"),
      "organdb" = glue::glue("OrganismDbi_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv"),
      "bsgenome" = glue::glue("BSgenome_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv"))
  }

  ## create output directory, if needed
  out_dir <- file.path(build_dir, "metadata")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, mode = "0755")
  }

  ## Set up the Granges data
  granges_metadata <- metadata %>%
    dplyr::mutate(
             Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
transcript information for {.data[['Taxon']]}"),
             RDataClass = "GRanges",
             DispatchClass = "GRanges",
             ResourceName = .data[["GrangesPkg"]],
             RDataPath = .data[["GrangesFile"]])

  ## fix column types
  numeric_cols <- c("NumGene", "NumOrtholog", "SourceVersion", "TaxonomyID")
  for (x in numeric_cols) {
    granges_metadata[, x] <- as.numeric(granges_metadata[, x])
  }

  if (file.exists(file_lst[["granges"]])) {
    message("Appending to an existing file: ", file_lst[["granges"]])
    readr::read_csv(file_lst[["granges"]], col_types = readr::cols()) %>%
      bind_rows(granges_metadata) %>%
      distinct() %>%
      readr::write_csv(path = file_lst[["granges"]])
  } else {
    readr::write_csv(x = granges_metadata, path = file_lst[["granges"]],
                     append = FALSE, col_names = TRUE)
  }

  ## Set up the orgdb data
  orgdb_metadata <- metadata %>%
    dplyr::mutate(
             Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
annotations for {.data[['Taxon']]}"),
             RDataClass = "OrgDb",
             DispatchClass = "SQLiteFile",
             ResourceName = .data[["OrgdbPkg"]],
             RDataPath = .data[["OrgdbFile"]])

  for (x in numeric_cols) {
    orgdb_metadata[, x] <- as.numeric(orgdb_metadata[, x])
  }

  if (file.exists(file_lst[["orgdb"]])) {
    message("Appending to an existing file: ", file_lst[["orgdb"]])
    readr::read_csv(file_lst[["orgdb"]], col_types = readr::cols()) %>%
      bind_rows(orgdb_metadata) %>%
      distinct() %>%
      readr::write_csv(path = file_lst[["orgdb"]])
  } else {
    readr::write_csv(x = orgdb_metadata, path = file_lst[["orgdb"]],
                     append = FALSE, col_names = TRUE)
  }

  ## Set up the txdb data
  txdb_metadata <- metadata %>%
    dplyr::mutate(
             Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Transcript information for {.data[['Taxon']]}"),
             RDataClass = "TxDb",
             DispatchClass = "SQLiteFile",
             ResourceName = .data[["TxdbPkg"]],
             RDataPath = .data[["TxdbFile"]])

  for (x in numeric_cols) {
    txdb_metadata[, x] <- as.numeric(txdb_metadata[, x])
  }

  if (file.exists(file_lst[["txdb"]])) {
    message("Appending to an existing file: ", file_lst[["txdb"]])
    readr::read_csv(file_lst[["txdb"]], col_types = readr::cols()) %>%
      bind_rows(txdb_metadata) %>%
      distinct() %>%
      readr::write_csv(path = file_lst[["txdb"]])
  } else {
    readr::write_csv(x = txdb_metadata, path = file_lst[["txdb"]],
                     append = FALSE, col_names = TRUE)
  }

  ## And the organismdbi data
  organismdbi_metadata <- metadata %>%
    dplyr::mutate(
             Title = glue::glue("Combined information for {.data[['Taxon']]}"),
             Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Combined information for {.data[['Taxon']]}"),
             RDataClass = "OrganismDBI",
             DispatchClass = "SQLiteFile",
             ResourceName = .data[["OrganismdbiPkg"]],
             RDataPath = .data[["OrganismdbiFile"]])

  for (x in numeric_cols) {
    organismdbi_metadata[, x] <- as.numeric(organismdbi_metadata[, x])
  }

  if (file.exists(file_lst[["organdb"]])) {
    message("Appending to an existing file: ", file_lst[["organdb"]])
    readr::read_csv(file_lst[["organdb"]], col_types = readr::cols()) %>%
      bind_rows(organismdbi_metadata) %>%
      distinct() %>%
      readr::write_csv(path = file_lst[["organdb"]])
  } else {
    readr::write_csv(x = organismdbi_metadata, path = file_lst[["organdb"]],
                     append = FALSE, col_names = TRUE)
  }

  bsgenome_metadata <- metadata %>%
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
    readr::read_csv(file_lst[["bsgenome"]], col_types = readr::cols()) %>%
      bind_rows(bsgenome_metadata) %>%
      distinct() %>%
      readr::write_csv(path = file_lst[["bsgenome"]])
  } else {
    readr::write_csv(x = bsgenome_metadata, path = file_lst[["bsgenome"]],
                     append = FALSE, col_names = TRUE)
  }
  return(file_lst)
}
