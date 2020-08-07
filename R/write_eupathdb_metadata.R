#' Standardize the writing of csv metadata.
#'
#' This function effectively splits the metadata from a single data frame to a set of
#' individual files, one for each data type created.
#'
#' @param metadata Set of metadata.
#' @param service EupathDB subproject, or the set of all projects named
<<<<<<< HEAD
#'  'eupathdb'.
#' @param type Either valid or invalid, defines the final output filenames.
=======
#'   'eupathdb'.
#' @param file_type Either valid or invalid, defines the final output filenames.
>>>>>>> fc81572 (Some more refactoring / fixes)
#' @param bioc_version Version of Bioconductor used for this set of metadata.
#' @param eupathdb_version Version of the EuPathDB used for this set of metadata.
#' @return List containing the filenames written.
<<<<<<< HEAD
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
=======
write_eupathdb_metadata <- function(metadata, service = "eupathdb", 
                                    bioc_version = "v3.12", eupathdb_version = "v46",
                                    file_type = "valid",
                                    build_dir = "EuPathDB") {
>>>>>>> cc20d16 (Continuing clean-up / re-organization)

  # determine version of eupathdb to query
  if (is.null(eupathdb_version)) {
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
    eupathdb_version <- gsub("^(\\d)(.*)$", "v\\1\\2", db_version)
  } else {
    eupathdb_version <- gsub("^(\\d)(.*)$", "v\\1\\2", eupathdb_version)
    db_version <- gsub("^v", "", eupathdb_version)
  }

  eupathdb_version <- gsub("^(\\d)(.*)$", "v\\1\\2", eupathdb_version)

  # determine bioconductor version
  if (is.null(bioc_version)) {
    bioc_version <- as.character(BiocManager::version())
  } else {
    bioc_version <- as.character(bioc_version)
  }

<<<<<<< HEAD
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
=======
  # determine output filenames to use
  output_paths <- list(
    "granges" = glue::glue("GRanges_bioc{bioc_version}_{service}{eupathdb_version}_metadata.csv"),
    "orgdb" = glue::glue("OrgDb_bioc{bioc_version}_{service}{eupathdb_version}_metadata.csv"),
    "txdb" = glue::glue("TxDb_bioc{bioc_version}_{service}{eupathdb_version}_metadata.csv"),
    "organismdb" = glue::glue("OrganismDbi_bioc{bioc_version}_{service}{eupathdb_version}_metadata.csv"),
    "bsgenome" = glue::glue("BSgenome_bioc{bioc_version}_{service}{eupathdb_version}_metadata.csv")
  )

  # add invalid suffix, depending on metadata type
  if (file_type == "invalid") {
    output_paths <- sub('metadata.csv', 'invalid_metadata.csv', output_paths)
  }

  # create output directory, if needed
  out_dir <- file.path(build_dir, 'metadata')

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, mode = '0755')
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  }
  output_paths[] <- file.path(out_dir, output_paths)

<<<<<<< HEAD
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
=======
  #
  # GRanges
  #
  granges_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Genomic coordinates information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} genomic coordinate information for {.data[['Taxon']]}"),
      RDataClass = "GRanges",
      DispatchClass = "GRanges",
      ResourceName = .data[["GrangesPkg"]],
      RDataPath = .data[["GrangesFile"]]
    )

  # fix column types
  numeric_cols <- c("NumGenes", "NumOrthologs", "SourceVersion", "TaxonomyId")

  for (x in numeric_cols) {
    granges_metadata[, x] <- as.numeric(granges_metadata[, x])
  }

  if (file.exists(output_paths[["granges"]])) {
    info("Appending to existing file: ", output_paths[["granges"]])

    readr::read_csv(output_paths$granges, col_types = readr::cols()) %>%
      bind_rows(granges_metadata) %>%
      distinct() %>%
      readr::write_csv(path = output_paths$granges)

  } else {
    readr::write_csv(x = granges_metadata, path = output_paths[["granges"]],
                     append = FALSE, col_names = TRUE)
  }

  #
  # OrgDb
  #
  orgdb_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Genome wide annotations for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} annotations for {.data[['Taxon']]}"),
      RDataClass = "OrgDb",
      DispatchClass = "SQLiteFile",
      ResourceName = .data[["OrgdbPkg"]],
      RDataPath = .data[["OrgdbFile"]]
    )

  # fix column types
  for (x in numeric_cols) {
    orgdb_metadata[, x] <- as.numeric(orgdb_metadata[, x])
  }

  if (file.exists(output_paths[["orgdb"]])) {
    info("Appending to existing file: ", output_paths[["orgdb"]])

    readr::read_csv(output_paths$orgdb, col_types = readr::cols()) %>%
      bind_rows(orgdb_metadata) %>%
      distinct() %>%
      readr::write_csv(path = output_paths$orgdb)

  } else {
    readr::write_csv(x = orgdb_metadata, path = output_paths[["orgdb"]],
                    append = FALSE, col_names = TRUE)
  }

  #
  # TxDb
  #
  txdb_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Transcript information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} Transcript information for {.data[['Taxon']]}"),
      RDataClass = "TxDb",
      DispatchClass = "SQLiteFile",
      ResourceName = .data[["TxdbPkg"]],
      RDataPath = .data[["TxdbFile"]]
  )

  # fix column types
  for (x in numeric_cols) {
    txdb_metadata[, x] <- as.numeric(txdb_metadata[, x])
  }

  if (file.exists(output_paths[["txdb"]])) {
    info("Appending to existing file: ", output_paths[["txdb"]])

    readr::read_csv(output_paths$txdb, col_types = readr::cols()) %>%
      bind_rows(txdb_metadata) %>%
      distinct() %>%
      readr::write_csv(path = output_paths$txdb)

  } else {
    readr::write_csv(x = txdb_metadata, path = output_paths[["txdb"]],
                     append = FALSE, col_names = TRUE)
  }

  #
  # OrganismDbi
  #
  organismdbi_metadata <- metadata %>%
    dplyr::mutate(
      Title = glue::glue("Combined information for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} Combined information for {.data[['Taxon']]}"),
      RDataClass = "OrganismDBI",
      DispatchClass = "SQLiteFile",
      ResourceName = .data[["OrganismdbiPkg"]],
      RDataPath = .data[["OrganismdbiFile"]]
    )

  # fix column types
  for (x in numeric_cols) {
    organismdbi_metadata[, x] <- as.numeric(organismdbi_metadata[, x])
  }

  if (file.exists(output_paths[["organismdb"]])) {
    info("Appending to existing file: ", output_paths[["organismdb"]])

    readr::read_csv(output_paths$organismdb, col_types = readr::cols()) %>%
      bind_rows(organismdbi_metadata) %>%
      distinct() %>%
      readr::write_csv(path = output_paths$organismdb)

  } else {
    readr::write_csv(x = organismdbi_metadata, path = output_paths[["organismdb"]],
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
                     append = FALSE, col_names = TRUE)
  }

  #
  # BSGenome
  #
  bsgenome_metadata <- metadata %>%
    dplyr::mutate(
<<<<<<< HEAD
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
=======
      Title = glue::glue("Biostrings genome data for {.data[['Taxon']]}"),
      Description = glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} Biostrings genome data for {.data[['Taxon']]}"),
      RDataClass = "BSGenome",
      DispatchClass = "2bit",
      ResourceName = .data[["BsgenomePkg"]],
      RDataPath = .data[["BsgenomeFile"]]
  )

  # fix column types
  for (x in numeric_cols) {
    bsgenome_metadata[, x] <- as.numeric(bsgenome_metadata[, x])
  }

  if (file.exists(output_paths[["bsgenome"]])) {
    info("Appending to existing file: ", output_paths[["bsgenome"]])

    readr::read_csv(output_paths$bsgenome, col_types = readr::cols()) %>%
      bind_rows(bsgenome_metadata) %>%
      distinct() %>%
      readr::write_csv(path = output_paths$Vsgenome)

  } else {
    readr::write_csv(x = bsgenome_metadata, path = output_paths[["bsgenome"]],
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
                     append = FALSE, col_names = TRUE)
  }

  # return list of output files
  return(output_paths)
}
