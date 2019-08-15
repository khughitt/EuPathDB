#' Standardize the writing of csv metadata.
#'
#' This function effectively splits the metadata from a single data frame to a set of
#' individual files, one for each data type created.
#'
#' @param metadata Set of metadata.
#' @param service EupathDB subproject, or the set of all projects named
#'   'eupathdb'.
#' @param type Either valid or invalid, defines the final output filenames.
#' @param bioc_version Version of Bioconductor used for this set of metadata.
#' @param eu_version Version of the EuPathDB used for this set of metadata.
#' @return List containing the filenames written.
write_eupath_metadata <- function(metadata, service="eupathdb", type="valid",
                                  bioc_version="3.9", eu_version="44") {
  eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  file_lst <- list(
    "granges" = glue::glue("GRanges_biocv{bioc_version}_{service}{eu_version}_metadata.csv"),
    "orgdb" = glue::glue("OrgDb_biocv{bioc_version}_{service}{eu_version}_metadata.csv"),
    "txdb" = glue::glue("TxDb_biocv{bioc_version}_{service}{eu_version}_metadata.csv"),
    "organdb" = glue::glue("OrganismDbi_biocv{bioc_version}_{service}{eu_version}_metadata.csv"),
    "bsgenome" = glue::glue("BSgenome_biocv{bioc_version}_{service}{eu_version}_metadata.csv"))
  if (type == "invalid") {
    file_lst <- list(
      "granges" = glue::glue("GRanges_biocv{bioc_version}_{service}{eu_version}_invalid_metadata.csv"),
      "orgdb" = glue::glue("OrgDb_biocv{bioc_version}_{service}{eu_version}_invalid_metadata.csv"),
      "txdb" = glue::glue("TxDb_biocv{bioc_version}_{service}{eu_version}_invalid_metadata.csv"),
      "organdb" = glue::glue("OrganismDbi_biocv{bioc_version}_{service}{eu_version}_invalid_metadata.csv"),
      "bsgenome" = glue::glue("BSgenome_biocv{bioc_version}_{service}{eu_version}_invalid_metadata.csv"))
  }

  granges_metadata <- metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
transcript information for {.data[['Taxon']]}"),
  RDataClass="GRanges",
  DispatchClass="GRanges",
  ResourceName=.data[["GrangesPkg"]],
  RDataPath=.data[["GrangesFile"]])
  if (file.exists(file_lst[["granges"]])) {
    message("Appending to an existing file: ", file_lst[["granges"]])
    readr::write_csv(x=granges_metadata, path=file_lst[["granges"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=granges_metadata, path=file_lst[["granges"]],
                     append=FALSE, col_names=TRUE)
  }

  orgdb_metadata <- metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
annotations for {.data[['Taxon']]}"),
  RDataClass="OrgDb",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["OrgdbPkg"]],
  RDataPath=.data[["OrgdbFile"]])
  if (file.exists(file_lst[["orgdb"]])) {
    message("Appending to an existing file: ", file_lst[["orgdb"]])
    readr::write_csv(x=orgdb_metadata, path=file_lst[["orgdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=orgdb_metadata, path=file_lst[["orgdb"]],
                     append=FALSE, col_names=TRUE)
  }

  txdb_metadata <- metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Transcript information for {.data[['Taxon']]}"),
  RDataClass="TxDb",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["TxdbPkg"]],
  RDataPath=.data[["TxdbFile"]])
  if (file.exists(file_lst[["txdb"]])) {
    message("Appending to an existing file: ", file_lst[["txdb"]])
    readr::write_csv(x=txdb_metadata, path=file_lst[["txdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=txdb_metadata, path=file_lst[["txdb"]],
                     append=FALSE, col_names=TRUE)
  }

  organismdbi_metadata <- metadata %>%
    dplyr::mutate(
             Title=glue::glue("Combined information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Combined information for {.data[['Taxon']]}"),
  RDataClass="OrganismDBI",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["OrganismdbiPkg"]],
  RDataPath=.data[["OrganismdbiFile"]])
  if (file.exists(file_lst[["organdb"]])) {
    message("Appending to an existing file: ", file_lst[["organdb"]])
    readr::write_csv(x=organismdbi_metadata, path=file_lst[["organdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=organismdbi_metadata, path=file_lst[["organdb"]],
                     append=FALSE, col_names=TRUE)
  }

  bsgenome_metadata <- metadata %>%
    dplyr::mutate(
             Title=glue::glue("Genome for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Genome for {.data[['Taxon']]}"),
  RDataClass="BSGenome",
  DispatchClass="2bit",
  ResourceName=.data[["BsgenomePkg"]],
  RDataPath=.data[["BsgenomeFile"]])
  if (file.exists(file_lst[["bsgenome"]])) {
    message("Appending to an existing file: ", file_lst[["bsgenome"]])
    readr::write_csv(x=bsgenome_metadata, path=file_lst[["bsgenome"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=bsgenome_metadata, path=file_lst[["bsgenome"]],
                     append=FALSE, col_names=TRUE)
  }
  return(file_lst)
}
