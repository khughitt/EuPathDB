#' Standardize the writing of csv metadata.
#'
#' Here is an attempt at making the various written csv files for AnnotationHub
#' consistent.  I had a problem when filtering the data in which I managed to
#' reverse the version numbers for bioconductor and eupathdb.  I am very smrt.
#'
#' @param valid_metadata Set of metadata which are deemed 'valid.'
#' @param service EupathDB subproject, or the set of all projects named
#'   'eupathdb'.
#' @param bioc_version Version of Bioconductor used for this set of metadata.
#' @param eu_version Version of the EuPathDB used for this set of metadata.
#' @return List containing the filenames written.
write_eupath_metadata <- function(valid_metadata, service="eupathdb",
                                  bioc_version="v3.9", eu_version="v44") {
  eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  file_lst <- list(
    "granges" = glue::glue("GRanges_bioc{bioc_version}_{service}{eu_version}_metadata.csv"),
    "orgdb" = glue::glue("OrgDb_bioc{bioc_version}_{service}{eu_version}_metadata.csv"),
    "txdb" = glue::glue("TxDb_bioc{bioc_version}_{service}{eu_version}_metadata.csv"),
    "organdb" = glue::glue("OrganismDbi_bioc{bioc_version}_{service}{eu_version}_metadata.csv"),
    "bsgenome" = glue::glue("BSgenome_bioc{bioc_version}_{service}{eu_version}_metadata.csv")
  )
  granges_metadata <- valid_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
transcript information for {.data[['Taxon']]}"),
  RDataClass="GRanges",
  DispatchClass="GRanges",
  ResourceName=.data[["GrangesPkg"]],
  RDataPath=.data[["GrangesFile"]])
  if (file.exists(file_lst[["granges"]])) {
    readr::write_csv(x=granges_metadata, path=file_lst[["granges"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=granges_metadata, path=file_lst[["granges"]],
                     append=FALSE, col_names=TRUE)
  }

  orgdb_metadata <- valid_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
annotations for {.data[['Taxon']]}"),
  RDataClass="OrgDb",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["OrgdbPkg"]],
  RDataPath=.data[["OrgdbFile"]])
  if (file.exists(file_lst[["orgdb"]])) {
    readr::write_csv(x=orgdb_metadata, path=file_lst[["orgdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=orgdb_metadata, path=file_lst[["orgdb"]],
                     append=FALSE, col_names=TRUE)
  }

  txdb_metadata <- valid_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Transcript information for {.data[['Taxon']]}"),
  RDataClass="TxDb",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["TxdbPkg"]],
  RDataPath=.data[["TxdbFile"]])
  if (file.exists(file_lst[["txdb"]])) {
    readr::write_csv(x=txdb_metadata, path=file_lst[["txdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=txdb_metadata, path=file_lst[["txdb"]],
                     append=FALSE, col_names=TRUE)
  }

  organismdbi_metadata <- valid_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Combined information for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Combined information for {.data[['Taxon']]}"),
  RDataClass="OrganismDBI",
  DispatchClass="SQLiteFile",
  ResourceName=.data[["OrganismdbiPkg"]],
  RDataPath=.data[["OrganismdbiFile"]])
  if (file.exists(file_lst[["organdb"]])) {
    readr::write_csv(x=organismdbi_metadata, path=file_lst[["organdb"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=organismdbi_metadata, path=file_lst[["organdb"]],
                     append=FALSE, col_names=TRUE)
  }

  bsgenome_metadata <- valid_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Genome for {.data[['Taxon']]}"),
             Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Genome for {.data[['Taxon']]}"),
  RDataClass="BSGenome",
  DispatchClass="2bit",
  ResourceName=.data[["BsgenomePkg"]],
  RDataPath=.data[["BsgenomeFile"]])
  if (file.exists(file_lst[["bsgenome"]])) {
    readr::write_csv(x=bsgenome_metadata, path=file_lst[["bsgenome"]],
                     append=TRUE)
  } else {
    readr::write_csv(x=bsgenome_metadata, path=file_lst[["bsgenome"]],
                     append=FALSE, col_names=TRUE)
  }
  return(file_lst)
}
