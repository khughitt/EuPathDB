#' Create a list of output csv filenames containing the metadata for each datatype.
get_metadata_filename <- function(webservice, bioc_version, eu_version,
                                  file_type = "valid", build_dir = 'build') {
  file_lst <- list(
    "all" = file.path(
      build_dir, "metadata",
      glue::glue("biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")),
    "granges" = file.path(
      build_dir, "metadata",
      glue::glue("GRanges_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")),
    "orgdb" = file.path(
      build_dir, "metadata",
      glue::glue("OrgDb_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")),
    "txdb" = file.path(
      build_dir, "metadata",
      glue::glue("TxDb_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")),
    "organdb" = file.path(
      build_dir, "metadata",
      glue::glue("OrganismDbi_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")),
    "bsgenome" = file.path(
      build_dir, "metadata",
      glue::glue("BSgenome_biocv{bioc_version}_{webservice}{eu_version}_metadata.csv")))
  if (file_type == "invalid") {
    file_lst <- list(
      "all" = file.path(
        build_dir, "metadata",
        glue::glue("biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")),
      "granges" = file.path(
        build_dir, "metadata",
        glue::glue("GRanges_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")),
      "orgdb" = file.path(
        build_dir, "metadata",
        glue::glue("OrgDb_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")),
      "txdb" = file.path(
        build_dir, "metadata",
        glue::glue("TxDb_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")),
      "organdb" = file.path(
        build_dir, "metadata",
        glue::glue("OrganismDbi_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")),
      "bsgenome" = file.path(
        build_dir, "metadata",
        glue::glue("BSgenome_biocv{bioc_version}_{webservice}{eu_version}_invalid_metadata.csv")))
  }
  return(file_lst)
}
