#' List the directory containing the various sqlite files and make sure they all have entries.
#'
#' Any files which do not have csv entries should be deleted, but for the moment I will move them
#' to the current working directory in an attempt to learn about why they went wrong.
#'
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @param build_dir Location to dump files.
#' @param verbose Talk while running?
#' @param destination Place to put non-matched files.
#' @export
check_files <- function(file_type = "OrgDb", bioc_version = NULL, eu_version = NULL,
                        build_dir = "EuPathDB", verbose = FALSE, destination = NULL) {
  if (is.null(destination)) {
    destination <- getwd()
  }
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  bioc_version <- versions[["bioc_version"]]
  eu_version <- gsub(x = eu_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")

  ## Get the column containing the filepaths of the data.
  path_column <- as.character(stringr::str_to_title(file_type))
  path_column <- as.character(glue::glue("{path_column}File"))
  csv_file <- file.path(build_dir, "metadata",
                        glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_metadata.csv"))

  ## Get the metadata and start comparing it to the actual files.
  mdata <- readr::read_csv(csv_file)
  file_dir <- dirname(as.character(mdata[1, path_column]))
  file_lst <- list.files(path = file_dir, all.files = TRUE)
  csv_file_lst <- mdata[[path_column]]
  happy_count <- 0
  sad_count <- 0
  ## Skip . and ..
  for (path in 3:length(file_lst)) {
    filename <- file_lst[path]
    file_path <- file.path(file_dir, filename)
    foundp <- file_path %in% csv_file_lst
    if (isTRUE(foundp)) {
      if (isTRUE(verbose)) {
        message("Found ", file_path, " in the metadata.")
      }
      happy_count <- happy_count + 1
    } else {
      new_path <- file.path(destination, filename)
      file.rename(from = file_path, to = new_path)
      if (isTRUE(verbose)) {
        message("Did not find ", file_path, " in the metadata, moving it to ", destination, ".")
      }
      sad_count <- sad_count + 1
    }
  } ## End iterating over every file in the directory.
  retlist <- list(
    "happy" = happy_count,
    "sad" = sad_count)
  return(retlist)
}
