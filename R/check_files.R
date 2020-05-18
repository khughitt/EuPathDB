#' List the directory containing the various sqlite files and make sure they all have entries.
#'
#' Any files which do not have csv entries should be deleted, but for the moment I will move them
#' to the current working directory in an attempt to learn about why they went wrong.
#'
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @param verbose Talk while running?
#' @param destination Place to put non-matched files.
#' @export
check_files <- function(file_type="OrgDb", bioc_version=NULL, eu_version=NULL,
                        verbose=FALSE, destination=NULL) {
  if (is.null(destination)) {
    destination <- getwd()
  }
  versions <- get_versions(bioc_version=bioc_version, eu_version=eu_version)
  eu_version <- versions[["eu_version"]]
  bioc_version <- versions[["bioc_version"]]
  eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  column <- as.character(stringr::str_to_title(file_type))
  column <- as.character(glue::glue("{column}File"))
  csv_file <- glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_metadata.csv")
  table <- readr::read_csv(csv_file)
  file_dir <- dirname(as.character(table[1, column]))
  file_lst <- list.files(path=file_dir, all.files=TRUE)
  csv_file_lst <- table[[column]]
  happy_count <- 0
  sad_count <- 0
  ## Skip . and ..
  for (f in 3:length(file_lst)) {
    filename <- file_lst[f]
    file_path <- file.path(file_dir, filename)
    foundp <- file_path %in% csv_file_lst
    if (isTRUE(foundp)) {
      if (isTRUE(verbose)) {
        message("Found ", file_path, " in the metadata.")
      }
      happy_count <- happy_count + 1
    } else {
      new_path <- file.path(destination, filename)
      file.rename(from=file_path, to=new_path)
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
