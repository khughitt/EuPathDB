#' Check the metadata csv files and write only the 'good' entries.
#'
#' While we are at it, put the failed entries into their own csv file so that I
#' can step through and look for why they failed.
#'
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @export
check_csv <- function(file_type="OrgDb", bioc_version="v3.9", eu_version="v44") {
  column <- stringr::str_to_title(file_type)
  column <- glue::glue("{column}File")
  csv_file <- glue::glue("{file_type}_bioc{bioc_version}_eupathdb{eu_version}_metadata.csv")
  failed_file <- glue::glue("{file_type}_bioc{bioc_version}_eupathdb{eu_version}_failed_metadata.csv")
  table <- readr::read_csv(csv_file)
  files <- table[[column]]
  keepers <- c()
  failed <- c()
  for (f in 1:length(files)) {
    file <- files[f]
    if (file.exists(file)) {
      keepers <- c(keepers, f)
    } else {
      failed <- c(failed, f)
      message("Did not find file: ", file)
    }
  }
  message("Out of ", length(files), " ", file_type, " ",
          " files, ", length(keepers), " were found.")
  final_table <- table[keepers, ]
  readr::write_csv(x=final_table, path=csv_file)
  failed_table <- table[failed, ]
  readr::write_csv(x=failed_table, path=failed_file)
  return(length(keepers))
}
