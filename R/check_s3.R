#' Check the metadata csv files and write only the 'good' entries.
#'
#' While we are at it, put the failed entries into their own csv file so that I
#' can step through and look for why they failed.
#'
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @export
check_s3 <- function(file_type="OrgDb", bioc_version="3.9", eu_version="44") {
  column <- stringr::str_to_title(file_type)
  column <- glue::glue("{column}File")
  eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  csv_file <- glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_metadata.csv")
  failed_file <- glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_failed_metadata.csv")
  final_file <- glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_final_metadata.csv")
  table <- readr::read_csv(csv_file)
  files <- table[[column]]
  keepers <- c()
  failed <- c()
  table[["md5sum"]] <- ""
  for (f in 1:length(files)) {
    file <- files[f]
    queried <- try(query_s3_file(table[f, ], file_column=column, file_type=file_type))
    if (file.exists(file) & class(queried)[1] == "character") {
      keepers <- c(keepers, f)
      table[f, "md5sum"] <- queried
    } else {
      failed <- c(failed, f)
      if (isTRUE(verbose)) {
        message("Did not find file: ", file)
      }
    }
  }
  message("Out of ", length(files), " ", file_type, " ",
          " files, ", length(keepers), " were found.")
  kept_table <- table[keepers, ]
  failed_table <- table[failed, ]
  ## Do one more check for weirdo entries not in AnnotationHubData::getSpeciesList()
  all_valid_species <- AnnotationHubData::getSpeciesList()
  valid_idx <- kept_table[["Species"]] %in% all_valid_species
  message("There remain ", sum(!valid_idx), " problematic species.")
  final_table <- kept_table[valid_idx, ]
  invalid_idx <- ! valid_idx
  if (sum(invalid_idx) > 0) {
    failed_table <- rbind(failed_table, kept_table[!valid_idx, ])
    message("Removing species: ", toString(kept_table[!valid_idx, "Species"]))
  }

  written <- readr::write_csv(x=final_table, path=csv_file)
  failed_written <- readr::write_csv(x=failed_table, path=failed_file)
  return(csv_file)
}
