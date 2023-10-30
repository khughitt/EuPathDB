#' Check the metadata csv files and write only the 'good' entries.
#'
#' While we are at it, put the failed entries into their own csv file so that I
#' can step through and look for why they failed.
#'
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @import glue
#' @export
check_s3 <- function(file_type = "OrgDb", bioc_version = NULL, eu_version = NULL) {
  ## Figure out the versions of bioc/eupathdb
  mdata_column <- stringr::str_to_title(file_type)
  mdata_column <- glue("{mdata_column}File")
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  bioc_version <- versions[["bioc_version"]]
  eu_version <- gsub(x = eu_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")

  ## Figure out the filenames for the various csv files of interest.
  csv_file <- glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_metadata.csv")
  failed_file <- glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_failed_metadata.csv")
  final_file <- glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_final_metadata.csv")

  ## Read the csv file and iterate to check the s3 data.
  dat <- readr::read_csv(csv_file)
  output_files <- dat[[mdata_column]]
  valid_files <- c()
  invalid_files <- c()
  dat[["md5sum"]] <- ""
  for (f in seq_along(length(output_files))) {
    path <- output_files[f]
    queried <- try(query_s3_file(dat[f, ], file_column = mdata_column, file_type = file_type))
    if (file.exists(file) & class(queried)[1] == "character") {
      valid_files <- c(valid_files, f)
      dat[f, "md5sum"] <- queried
    } else {
      invalid_files <- c(invalid_files, f)
      if (isTRUE(verbose)) {
        message("Did not find file: ", path)
      }
    }
  }
  message(length(valid_files), " / ", length(output_files), " expected ", file_type,
          " files were found.")

  ## Choose the valid/invalid entries to write to the csv file.
  kept_table <- dat[valid_files, ]
  failed_table <- dat[invalid_files, ]
  ## Do one more check for weirdo entries not in AnnotationHubData::getSpeciesList()
  all_valid_species <- AnnotationHubData::getSpeciesList()
  valid_idx <- kept_table[["Species"]] %in% all_valid_species
  message("There remain ", sum(!valid_idx), " problematic species.")

  ## We should have the final set of entries which are good/bad now, write it out.
  final_table <- kept_table[valid_idx, ]
  invalid_idx <- ! valid_idx
  if (sum(invalid_idx) > 0) {
    failed_table <- rbind(failed_table, kept_table[!valid_idx, ])
    message("Removing species: ", toString(kept_table[!valid_idx, "Species"]))
  }

  written <- readr::write_csv(x = final_table, path = csv_file)
  failed_written <- readr::write_csv(x = failed_table, path = failed_file)
  return(csv_file)
}
