#' Check the metadata csv files and write only the 'good' entries.
#'
#' While we are at it, put the failed entries into their own csv file so that I
#' can step through and look for why they failed.
#'
#' @param build_dir Directory in which to put the data.
#' @param file_type Is this an OrgDB, GRanges, TxDb, OrganismDbi, or BSGenome dataset?
#' @param bioc_version Which bioconductor version is this for?
#' @param eu_version Which eupathdb version is this for?
#' @export
check_csv <- function(build_dir = "build", file_type = "OrgDb",
                      bioc_version = NULL, eu_version = NULL) {
  ## Make sure the versions are set.
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  bioc_version <- versions[["bioc_version"]]
  eu_version <- versions[["eu_version"]]

  ## Determine column name according to the output file
  mdata_column <- stringr::str_to_title(file_type)
  mdata_column <- glue::glue("{mdata_column}File")

  ## This line is probably not needed anymore after get_versions(), but leaving it for now.
  eu_version <- gsub(x = eu_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")

  ## Set the file paths for the metadata.  I was just doing it in the cwd, but this is better.
  csv_file <- file.path(build_dir, "metadata",
                        glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_metadata.csv"))
  failed_file <- file.path(build_dir, "metadata",
                           glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_failed_metadata.csv"))
  final_file <- file.path(build_dir, "metadata",
                          glue::glue("{file_type}_biocv{bioc_version}_eupathdb{eu_version}_final_metadata.csv"))

  ## Load the metadata and hunt down the expected output files
  dat <- readr::read_csv(csv_file, col_types = readr::cols())
  output_files <- dat[[mdata_column]]
  valid_files <- c()
  invalid_files <- c()
  dat[["md5sum"]] <- ""

  ## Iterate over the output files and make sure everything is ok.
  for (f in 1:length(files)) {
    output_file <- output_files[f]
    entry <- dat[f, ]
    message("Checking CSV entry: ", f, " file: ", file, ".")

    queried <- query_s3_file(row, file_type = file_type, file_column = mdata_column)
    valid <- file.exists(output_file) &
      class(queried)[1] == "character" &
      file.size(output_file) > 0
    if (file.size(file) == 0) {
      removed_zero <- file.remove(file)
    }
    if (isTRUE(valid)) {
      valid_files <- c(valid_files, f)
      dat[f, "md5sum"] <- queried
    } else {
      invalid_files <- c(invalid_files, f)
      if (isTRUE(verbose)) {
        message("The following file was missing or invalid: ", output_file)
      }
    }
  }
  message(length(valid_files), " / ", length(output_files), " expected ", file_type,
          " files were found.")
  kept_table <- dat[valid_files, ]
  failed_table <- dat[invalid_files, ]

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

  ## Add a check for duplicated data
  final_table <- final_table %>%
    distinct()
  failed_table <- failed_table %>%
    distinct()

  ## Write out the final csv file and return the name of the written csv.
  written <- readr::write_csv(x = final_table, path = csv_file)
  failed_written <- readr::write_csv(x = failed_table, path = failed_file)
  return(csv_file)
}
