start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
webservice <- "plasmodb"
context(webservice)
devtools::load_all("../../")

install <- FALSE
reinstall <- FALSE
build <- TRUE
copy_s3 <- TRUE
version <- NULL
skip_finished <- TRUE
overwrite <- TRUE
verbose <- TRUE
godb_source <- NULL
split <- 13
build_dir <- "/scratchbr/build"
eu_version <- NULL
bioc_version <- NULL

versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
eu_version <- versions[["eu_version"]]
db_version <- versions[["db_version"]]
bioc_version <- versions[["bioc_version"]]
meta_valid_files <- get_metadata_filename(webservice, bioc_version, eu_version)
meta_invalid_files <- get_metadata_filename(webservice, bioc_version, eu_version,
                                            file_type = "invalid")
if (file.exists(meta_valid_files[["orgdb"]])) {
  valid <- read.csv(meta_valid_files[["orgdb"]])
  invalid <- read.csv(meta_invalid_files[["orgdb"]])
  meta <- list(
    "valid" = valid,
    "invalid" = invalid)
} else {
  meta <- download_eupath_metadata(overwrite = overwrite, webservice = webservice,
                                   bioc_version = bioc_version, eu_version = eu_version,
                                   verbose = verbose)
}
test_that("Did we download the metadata?", {
  expect_gt(nrow(meta[["valid"]]), 1)
  expect_lt(nrow(meta[["invalid"]]), 99)
})

valid_end <- nrow(meta[["valid"]])
for (it in seq_len(valid_end)) {
  entry <- meta[["valid"]][it, ]
  sp <- entry[["TaxonUnmodified"]]
  message("Starting generation of ", sp, ", which is ", it, " of ", valid_end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  passedp <- test_pkgnames(pkgnames)
  do_orgdb <- TRUE
  if (isTRUE(skip_finished) && file.exists(entry[["OrgdbFile"]])) {
    message("This orgdb sqlite file already exists, skipping.")
    do_orgdb <- FALSE
  }
  if (isTRUE(do_orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, install = install, reinstall = reinstall,
                                      overwrite = overwrite, verbose = verbose, build = build,
                                      copy_s3 = copy_s3, godb_source = godb_source,
                                      split = split, build_dir = build_dir)
    run_orgdb_tests <- TRUE
    if (is.null(orgdb_result)) {
      run_orgdb_tests <- FALSE
    }
    if (isTRUE(run_orgdb_tests)) {
      passedp <- test_orgdb(orgdb_result, entry)
    }
  }
  orgdb_result <- NULL
  cleaned <- gc()
} ## End iterating over every entry in the eupathdb metadata.

end <- as.POSIXlt(Sys.time())
elapsed <- round(x = as.numeric(end) - as.numeric(start))
message(paste0("\nFinished in ", elapsed,  " seconds."))
