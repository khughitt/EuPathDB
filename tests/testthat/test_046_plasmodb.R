start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
webservice <- "plasmodb"
context(webservice)
devtools::load_all("../../")

install <- FALSE
reinstall <- FALSE
copy_s3 <- TRUE
version <- NULL
skip_finished <- TRUE
verbose = TRUE
meta <- download_eupath_metadata(webservice = webservice, verbose = verbose)
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
  do_bsgenome <- TRUE
  if (isTRUE(skip_finished) && file.exists(entry[["BsgenomeFile"]])) {
    message("This bsgenome file already exists, skipping.")
    do_bsgenome <- FALSE
  }
  if (isTRUE(do_bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, eu_version = version, copy_s3 = copy_s3,
                                            install = install, reinstall = reinstall,
                                            verbose = verbose)
    if (!is.null(bsgenome_result)) {
      passedp <- test_bsgenome(bsgenome_result)
    }
  }
  do_orgdb <- TRUE
  if (isTRUE(skip_finished) && file.exists(entry[["OrgdbFile"]])) {
    message("This orgdb sqlite file already exists, skipping.")
    do_orgdb <- FALSE
  }
  if (isTRUE(do_orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, copy_s3 = copy_s3,
                                      install = install, reinstall = reinstall,
                                      verbose = verbose)
    run_orgdb_tests <- TRUE
    if (is.null(orgdb_result)) {
      run_orgdb_tests <- FALSE
    }
    if (isTRUE(run_orgdb_tests)) {
      passedp <- test_orgdb(orgdb_result)
    }
  }
  do_txdb <- TRUE
  if (isTRUE(skip_finished) && file.exists(entry[["TxdbFile"]])) {
    message("This txdb file already exists, skipping.")
    do_txdb <- FALSE
  }
  if (isTRUE(do_txdb)) {
    txdb_result <- make_eupath_txdb(entry, eu_version = version, copy_s3 = copy_s3,
                                    install = install, reinstall = reinstall,
                                    verbose = verbose)
    if (!is.null(txdb_result)) {
      passedp <- test_txdb(txdb_result)
    }
  }
  bsgenome_result <- NULL
  orgdb_result <- NULL
  txdb_result <- NULL
  cleaned <- gc()
} ## End iterating over every entry in the eupathdb metadata.

end <- as.POSIXlt(Sys.time())
elapsed <- round(x = as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 020_orgdb_tritrypdb in ", elapsed,  " seconds."))
