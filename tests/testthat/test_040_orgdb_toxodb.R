start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("040_orgdb_toxodb")

install <- FALSE
copy_s3 <- FALSE
version <- NULL
webservice <- "toxodb"
meta <- download_eupath_metadata(webservice = webservice, verbose = TRUE)
test_that("Did we download the metadata?", {
  expect_gt(nrow(meta[["valid"]]), 30)
  expect_lt(nrow(meta[["invalid"]]), 5)
})

all_metadata <- rbind(meta[["valid"]], meta[["invalid"]])
end <- nrow(all_metadata)

for (it in seq_len(end)) {
  entry <- all_metadata[it, ]
  sp <- entry[["TaxonUnmodified"]]
  message("Starting generation of ", sp, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  test_that("We have valid package names for this entry.", {
    expect_false(is.null(pkgnames[["organismdbi"]]))
    expect_false(is.null(pkgnames[["orgdb"]]))
    expect_false(is.null(pkgnames[["txdb"]]))
    expect_false(is.null(pkgnames[["bsgenome"]]))
    expect_false(is.null(pkgnames[["granges"]]))
  })

  bsgenome_result <- make_eupath_bsgenome(entry, eu_version = version, copy_s3 = copy_s3,
                                          install = install, reinstall = FALSE)
  test_that("We got a valid bsgnome package name result.", {
    expect_equal(entry[["BsgenomePkg"]], as.character(bsgenome_result[["bsgenome_name"]]))
  })

  orgdb_result <- make_eupath_orgdb(entry, copy_s3 = copy_s3,
                                    install = install, reinstall = FALSE)

  txdb_result <- make_eupath_txdb(entry, eu_version = version, copy_s3 = copy_s3,
                                  install = install, reinstall = FALSE)
  organ_result <- make_eupath_organismdbi(entry, eu_version = version, copy_s3 = copy_s3,
                                          install = install, reinstall = FALSE)

} ## End iterating over every entry in the eupathdb metadata.

## This needs to be rewritten for a specific toxo species of interest,
## I wrote it to make sure my L.major friedlin downloads were working from
## tritrypdb.  I don't really do Toxo, so I am not sure of a good choice,
## but I can pick something arbitrarily, like the first one!
wanted <- all_metadata[1, "Taxon"]
entry <- get_eupath_entry(species = wanted, metadata = all_metadata, column = "Taxon")
orgdb_result <- make_eupath_orgdb(entry, copy_s3 = TRUE, verbose = TRUE)
expected <- paste0("org.Tgondii.Ari.", eu_version, ".eg.db")
actual <- as.character(orgdb_result)
test_that("Did make_eupath_orgdb return an expected result?", {
  expect_equal(actual, expected)
})
library(expected, character.only = TRUE)
orgdb_annot <- load_eupath_annotations(entry)
test_that("Do we get some expected annotation data?", {
  expect_gt(ncol(orgdb_annot), 100)
  expect_gt(nrow(orgdb_annot), 9400)
})
orgdb_go <- load_eupath_go(entry)
test_that("Do we get some expected GO data?", {
  expect_equal(ncol(orgdb_go), 4)
  expect_gt(nrow(orgdb_go), 35800)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 020_orgdb_tritrypdb in ", elapsed,  " seconds."))
