start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("040_crypto")
devtools::load_all("../../")

install <- FALSE
copy_s3 <- TRUE
version <- NULL
webservice <- "cryptodb"
meta <- download_eupath_metadata(webservice = webservice, verbose = TRUE)
test_that("Did we download the metadata?", {
  expect_gt(nrow(meta[["valid"]]), 19)
  expect_lt(nrow(meta[["invalid"]]), 5)
})

valid_end <- nrow(meta[["valid"]])
for (it in seq_len(valid_end)) {
  entry <- meta[["valid"]][it, ]
  sp <- entry[["TaxonUnmodified"]]
  message("Starting generation of ", sp, ", which is ", it, " of ", valid_end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  test_that("We have valid package names for this entry.", {
    expect_false(is.null(pkgnames[["organismdbi"]]))
    expect_false(is.null(pkgnames[["orgdb"]]))
    expect_false(is.null(pkgnames[["txdb"]]))
    expect_false(is.null(pkgnames[["bsgenome"]]))
    expect_false(is.null(pkgnames[["granges"]]))
  })

  bsgenome_result <- make_eupath_bsgenome(entry, eu_version = version, copy_s3 = FALSE,
                                          install = install, reinstall = FALSE)
  test_that("We got a valid bsgnome package name result.", {
    expect_equal(entry[["BsgenomePkg"]], as.character(bsgenome_result[["bsgenome_name"]]))
  })

  orgdb_result <- make_eupath_orgdb(entry, copy_s3 = copy_s3,
                                    install = install, reinstall = FALSE)
  run_orgdb_tests <- TRUE
  if (is.null(orgdb_result)) {
    run_orgdb_tests <- FALSE
  }
  if (isTRUE(run_orgdb_tests)) {
    test_that("Created an orgdb sqlite file.", {
      expect_true(file.exists(orgdb_result[["db_path"]]))
    })
    db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = orgdb_result[["db_path"]])
    tables <- RSQLite::dbListTables(db)
    ## I expect to find a couple of tables always: gene_info, genes, go, chromosomes
    test_that("The sqlite data has some of the expected tables.", {
      expect_gt(length(tables), 10)
      expect_true("gene_info" %in% tables)
      expect_true("chromosome" %in% tables)
      expect_true("go" %in% tables)
      expect_true("genes" %in% tables)
    })
    ## Now lets do a few queries on the sqlite and see if they make sense.
    found_chromosomes <- RSQLite::dbGetQuery(db, "SELECT * FROM chromosome")
    ## Interestingly, the id column is '_id', does this portend a problem?
    ## hmm it looks like the first column of every table is '_id'
    ## I guess AnnotationDbi maps this to the name 'GID'?
    found_genes <- RSQLite::dbGetQuery(db, "SELECT * FROM gene_info limit 10")
    found_gene_columns <- colnames(found_genes)
    test_that("We have some sensible data in the gene_info table:", {
      expect_gt(length(found_gene_columns), 10)
      expect_equal(nrow(found_genes), 10)
    })
    closed <- RSQLite::dbDisconnect(db)
  }

  txdb_result <- make_eupath_txdb(entry, eu_version = version, copy_s3 = copy_s3,
                                  install = install, reinstall = FALSE)
  if (!is.null(txdb_result)) {
    test_that("Created an txdb sqlite file.", {
      expect_true(file.exists(txdb_result[["db_path"]]))
    })
    db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = txdb_result[["db_path"]])
    tables <- RSQLite::dbListTables(db)
    ## I expect to find a couple of tables always: gene_info, genes, go, chromosomes
    test_that("The sqlite data has some of the expected tables.", {
      expect_gt(length(tables), 2)
      expect_true("metadata" %in% tables)
      expect_true("gene" %in% tables)
    })
    found_meta <- RSQLite::dbGetQuery(db, "SELECT * FROM metadata")
    test_that("The metadata makes sense.", {
      expect_gt(nrow(found_meta), 10)
    })
    found_genes <- RSQLite::dbGetQuery(db, "SELECT * FROM gene LIMIT 10")
    test_that("We have some txdb genes.", {
      expect_equal(nrow(found_genes), 10)
    })
  }

} ## End iterating over every entry in the eupathdb metadata.

end <- as.POSIXlt(Sys.time())
elapsed <- round(x = as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 020_orgdb_tritrypdb in ", elapsed,  " seconds."))
