test_pkgnames <- function(pkgnames) {
  passedp <- test_that("We have valid package names for this entry.", {
    expect_false(is.null(pkgnames[["organismdbi"]]))
    expect_false(is.null(pkgnames[["orgdb"]]))
    expect_false(is.null(pkgnames[["txdb"]]))
    expect_false(is.null(pkgnames[["bsgenome"]]))
    expect_false(is.null(pkgnames[["granges"]]))
  })
  return(passedp)
}

test_bsgenome <- function(bsgenome) {
  passedp <- test_that("We got a valid bsgnome package name result.", {
    expect_equal(entry[["BsgenomePkg"]], as.character(bsgenome_result[["bsgenome_name"]]))
  })
  return(passedp)
}

test_orgdb <- function(orgdb) {
  passedp <- list()
  passedp[["sqlite"]] <- test_that("Created an orgdb sqlite file.", {
    expect_true(file.exists(orgdb_result[["db_path"]]))
  })
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = orgdb_result[["db_path"]])
  tables <- RSQLite::dbListTables(db)
  ## I expect to find a couple of tables always: gene_info, genes, go, chromosomes
  passedp[["tables"]] <- test_that("The sqlite data has some of the expected tables.", {
    expect_gt(length(tables), 10)
    expect_true("gene_info" %in% tables)
    expect_true("chromosome" %in% tables)
    expect_true("go" %in% tables)
    expect_true("genes" %in% tables)
  })
  ## Now lets do a few queries on the sqlite and see if they make sense.
  found_chromosomes <- RSQLite::dbGetQuery(db, "SELECT * FROM chromosome LIMIT 10")
  passedp[["chromosomes"]] <- test_that("We have some sensible data in the gene_info table:", {
    expect_equal(nrow(found_chromosomes), 10)
  })
  ## Interestingly, the id column is '_id', does this portend a problem?
  ## hmm it looks like the first column of every table is '_id'
  ## I guess AnnotationDbi maps this to the name 'GID'?
  found_genes <- RSQLite::dbGetQuery(db, "SELECT * FROM gene_info LIMIT 10")
  found_gene_columns <- colnames(found_genes)
  passedp[["gene_info"]] <- test_that("We have some sensible data in the gene_info table:", {
    expect_gt(length(found_gene_columns), 10)
    expect_equal(nrow(found_genes), 10)
  })
  closed <- RSQLite::dbDisconnect(db)
  found_chromosomes <- NULL
  found_genes <- NULL
  return(passedp)
}

test_txdb <- function(txdb) {
  passedp <- list()
  passedp[["sqlite"]] <- test_that("Created an txdb sqlite file.", {
    expect_true(file.exists(txdb_result[["db_path"]]))
  })
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = txdb_result[["db_path"]])
  tables <- RSQLite::dbListTables(db)
  ## I expect to find a couple of tables always: gene_info, genes, go, chromosomes
  passedp[["tables"]] <- test_that("The sqlite data has some of the expected tables.", {
    expect_gt(length(tables), 2)
    expect_true("metadata" %in% tables)
    expect_true("gene" %in% tables)
  })
  found_meta <- RSQLite::dbGetQuery(db, "SELECT * FROM metadata")
  passedp[["metadata"]] <- test_that("The metadata makes sense.", {
    expect_gt(nrow(found_meta), 10)
  })
  found_genes <- RSQLite::dbGetQuery(db, "SELECT * FROM gene LIMIT 10")
  passedp[["genes"]] <- test_that("We have some txdb genes.", {
    expect_equal(nrow(found_genes), 10)
  })
  return(passedp)
}
