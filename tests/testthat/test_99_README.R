start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("README")
devtools::load_all("../../")
## Test that everything in the README works (excepting for now the AH)

library(EuPathDB)
## I pretty much always use Leishmania major strain friedlin as my example.

## This downloads metadata from every eupathdb resource:
## cryptodb, fungidb, giardiadb, microsporidiadb, piroplasmadb,
## plasmodb, toxodb, trichdb, and the tritrypdb.
## eu_metadata <- download_eupath_metadata()
## One likely wants:

## Oh yeah, there is a new L.major assembly named 'Friedlin2021' which
## is arguably better than the existing assembly, but since it has the
## suffix '2021' does not match anything in the
## taxonomyDb/GenomeInfoDb and so does not get created...
## I want it.
## Perhaps I will add a parameter to look for strain suffixes which
## look like \d{4}?  hmmm...  anyhow.


## Run the code here, test it below.

## Download metadata from tritrypdb, this step is optional.
tritryp_metadata <- download_eupath_metadata(webservice = "tritrypdb",
                                             overwrite = FALSE)
## I keep meaning to add a test to see if the metadata has already
## been downloaded, do that soon, waiting for this is silly.
## Oh! I already did!  Yay! just set overwrite = FALSE

## Extract an entry of interest, if metadata is not provided it will
## download it (defaulting to all eupathdb webservices, which can take
## a little while.
lm_entry <- get_eupath_entry(species = "MHOM/COL", metadata = tritryp_metadata)
## Look at the entry of interest.
colnames(lm_entry)
## Create an orgdb database
orgdb_pkgname <- make_eupath_orgdb(lm_entry)
## Create a txdb database, since there are so few introns in the
## trypanosomatids, it tends to be less interesting for them...
txdb_pkgname <- make_eupath_txdb(lm_entry)
grange_pkgname <- make_eupath_granges(lm_entry)
## Create a bsgenome, note you _must_ increase the number of open
## files for this to work with fragmented assemblies.[1]
bsgenome_pkgname <- make_eupath_bsgenome(lm_entry)
## Create the union of the orgdb/Txdb; this has not been tested in a _long_ time
organismdbi_pkgname <- make_eupath_organismdbi(lm_entry)
## Get a big monster data table of annotations
pkgname <- as.character(orgdb_pkgname[[1]])
major_annotations <- load_orgdb_annotations(pkgname)
## Or GO categories
major_go <- load_orgdb_go(pkgname)
library(pkgname, character = TRUE)
avail_columns <- AnnotationDbi::columns(get0(pkgname))
## Or interpro categories
lm_interpro <- load_orgdb_go(pkgname, table = "interpro")
## Or Orthologs
lm_ortho <- load_orgdb_go(pkgname, table = "ortholog")
## Or Pathway data
lm_path <- load_orgdb_go(pkgname, table = "pathway")
## Or PDB
lm_pdb <- load_orgdb_go(pkgname, table = "pdb")
## Or links to other database
lm_linkout <- load_orgdb_go(pkgname, table = "linkout")
## Or publications
lm_pubmed <- load_orgdb_go(pkgname, table = "pubmed")

## Now test the stuff we ran!
expected_valid_columns <- 66
## As of this writing, there are 60 rows.
expected_valid_rows <- 58
test_that("We downloaded metadata of species matching the taxonomyDb/genomeInfoDb?", {
  expect_equal(expected_valid_columns, ncol(tritryp_metadata[["valid"]]))
  expect_gt(nrow(tritryp_metadata[["valid"]]), expected_valid_rows)
})

test_that("We got the correct entry?", {
  expect_equal("Leishmania major strain Friedlin", lm_entry[["TaxonUnmodified"]])
})

## We will poke at the orgdb later, so just see that we got a
## packagename here.
test_that("We got a reasonable result from make_orgdb:", {
  expect_equal(c("orgdb_name", "db_path"), names(orgdb_pkgname))
})

## Test the invocation of:
## txdb_pkgname <- make_eupath_txdb(lm_entry)
test_that("We got a reasonable result from make_txdb:", {
  expect_equal(c("gff", "txdb_name"), names(txdb_pkgname))
})

actual <- library(txdb_pkgname[["txdb_name"]], character = TRUE)
test_that("We can load the TxDb:", {
  expect_true("GenomicFeatures" %in% actual)
})

## Test the invocation of:
## bsgenome_pkgname <- make_eupath_bsgenome(lm_entry)
test_that("We got a reasonable result from make_bsgenome:", {
  expect_equal("bsgenome_name", names(bsgenome_pkgname))
})

library(bsgenome_pkgname[["bsgenome_name"]], character = TRUE)
pkg_data <- get0(bsgenome_pkgname[["bsgenome_name"]])
test_that("We have the first chromosome?", {
  expect_true("LmjF.01" %in% names(pkg_data))
})

## Test the invocation of:
## organismdbi_pkgname <- make_eupath_organismdbi(lm_entry)
test_that("We got a reasonable result from make_organismdbi:", {
  expect_equal("organdb_name", names(organismdbi_pkgname))
})

## Poke at the orgdb data and prove that it is or is not valid
## Note, I should rename the function load_orgdb_go to
## load_orgdb_table, since that is all it does...

## major_annotations <- load_orgdb_annotations(pkgname)
test_that("We got some gene annotations?", {
  expect_gt(nrow(major_annotations[["genes"]]), 9300)
  expect_equal(ncol(major_annotations[["genes"]]), 8)
  expect_true("LmjF.01.0010" %in% rownames(major_annotations[["genes"]]))
})
## major_go <- load_orgdb_go(pkgname)
test_that("We got some GO data?", {
  expect_gt(nrow(major_go), 37200)
  expect_equal(ncol(major_go), 4)
  expect_true("GO:0004535" %in% major_go[["GO"]])
})
## lm_interpro <- load_orgdb_go(pkgname, table = "interpro")
test_that("We got some interpro data?", {
  expect_gt(nrow(lm_interpro), 48700)
  expect_equal(ncol(lm_interpro), 10)
  expect_true("PF11162" %in% lm_interpro[["INTERPRO_PRIMARY_ID"]])
})
## lm_ortho <- load_orgdb_go(pkgname, table = "ortholog")
## I am not going to test the data in lm_ortho, it is big
test_that("We got some ortholog data?", {
  expect_gt(nrow(lm_ortho), 697900)
  expect_equal(ncol(lm_ortho), 6)
})
## lm_path <- load_orgdb_go(pkgname, table = "pathway")
test_that("We got pathway data?", {
  expect_gt(nrow(lm_path), 144700)
  expect_equal(ncol(lm_path), 8)
})
## lm_pdb <- load_orgdb_go(pkgname, table = "pdb")
test_that("We got some pdb data?", {
  expect_gt(nrow(lm_pdb), 47000)
  expect_equal(ncol(lm_interpro), 13)
  expect_true("2heh_A" %in% lm_pdb[["PDB_STRUCTURE"]])
})
## lm_linkout <- load_orgdb_go(pkgname, table = "linkout")
test_that("We got some linkout data?", {
  expect_gt(nrow(lm_linkout), 167800)
  expect_equal(ncol(lm_linkout), 5)
})
## lm_pubmed <- load_orgdb_go(pkgname, table = "pubmed")
test_that("We got some publication data?", {
  expect_gt(nrow(lm_pubmed), 11200)
  expect_equal(ncol(lm_pubmed), 5)
  expect_true("10.1016/j.molbiopara.2006.11.009" %in% lm_pubmed[["PUBMED_DOI"]])
})
