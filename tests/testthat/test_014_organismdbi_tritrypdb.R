start <- as.POSIXlt(Sys.time())
library(testthat)
library(EuPathDB)
context("014_organismdbi_tritrypdb
	  12\n")

webservice <- "tritrypdb"
testing <- download_eupath_metadata(overwrite=FALSE, webservice=webservice)
wanted <- "Friedlin"

entry <- get_eupath_entry(species=wanted, metadata=testing)

organdb <- make_eupath_organismdbi(entry=entry)
library(organdb$organdb_name, character.only=TRUE)
db <- get0(organdb$organdb_name)

expected <- c("LmjF.01.0010", "LmjF.01.0020", "LmjF.01.0030",
              "LmjF.01.0040", "LmjF.01.0050", "LmjF.01.0060")
actual <- head(keys(db, keytype="GID"))
test_that("Did we get something from the organismdbi?", {
  expect_equal(expected, actual)
})

## My other favorite Leishmania species
wanted <- "panamensis"
entry <- get_eupath_entry(species=wanted, metadata=testing)
organdb <- make_eupath_organismdbi(entry=entry)
library(organdb$organdb_name, character.only=TRUE)
db <- get0(organdb$organdb_name)
expected <- c("LPAL13_000005000", "LPAL13_000005100", "LPAL13_000005200",
              "LPAL13_000005300", "LPAL13_000005400", "LPAL13_000005500")
actual <- head(keys(db, keytype="GID"))
test_that("Did we get something from the organismdbi?", {
  expect_equal(expected, actual)
})

end <- as.POSIXlt(Sys.time())
elapsed <- round(x=as.numeric(end) - as.numeric(start))
message(paste0("\nFinished 014_organismdbi_tritrypdb in ", elapsed,  " seconds."))
