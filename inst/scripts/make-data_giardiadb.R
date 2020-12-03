#!/usr/bin/env Rscript
source("config.R")
webservice <- "giardiadb"
meta <- download_eupath_metadata(bioc_version=bioc_version, overwrite=TRUE,
                                 webservice=webservice, eu_version=eu_version)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

start <- 1
for (it in start:end) {
  entry <- all_metadata[it, ]
  species <- entry[["TaxonUnmodified"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupathdb_bsgenome(entry, copy_s3=TRUE, install=install, overwrite=TRUE)
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE, install=install, overwrite=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the orgdb.")
    }
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, eu_version=eu_version,
                                    copy_s3=TRUE, install=install)
    if (is.null(txdb_result)) {
      message("Unable to create a txdb for ", species)
      next
    }
    results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  }
  if (isTRUE(granges)) {
    grange_result <- make_eupath_granges(entry, eu_version=eu_version,
                                         copy_s3=TRUE)
    if (is.null(grange_result)) {
      message("Unable to create a txdb for ", species)
      next
    }
    results[["granges"]][[species]] <- grange_result[["name"]]
  }
  if (isTRUE(organismdb)) {
    organ_result <- make_eupathdb_organismdbi(entry, eu_version=eu_version,
                                              copy_s3=TRUE, install=install)
  }
} ## End iterating over every entry in the eupathdb metadata.
