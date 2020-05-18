#!/usr/bin/env Rscript
source("config.R")
webservice <- "microsporidiadb"
meta <- download_eupath_metadata(overwrite=TRUE, webservice=webservice, write_csv=TRUE)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

start <- 1
for (it in start:end) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, copy_s3=TRUE)
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the other packages.")
    }
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, copy_s3=TRUE)
    if (is.null(txdb_result)) {
      next
    }
    results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  }
  if (isTRUE(organdb)) {
    organ_result <- make_eupath_organismdbi(entry, copy_s3=TRUE)
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.
