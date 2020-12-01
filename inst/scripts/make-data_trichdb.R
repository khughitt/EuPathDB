#!/usr/bin/env Rscript
source("config.R")
webservice <- "trichdb"
meta <- download_eupathdb_metadata(
  bioc_version=bioc_version, overwrite=TRUE, webservice=webservice,
  eupathdb_version=eupathdb_version, write_csv=TRUE)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

start <- 1
for (it in start:end) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupathdb_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupathdb_bsgenome(entry, eupathdb_version=eupathdb_version, copy_s3=TRUE, install=install)
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupathdb_orgdb(entry, eupathdb_version=eupathdb_version, copy_s3=TRUE, install=install)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the other packages.")
    }
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupathdb_txdb(entry, eupathdb_version=eupathdb_version, copy_s3=TRUE, install=install)
    if (is.null(txdb_result)) {
      next
    }
    results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  }
  if (isTRUE(organismdb)) {
    organ_result <- make_eupathdb_organismdbi(entry, eupathdb_version=eupathdb_version, copy_s3=TRUE, install=install)
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.
