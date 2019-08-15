#!/usr/bin/env Rscript
source("config.R")
webservice <- "tritrypdb"
meta <- download_eupath_metadata(
  bioc_version=bioc_version, overwrite=TRUE, webservice=webservice,
  eu_version=eu_version, write_csv=TRUE)
all_metadata <- meta[["valid"]]
end <- nrow(all_metadata)

odds <- c()
for (i in 1:nrow(all_metadata)) {
  if (i %% 2 == 1) {
    odds <- c(odds, i)
  }
}

for (it in odds) {
  entry <- all_metadata[it, ]
  species <- entry[["Species"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupath_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, eu_version=eu_version, copy_s3=TRUE)
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, eu_version=eu_version, copy_s3=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the other packages.")
    }
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, eu_version=eu_version, copy_s3=TRUE)
    if (is.null(txdb_result)) {
      next
    }
    results[["txdb"]][[species]] <- txdb_result[["txdb_name"]]
  }
  if (isTRUE(organdb)) {
    organ_result <- make_eupath_organismdbi(entry, eu_version=eu_version, copy_s3=TRUE)
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.
