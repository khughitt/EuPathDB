#!/usr/bin/env Rscript
source("config.R")
webservice <- "fungidb"
meta <- download_eupath_metadata(bioc_version=bioc_version, overwrite=TRUE,
                                 webservice=webservice, eu_version=eu_version)
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
  species <- entry[["TaxonUnmodified"]]
  message("Starting generation of ", species, ", which is ", it, " of ", end, " species.")
  pkgnames <- get_eupathdb_pkgnames(entry)
  if (isTRUE(bsgenome)) {
    bsgenome_result <- make_eupath_bsgenome(entry, copy_s3=TRUE, install=install, overwrite=TRUE)
    results[["bsgenome"]][[species]] <- bsgenome_result
  }
  if (isTRUE(orgdb)) {
    orgdb_result <- make_eupath_orgdb(entry, copy_s3=TRUE, install=install, overwrite=TRUE)
    if (is.null(orgdb_result)) {
      message("There is insufficient data for ", species, " to make the other packages.")
    }
    results[["orgdb"]][[species]] <- orgdb_result
  }
  if (isTRUE(txdb)) {
    txdb_result <- make_eupath_txdb(entry, eu_version=eu_version, copy_s3=TRUE, install=install)
    if (is.null(txdb_result)) {
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
    organ_result <- make_eupath_organismdbi(entry, eu_version=eu_version,
                                            copy_s3=TRUE, install=install)
    results[["organismdbi"]] <- organ_result
  }
} ## End iterating over every entry in the eupathdb metadata.
