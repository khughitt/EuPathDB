#!/usr/bin/env Rscript
## Collect eupathdb data from their various webservices and make packages from it.
library("EuPathDB")
library("dplyr")
library("glue")
library("Biostrings")

create_packages <- function(metadata, directory, cpus=2) {
  ## Increase cpus once I know this is working properly.
  cl <- parallel::makeCluster(cpus)
  doParallel::registerDoParallel(cl)
  tt <- requireNamespace("parallel")
  tt <- requireNamespace("doParallel")
  tt <- requireNamespace("iterators")
  tt <- requireNamespace("foreach")
  returns <- list(
    "bsgenome" = list(),
    "orgdb" = list(),
    "txdb" = list(),
    "organismdbi" = list())

  pkgs <- c("EuPathDB", "glue", "dplyr", "Biostrings")
  ##res <- foreach(c=1:nrow(metadata), .packages=pkgs) %dopar% {
  for (c in 1:nrow(metadata)) {
    entry <- metadata[c, ]
    species <- datum[["Species"]]
    message(glue("species is {species} \n"))
    returns[["bsgenome"]][[species]] <- make_eupath_bsgenome(
      species=species,
      metadata=metadata,
      dir=directory,
      entry=entry)
    returns[["orgdb"]][[species]] <- make_eupath_orgdb(
      species=species,
      metadata=metadata,
      entry=entry,
      dir=directory)
    returns[["txdb"]][[species]] <- make_eupath_txdb(
      species=species,
      metadata=metadata,
      entry=entry,
      dir=directory)
    returns[["organismdbi"]][[species]] <- make_eupath_organismdbi(
      species=species,
      metadata=metadata,
      entry=entry,
      dir=directory)
  }
}

## Start the main namespace here.
projects <- c("eupathdb", "amoebadb", "cryptodb", "fungidb",
              "giardiadb", "microsporidiadb", "piroplasmadb", "plasmodb",
              "toxodb", "trichdb", "tritrypdb")

for (p in 1:length(projects)) {
  project <- projects[p]
  metadata <- download_eupath_metadata(webservice=project, dir=project)
  project_result <- create_packages(metadata, project)
}
