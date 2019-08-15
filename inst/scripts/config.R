devtools::load_all("~/scratch/git/EuPathDB")
bsgenome <- FALSE
orgdb <- TRUE
txdb <- TRUE
organdb <- FALSE
granges <- TRUE
eu_version <- "v42"
bioc_version <- "v3.9"

results <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list())
unlink("*.csv")
