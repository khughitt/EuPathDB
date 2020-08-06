# note: make-data.R script should be executed from the root directory of the EuPathDB package
devtools::load_all(".")

bsgenome <- FALSE
orgdb <- TRUE
txdb <- TRUE
organdb <- FALSE
granges <- TRUE
eu_version <- "v46"
bioc_version <- "v3.12"

results <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list()
)

unlink("*.csv")
