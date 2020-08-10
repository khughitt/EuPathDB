devtools::load_all(".")

bsgenome <- FALSE
orgdb <- TRUE
txdb <- TRUE
organismdb <- FALSE
granges <- TRUE
eupathdb_version <- "v46"
bioc_version <- "v3.12"

# base directory to output data files to
build_dir <- "EuPathDB"

if (!dir.exists(build_dir)) {
  dir.create(build_dir, recursive = TRUE, mode = '0755')
}

results <- list(
  "bsgenome" = list(),
  "orgdb" = list(),
  "txdb" = list(),
  "organismdbi" = list(),
  "granges" = list()
)

unlink("*.csv")
