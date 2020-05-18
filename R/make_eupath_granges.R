#' Generate a GRanges rda savefile from a gff file.
#'
#' There is not too much else to say. This uses import.gff from rtracklayer.
#' I should probably steal my code from hpgltools to make this work for any
#' version of a gff file, but the eupathdb is good about keeping consistent on
#' this front.
#'
#' @param entry Metadatum entry.
#' @param workdir Place to put the resulting file(s).
#' @param eu_version Optionally request a specific version of the gff file.
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @export
make_eupath_granges <- function(entry=NULL, workdir="EuPathDB", eu_version=NULL, copy_s3=FALSE) {
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  versions <- get_versions(eu_version=eu_version)
  eu_version <- versions[["eu_version"]]
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, eu_version=eu_version)
  pkgname <- pkgnames[["txdb"]]

  input_gff <- file.path(workdir, glue::glue("{pkgname}.gff"))
  if (!file.exists(input_gff)) {
    gff_url <- gsub(pattern="^http:", replacement="https:", x=entry[["SourceUrl"]])
    tt <- download.file(url=gff_url, destfile=input_gff,
                        method="curl", quiet=FALSE)
  }

  ## Dump a granges object and save it as an rda file.
  granges_result <- rtracklayer::import.gff3(input_gff)
  granges_name <- pkgnames[["granges"]]
  granges_env <- new.env()
  granges_variable <- gsub(pattern="\\.rda$", replacement="", x=granges_name)
  granges_env[[granges_variable]] <- granges_result
  granges_file <- file.path(workdir, granges_name)
  save_result <- save(list=ls(envir=granges_env),
                      file=granges_file,
                      envir=granges_env)

  if (isTRUE(copy_s3)) {
    s3_file <- entry[["GrangesFile"]]
    copied <- copy_s3_file(src_dir=granges_file, type="granges", s3_file=s3_file)
  }

  ## import.gff3 appears to be opening 2 connections to the gff file, both are read only.
  ## It is not entirely clear to me, given the semantics of import.gff3, how to close these
  ## connections cleanly, ergo the following.
  extra_connections <- rownames(showConnections())
  for (con in extra_connections) {
    closed <- try(close(getConnection(con)), silent=TRUE)
  }

  return(granges_name)
}
