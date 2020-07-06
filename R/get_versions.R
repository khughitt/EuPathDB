get_versions <- function(eu_version=NULL, bioc_version=NULL) {
  if (is.null(eu_version)) {
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
    eu_version <- gsub(x=db_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
  } else {
    eu_version <- gsub(x=eu_version, pattern="^(\\d)(.*)$", replacement="v\\1\\2")
    db_version <- gsub(x=eu_version, pattern="^v", replacement="")
    ## eupath_version
  }
  ## For when releasing a new bioconductor release which I don't yet have.
  if (is.null(bioc_version)) {
    bioc_version <- as.character(BiocManager::version())
  }
  retlist <- list(
      "eu_version" = eu_version,
      "db_version" = db_version,
      "bioc_version" = bioc_version)
  return(retlist)
}
