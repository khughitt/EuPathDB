#' Generate an EuPathDB organism TxDb package.
#'
#' This will hopefully create a txdb package and granges savefile for a single
#' species in the eupathdb.  This depends pretty much entirely on the successful
#' download of a gff file from the eupathdb.
#'
#' @param entry One row from the organism metadata.
#' @param build_dir Base directory for building the package.
#' @param eu_version Which version of the eupathdb to use for creating this package?
#' @param reinstall Overwrite an existing installed package?
#' @param install Install the resulting package?
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @return TxDb instance name.
#' @author Keith Hughitt with significant modifications by atb.
#' @export
make_eupath_txdb <- function(entry = NULL, eu_version = NULL,
                             reinstall = FALSE, install = TRUE,
                             copy_s3 = FALSE, verbose = FALSE,
                             build_dir = "build", build = TRUE) {
  if (is.null(entry)) {
    stop("Need an entry.")
  }

  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, eu_version = eu_version)
  pkgname <- pkgnames[["txdb"]]

  gff_download_dir <- file.path(build_dir, "gff")
  if (!file.exists(gff_download_dir)) {
    created <- dir.create(gff_download_dir)
  }
  input_gff <- file.path(gff_download_dir, glue::glue("{pkgname}.gff"))
  gff_url <- gsub(pattern = "^http:", replacement = "https:", x = entry[["SourceUrl"]])
  if (isTRUE(pkgnames[["txdb_installed"]]) & !isTRUE(reinstall)) {
    message(" ", pkgname, " is already installed.")
    retlist <- list(
      "gff" = input_gff,
      "txdb_name" = pkgname)
    return(retlist)
  }

  ## check to see if gff file exists
  if (!RCurl::url.exists(gff_url)) {
    warn(sprintf("Cannot create TxDb package for %s %s: GFF file unavailable.",
                 entry[["Species"]], entry[["Strain"]]))
    message("Attempted to download url: ")
    message(gff_url)
    return(NULL)
  }
  if (isTRUE(verbose)) {
    message("Starting creation of ", pkgname, ".")
  }
  if (!file.exists(input_gff)) {
    downloaded_gff <- try(download.file(url = gff_url, destfile = input_gff,
                                        method = "curl", quiet = FALSE), silent = TRUE)
    if ("try-error" %in% class(downloaded_gff)) {
      warning(" Failed to download the gff file from: ", gff_url, ".")
      return(NULL)
    }
  }

  chr_entries <- read.delim(file = input_gff, header = FALSE, sep = "")
  chromosomes <- chr_entries[["V1"]] == "##sequence-region"
  chromosomes <- chr_entries[chromosomes, c("V2", "V3", "V4")]
  colnames(chromosomes) <- c("ID", "start", "end")
  chromosome_info <- data.frame(
    "chrom" = chromosomes[["ID"]],
    "length" = as.numeric(chromosomes[["end"]]),
    "is_circular" = NA,
    stringsAsFactors = FALSE)

  ## It appears that sometimes I get weird results from this download.file()
  ## So I will use the later import.gff3 here to ensure that the gff is actually a gff.
  granges_lst <- try(make_eupath_granges(entry = entry,
                                         copy_s3 = copy_s3), silent = TRUE)
  if ("try-error" %in% class(granges_lst)) {
    warn(sprintf("Cannot create TxDb package for %s %s: failed to create GRanges object.",
                 entry[["Species"]], entry[["Strain"]]))
    return(NULL)
  }

  txdb_metadata <- as.data.frame(t(entry))
  txdb_metadata[["name"]] <- rownames(txdb_metadata)
  colnames(txdb_metadata) <- c("value", "name")
  txdb_metadata <- txdb_metadata[, c("name", "value")]
  txdb <- NULL
  if (!is.na(entry[["TaxonomyID"]])) {
    txdb <- try(GenomicFeatures::makeTxDbFromGFF(
      taxonomyId = entry[["TaxonomyID"]],
      file = input_gff, format = "gff", chrominfo = chromosome_info,
      dataSource = entry[["SourceUrl"]],
      organism = glue::glue("{taxa[['genus']]} {taxa[['species']]}")))
  } else {
    txdb <- try(GenomicFeatures::makeTxDbFromGFF(
      file = input_gff, format = "gff", chrominfo = chromosome_info,
      dataSource = entry[["SourceUrl"]],
      organism = glue::glue("{taxa[['genus']]} {taxa[['species']]}")))
  }
  if ("try-error" %in% class(txdb)) {
    ## Perhaps it is an invalid taxonomy ID?
    txdb <- try(GenomicFeatures::makeTxDbFromGFF(
      taxonomyId = 32644, ## 32644 is unidentified according to:
      ## https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=32644
      file = input_gff, format = "gff", chrominfo = chromosome_info,
      dataSource = entry[["SourceUrl"]],
      organism = glue::glue("{taxa[['genus']]} {taxa[['species']]}")))
  }
  if ("try-error" %in% class(txdb)) {
    message("The txdb creation failed.")
    return(NULL)
  }

  ## Fill in the granges metadata.
  provider <- getMetaDataValue(txdb, "Data source")
  providerVersion <- getTxDbVersion(txdb)
  dbType <- getMetaDataValue(txdb, "Db type")
  authors <- normAuthor(entry[["Maintainer"]], entry[["Maintainer"]])
  template_path <- system.file("txdb-template", package = "GenomicFeatures")
  version_string <- format(Sys.time(), "%Y.%m")
  data_source <- getMetaDataValue(txdb, "Data source")

  ## Fill in the metadata
  symvals <- list(
    "PKGTITLE" = glue::glue("Annotation package for {dbType} object(s)"),
    "PKGDESCRIPTION" = glue::glue("Exposes an annotation databases generated from \\
                             {data_source} by exposing these as {dbType} objects"),
    "PKGVERSION" = version_string,
    "AUTHOR" = paste(authors, collapse = ", "),
    "MAINTAINER" = as.character(getMaintainer(authors)),
    "GFVERSION" = getMetaDataValue(txdb,
                                   "GenomicFeatures version at creation time"),
    "LIC" = "Artistic-2.0",
    "DBTYPE" = dbType,
    "ORGANISM" = getMetaDataValue(txdb, "Organism"),
    "SPECIES" = getMetaDataValue(txdb, "Organism"),
    "PROVIDER" = provider,
    "PROVIDERVERSION" = providerVersion,
    "RELEASEDATE" = getMetaDataValue(txdb, "Creation time"),
    "SOURCEURL" = entry[["SourceUrl"]],
    "ORGANISMBIOCVIEW" = gsub(" ", "_", getMetaDataValue(txdb, "Organism")),
    "TXDBOBJNAME" = pkgname)

  ## Do a little final sanity checking
  if (any(duplicated(names(symvals)))) {
    str(symvals)
    stop(" 'symvals' contains duplicated symbols")
  }
  is_OK <- sapply(symvals, S4Vectors::isSingleString)
  if (!all(is_OK)) {
    bad_syms <- paste(names(is_OK)[!is_OK], collapse = ", ")
    stop(" values for symbols ", bad_syms, " are not single strings")
  }

  ## Assuming we got this far, we should be able to create the txdb package.
  if (!file.exists(build_dir)) {
    tt <- dir.create(build_dir, recursive=TRUE)
  }
  pkg_list <- Biobase::createPackage(pkgname = pkgname, destinationDir = build_dir,
                                     originDir = template_path, symbolValues = symvals,
                                     unlink = TRUE)
  db_dir <- file.path(build_dir, pkgname, "inst", "extdata")
  if (!file.exists(db_dir)) {
    tt <- dir.create(db_dir, recursive = TRUE)
  }
  db_path <- file.path(db_dir, paste(pkgname, "sqlite", sep = "."))

  obj <- try(AnnotationDbi::saveDb(txdb, file = db_path))
  if ("try-error" %in% class(obj)) {
    warning(" Failed to save the txdb object.")
  }
  closed <- try(RSQLite::dbDisconnect(BiocGenerics::dbconn(obj)), silent = TRUE)

  install_dir <- file.path(build_dir, pkgname)
  install_dir <- clean_pkg(install_dir)
  ## The following two lines look stupid, because they are.
  ## They are included because of the peculiar ways in which the various
  ## Trypanosoma cruzi strains are included: e.g. the 'Esmeraldo-like' and
  ## 'NonEsmeraldo-like' species/strains.
  install_dir <- clean_pkg(install_dir, removal = "_", replace = "")
  install_dir <- clean_pkg(install_dir, removal = "_like", replace = "like")

  s3_file <- entry[["TxdbFile"]]
  if (isTRUE(copy_s3)) {
    copied <- copy_s3_file(src_dir = db_dir, type = "txdb", s3_file = s3_file)
    if (isTRUE(copied)) {
      if (isTRUE(verbose)) {
        message(" Successfully copied the txdb sqlite to the s3 staging directory.")
      }
    } else {
      warning(" Failed to copy the txdb sqlite file to the s3 staging directory.")
    }
  }

  built <- NULL
  workedp <- FALSE
  if (isTRUE(build)) {
    built <- try(devtools::build(install_dir, quiet = TRUE))
    workedp <- ! "try-error" %in% class(built)
  }

  if (isTRUE(install)) {
    inst <- try(devtools::install(install_dir, quiet = TRUE))
    workedp <- ! "try-error" %in% class(inst)
  }

  if (isTRUE(workedp)) {
    final_path <- move_final_txdb_package(pkgname)
    final_deleted <- unlink(x = install_dir, recursive = TRUE, force = TRUE)
    message("Moved built tar to ", final_path, " and deleted installation directory:")
    message(install_dir)
  }

  if (isTRUE(verbose)) {
    message("Finished creation of ", pkgname)
  }
  retlist <- list(
    "db_path" = s3_file,
    "gff" = input_gff,
    "txdb_name" = pkgname,
    "granges_file" = granges_lst[["name"]],
    "granges_variable" = granges_lst[["variable"]])
  return(retlist)
}
