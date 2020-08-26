#' Create an organismDbi instance for an eupathdb organism.
#'
#' The primary goal of an organismdbi instance is to provide a series of links
#' between an orgdb, txdb, and other relevant annotation packages
#' (reactome/go/etc).  In its current iteration, this function brings together a
#' couple columns from the orgdb, txdb, GO.db, and reactome.db.
#'
#' @param entry A row from the eupathdb metadataframe.
#' @param eu_version Which version of the eupathdb to use for creating this package?
#' @param workdir Directory in which to build the packages.
#' @param installp Install the resulting package?
#' @param reinstall Overwrite existing data files?
#' @param kegg_abbreviation For when we cannot automagically find the kegg species id.
#' @param exclude_join I had a harebrained idea to automatically set up the
#'  joins between columns of GO.db/reactome.db/orgdb/txdb objects.  This
#'  variable is intended to exclude columns with common IDs that might
#'  multi-match spuriously -- I think in the end I killed the idea though,
#'  perhaps this should be removed or resurrected.
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @return The result of attempting to install the organismDbi package.
#' @author Keith Hughitt, modified by atb.
#' @export
make_eupath_organismdbi <- function(entry = NULL, eu_version = NULL, workdir = "EuPathDB",
                                    installp = TRUE, reinstall = FALSE, kegg_abbreviation = NULL,
                                    exclude_join = "ENTREZID", copy_s3 = FALSE) {
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  versions <- get_versions()
  eu_version <- versions[["eu_version"]]
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, eu_version=eu_version)
  pkgname <- pkgnames[["organismdbi"]]
  if (isTRUE(pkgnames[["organismdbi_installed"]]) & !isTRUE(reinstall)) {
    message(" ", pkgname, " is already installed.")
    retlist <- list(
      "organdb_name" = pkgname)
    return(retlist)
  }
  orgdb_name <- pkgnames[["orgdb"]]
  txdb_name <- pkgnames[["txdb"]]
  orgdb_ret <- make_eupath_orgdb(entry, workdir=workdir,
                                 kegg_abbreviation=kegg_abbreviation,
                                 reinstall=reinstall)
  if (is.null(orgdb_ret)) {
    return(NULL)
  }
  txdb_ret <- make_eupath_txdb(entry, eu_version=eu_version, workdir=workdir, reinstall=reinstall)
  if (is.null(txdb_ret)) {
    return(NULL)
  }

  tt <- requireNamespace(orgdb_name)
  tt <- requireNamespace(txdb_name)
  test <- do.call("library", as.list(orgdb_name))
  test <- do.call("library", as.list(txdb_name))
  organism <- taxa[["taxon"]]
  required <- requireNamespace("OrganismDbi")

  message("Joining the txdb and orgdb objects.")
  count <- 0
  graph_data <- list()
  geneids_found <- "GID" %in% AnnotationDbi::keytypes(get(orgdb_name)) &&
    "GENEID" %in% AnnotationDbi::keytypes(get(txdb_name))
  if (isTRUE(geneids_found)) {
    count <- count + 1
    name <- glue::glue("join{count}")
    graph_data[[name]] <- c(orgdb="GID",  txdb="GENEID")
    names(graph_data[[name]]) <- c(orgdb_name, txdb_name)
  }
  required <- requireNamespace("GO.db")
  required <- try(attachNamespace("GO.db"), silent=TRUE)

  goids_found_in_godb <- "GOID" %in% AnnotationDbi::keytypes(get("GO.db"))
  goids_found_in_pkg <- "GO_ID" %in% AnnotationDbi::columns(get(orgdb_name))
  goids_found <- goids_found_in_godb & goids_found_in_pkg
  if (isTRUE(goids_found)) {
    count <- count + 1
    name <- glue::glue("join{count}")
    graph_data[[name]] <- c(GO.db="GOID", orgdb="GO_ID")
    names(graph_data[[name]]) <- c("GO.db", orgdb_name)
  }

  required <- requireNamespace("reactome.db")
  required <- try(attachNamespace("reactome.db"), silent=TRUE)
  reactomeids_found_in_reactome <- "REACTOMEID" %in% AnnotationDbi::columns(get("reactome.db"))
  reactomeids_found_in_pkg <- "PATHWAY_ID" %in% AnnotationDbi::keytypes(get(orgdb_name))
  reactomeids_found <- reactomeids_found_in_reactome & reactomeids_found_in_pkg
  if (isTRUE(reactomeids_found)) {
    count <- count + 1
    name <- glue::glue("join{count}")
    graph_data[[name]] <- c(reactome.db="REACTOMEID", orgdb="PATHWAY_ID")
    names(graph_data[[name]]) <- c("reactome.db", orgdb_name)
  }

  author <- as.character(entry[["Maintainer"]])
  maintainer <- as.character(entry[["Maintainer"]])
  final_dir <- file.path(workdir, "organismdbi", pkgname)
  if (file.exists(final_dir)) {
    if (isTRUE(reinstall)) {
      unlinkret <- unlink(x=final_dir, recursive=TRUE)
    } else {
      if (file.exists(glue::glue("{final_dir}.bak"))) {
        unlinkret <- unlink(x=glue::glue("{final_dir}.bak"), recursive=TRUE)
      }
      renamed <- file.rename(from=final_dir, to=glue::glue("{final_dir}.bak"))
    }
  }

  ## The way makeOrganismPackage handles directories is very confusing.
  tmp_pkg_dir <- file.path(workdir)
  if (!file.exists(tmp_pkg_dir)) {
    dir.create(tmp_pkg_dir, recursive=TRUE)
  }
  version_string <- format(Sys.time(), "%Y.%m")
  organdb <- OrganismDbi::makeOrganismPackage(
                            pkgname=pkgname,
                            graphData=graph_data,
                            organism=organism,
                            version=version_string,
                            maintainer=maintainer,
                            author=author,
                            destDir=tmp_pkg_dir,
                            license="Artistic-2.0")
  srcdir <- file.path(tmp_pkg_dir, pkgname)
  if (!file.exists(dirname(final_dir))) {
    dir.create(dirname(final_dir), recursive=TRUE)
  }
  renamed <- file.rename(srcdir, final_dir)

  organdb_path <- clean_pkg(final_dir)
  organdb_path <- clean_pkg(organdb_path, removal="_", replace="", sqlite=FALSE)
  organdb_path <- clean_pkg(organdb_path, removal="_like", replace="like", sqlite=FALSE)

  if (isTRUE(copy_s3)) {
    s3_file <- entry[["OrganismdbiFile"]]
    copied <- copy_s3_file(src_dir=organdb_path, type="organismdbi", s3_file=s3_file)
    if (isTRUE(copied)) {
      message("Successfully copied the organismdbi map to the s3 staging directory.")
    }
  }

  if (isTRUE(installp)) {
    if (class(organdb) == "list") {
      inst <- try(devtools::install(organdb_path))
      if (class(inst) != "try-error") {
        built <- try(devtools::build(organdb_path, quiet=TRUE))
        if (class(built) != "try-error") {
          final_deleted <- unlink(x=organdb_path, recursive=TRUE, force=TRUE)
        }
      }
    }
    final_organdb_name <- basename(organdb_path)
    final_organdb_path <- move_final_package(organdb_path, type="organismdbi", workdir=workdir)
  }

  retlist <- list(
    "orgdb_name" = orgdb_name,
    "txdb_name" = txdb_name,
    "organdb_name" = final_organdb_name)
  tt <- unloadNamespace(orgdb_name)
  tt <- unloadNamespace(txdb_name)
  return(retlist)
}
