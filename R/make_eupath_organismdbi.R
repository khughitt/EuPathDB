#' Create an organismDbi instance for an eupathdb organism.
#'
#' The primary goal of an organismdbi instance is to provide a series of links
#' between an orgdb, txdb, and other relevant annotation packages
#' (reactome/go/etc).  In its current iteration, this function brings together a
#' couple columns from the orgdb, txdb, GO.db, and reactome.db.
#'
#' @param entry A row from the eupathdb metadataframe.
#' @param version Which version of the eupathdb to use for creating this package?
#' @param dir Directory in which to build the packages.
#' @param reinstall Overwrite existing data files?
#' @param kegg_abbreviation  For when we cannot automagically find the kegg species id.
#' @param exclude_join I had a harebrained idea to automatically set up the
#'   joins between columns of GO.db/reactome.db/orgdb/txdb objects.  This
#'   variable is intended to exclude columns with common IDs that might
#'   multi-match spuriously -- I think in the end I killed the idea though,
#'   perhaps this should be removed or resurrected.
#' @return  The result of attempting to install the organismDbi package.
#' @author  Keith Hughitt, modified by atb.
#' @export
make_eupath_organismdbi <- function(entry=NULL, version=NULL, dir="EuPathDB", reinstall=FALSE,
                                    kegg_abbreviation=NULL, exclude_join="ENTREZID") {
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, version=version)
  pkgname <- pkgnames[["organismdbi"]]
  if (isTRUE(pkgnames[["organismdbi_installed"]]) & !isTRUE(reinstall)) {
    message(pkgname, " is already installed, set reinstall=TRUE if you wish to reinstall.")
    retlist <- list(
      "organdb_name" = pkgname)
    return(retlist)
  }
  orgdb_name <- pkgnames[["orgdb"]]
  txdb_name <- pkgnames[["txdb"]]
  orgdb_ret <- make_eupath_orgdb(entry, version=version, dir=dir,
                                 kegg_abbreviation=kegg_abbreviation, reinstall=reinstall)
  if (is.null(orgdb_ret)) {
    return(NULL)
  }
  txdb_ret <- make_eupath_txdb(entry, version=version, dir=dir, reinstall=reinstall)
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
  ## FIXME Theoretically we should no longer have columns with names like
  ## GO_GO
  orgdb_go_col <- "GO_GO_ID"
  goids_found <- "GOID" %in% AnnotationDbi::keytypes(get("GO.db")) &&
    orgdb_go_col %in% AnnotationDbi::keytypes(get(orgdb_name))
  if (isTRUE(goids_found)) {
    count <- count + 1
    name <- glue::glue("join{count}")
    graph_data[[name]] <- c(GO.db="GOID", orgdb=orgdb_go_col)
    names(graph_data[[name]]) <- c("GO.db", orgdb_name)
  }
  ## FIXME Theoretically we should no longer have columns with names like
  ## PATHWAY_PATHWAY
  required <- requireNamespace("reactome.db")
  required <- try(attachNamespace("reactome.db"), silent=TRUE)
  orgdb_path_col <- "PATHWAY_PATHWAY"
  reactomeids_found <- "REACTOMEID" %in% AnnotationDbi::keytypes(get("reactome.db")) &&
    orgdb_path_col %in% AnnotationDbi::keytypes(get(orgdb_name))
  if (isTRUE(reactomeids_found)) {
    count <- count + 1
    name <- glue::glue("join{count}")
    graph_data[[name]] <- c(reactome.db="REACTOMEID", orgdb=orgdb_path_col)
    names(graph_data[[name]]) <- c("reactome.db", orgdb_name)
  }

  author <- as.character(entry[["Maintainer"]])
  maintainer <- as.character(entry[["Maintainer"]])
  final_dir <- file.path(dir, "organismdbi", pkgname)
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

  tmp_pkg_dir <- file.path(dir)
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
  organdb_path <- clean_pkg(final_dir)
  organdb_path <- clean_pkg(organdb_path, removal="_", replace="", sqlite=FALSE)
  organdb_path <- clean_pkg(organdb_path, removal="_like", replace="like", sqlite=FALSE)
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
  final_organdb_path <- move_final_package(organdb_path, type="organismdbi", dir=dir)
  retlist <- list(
    "orgdb_name" = orgdb_name,
    "txdb_name" = txdb_name,
    "organdb_name" = final_organdb_name)
  tt <- unloadNamespace(orgdb_name)
  tt <- unloadNamespace(txdb_name)
  return(retlist)
}
