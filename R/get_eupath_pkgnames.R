#' Generate standardized package names for the various eupathdb species.
#'
#' This is a surprisingly difficult problem.  Many species names in the eupathdb
#' have odd characters in the species suffix which defines the strain ID.  Many
#' of these peculiarities result in packages which are non-viable for
#' installation.  Thus this function attempts to filter them out and result in
#' consistent, valid package names.  They are not exactly the same in format as
#' other orgdb/txdb/etc packages, as I include in them a field for the eupathdb
#' version used; but otherwise they should be familiar to any user of the sqlite
#' based organism packages.
#'
#' The default argument for this function shows the funniest one I have found so
#' far thanks to the hash character in the strain definition.
#'
#' @param entry A metadatum entry.
#' @param version Choose a specific version of the eupathdb, only really useful
#'   when downloading files.
#' @return List of package names and some booleans to see if they have already
#'   been installed.
#' @author atb
#' @export
get_eupath_pkgnames <- function(entry, version=NULL, column="TaxonUnmodified") {
  species <- entry[[column]]
  version_string <- glue::glue(".v{entry[['SourceVersion']]}")
  if (!is.null(version)) {
    version_string <- glue::glue(".v{version}")
  }
  provider <- tolower(entry[["DataProvider"]])
  taxa <- make_taxon_names(entry, column=column)
  first_char <- strsplit(taxa[["genus"]], split="")[[1]][[1]]
  pkg_list <- list(
    "bsgenome" = glue::glue("BSGenome.{taxa[['taxon']]}{version_string}"),
    "bsgenome_installed" = FALSE,
    "granges" = glue::glue("GRanges.{taxa[['taxon']]}{version_string}.rda"),
    "organismdbi" = glue::glue("{provider}.{taxa[['taxon']]}{version_string}"),
    "organismdbi_installed" = FALSE,
    "orgdb" = glue::glue("org.{first_char}{taxa[['species_strain']]}{version_string}.eg.db"),
    "orgdb_installed" = FALSE,
    "txdb" = glue::glue("TxDb.{taxa[['genus']]}.{taxa[['species_strain']]}.\\
                  {entry[['DataProvider']]}{version_string}"),
    "txdb_installed" = FALSE
  )
  inst <- as.data.frame(installed.packages())
  if (pkg_list[["bsgenome"]] %in% inst[["Package"]]) {
    pkg_list[["bsgenome_installed"]] <- TRUE
  }
  if (pkg_list[["organismdbi"]] %in% inst[["Package"]]) {
    pkg_list[["organismdbi_installed"]] <- TRUE
  }
  if (pkg_list[["orgdb"]] %in% inst[["Package"]]) {
    pkg_list[["orgdb_installed"]] <- TRUE
  }
  if (pkg_list[["txdb"]] %in% inst[["Package"]]) {
    pkg_list[["txdb_installed"]] <- TRUE
  }
  return(pkg_list)
}
