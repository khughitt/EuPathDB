#' Create an orgdb SQLite database from the tables in eupathdb.
#'
#' This function has passed through multiple iterations as the preferred
#' method(s) for accessing data in the eupathdb has changed.  It currently uses
#' my empirically defined set of queries against the eupathdb webservices.  As a
#' result, I have made some admittedly bizarre choices when creating the
#' queries.  Check through eupath_webservices.r for some amusing examples of how
#' I have gotten around the idiosyncrasies in the eupathdb.
#'
#' @param entry If not provided, then species will get this, it contains all the information.
#' @param dir Where to put all the various temporary files.
#' @param version Which version of the eupathdb to use for creating this package?
#' @param kegg_abbreviation If known, provide the kegg abbreviation.
#' @param reinstall Re-install an already existing orgdb?
#' @param overwrite Overwrite a partial installation?
#' @param do_go Create the gene ontology table?
#' @param do_orthologs Create the gene ortholog table?
#' @param do_interpro Create the interpro table?
#' @param do_pathway Create the pathway table?
#' @param do_kegg Attempt to create the kegg table?
#' @return Currently only the name of the installed package.  This should
#'   probably change.
#' @author Keith Hughitt with significant modifications by atb.
#' @export
make_eupath_orgdb <- function(entry=NULL, dir="EuPathDB", version=NULL,
                              kegg_abbreviation=NULL, reinstall=FALSE, overwrite=FALSE,
                              do_go=TRUE, do_orthologs=TRUE, do_interpro=TRUE,
                              do_pathway=TRUE, do_kegg=TRUE) {
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  if (class(entry)[1] == "character") {
    entry <- get_eupath_entry(entry)
  }
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, version=version)
  pkgname <- pkgnames[["orgdb"]]
  if (isTRUE(pkgnames[["orgdb_installed"]]) & !isTRUE(reinstall)) {
    message(pkgname, " is already installed, set reinstall=TRUE if you wish to reinstall.")
    retlist <- list(
      "orgdb_name" = pkgname)
    return(retlist)
  }

  if (!file.exists(dir)) {
    created <- dir.create(dir, recursive=TRUE)
  }

  if (is.null(kegg_abbreviation)) {
    kegg_abbreviation <- get_kegg_orgn(glue::glue("{taxa[['genus']]} {taxa[['species']]}"))
    if (length(kegg_abbreviation) == 0) {
      do_kegg <- FALSE
    }
  }

  gene_table <- try(post_eupath_annotations(entry, dir=dir, overwrite=overwrite))
  if (class(gene_table) == "try-error") {
    gene_table <- data.frame()
    warning("Unable to create an orgdb for this species.")
    return(NULL)
  }
  if (nrow(gene_table) == 0) {
    gene_table <- data.frame()
    warning("Unable to create an orgdb for this species.")
    return(NULL)
  }

  go_table <- data.frame()
  if (isTRUE(do_go)) {
    go_table <- try(post_eupath_go_table(entry, dir=dir, overwrite=overwrite))
    if (class(go_table) == "try-error") {
      go_table <- data.frame()
    }
  }

  gene_ids <- gene_table[["GID"]]
  ortholog_table <- data.frame()
  if (class(do_orthologs)[1] == "character" & do_orthologs == "get") {
    ortholog_table <- try(get_orthologs_all_genes(entry=entry, dir=dir, gene_ids=gene_ids))
    if (class(ortholog_table)[1] == "try-error") {
      ortholog_table <- data.frame()
    }
  } else if (isTRUE(do_orthologs)) {
    ortholog_table <- try(post_eupath_ortholog_table(entry=entry, dir=dir, overwrite=overwrite))
    if (class(ortholog_table)[1] == "try-error") {
      ## Try again on the fallback table.
      ortholog_table <- try(post_eupath_ortholog_table(entry=entry, dir=dir,
                                                       overwrite=overwrite, table="Orthologs"))
      if (class(ortholog_table)[1] == "try-error") {
        ortholog_table <- data.frame()
      }
    }
  }

  interpro_table <- data.frame()
  if (isTRUE(do_interpro)) {
    interpro_table <- try(post_eupath_interpro_table(entry=entry, dir=dir, overwrite=overwrite))
    if (class(interpro_table) == "try-error") {
      interpro_table <- data.frame()
    }
  }

  pathway_table <- data.frame()
  if (isTRUE(do_pathway)) {
    pathway_table <- try(post_eupath_pathway_table(entry=entry, dir=dir, overwrite=overwrite))
    if (class(pathway_table) == "try-error") {
      pathway_table <- data.frame()
    }
  }

  kegg_table <- data.frame()
  if (isTRUE(do_kegg)) {
    kegg_table <- try(load_kegg_annotations(species=taxa[["genus_species"]],
                                            flatten=FALSE,
                                            abbreviation=kegg_abbreviation))
    if (class(kegg_table) == "try-error" | nrow(kegg_table) == 0) {
      kegg_table <- data.frame()
    } else {
      colnames(kegg_table) <- glue::glue("KEGGREST_{toupper(colnames(kegg_table))}")
      colnames(kegg_table)[[1]] <- "GID"
    }
  }

  chromosome_table <- gene_table[, c("GID", "ANNOT_SEQUENCE_ID")]
  colnames(chromosome_table) <- c("GID", "CHR_ID")
  type_table <- gene_table[, c("GID", "ANNOT_GENE_TYPE")]
  colnames(type_table) <- c("GID", "GENE_TYPE")

  ## Compile list of arguments for makeOrgPackage call
  version_string <- format(Sys.time(), "%Y.%m")
  orgdb_args <- list(
    "gene_info" = gene_table,
    "chromosome" = chromosome_table,
    "type" = type_table,
    "version" = version_string,
    "author" = entry[["Maintainer"]],
    "maintainer" = entry[["Maintainer"]],
    "tax_id" = as.character(entry[["TaxonomyId"]]),
    "genus" = taxa[["genus"]],
    "species" = glue::glue("{taxa[['species_strain']]}.v{entry[['SourceVersion']]}"),
    "outputDir" = dir)

  ## add non-empty tables
  if (is.null(go_table)) {
    message("This should not be possible, but the go table is still null.")
  } else if (nrow(go_table) > 0) {
    orgdb_args[["go"]] <- go_table
  }
  if (is.null(ortholog_table)) {
    message("This should not be possible, but the ortholog table is still null.")
  } else if (nrow(ortholog_table) > 0) {
    orgdb_args[["orthologs"]] <- ortholog_table
  }
  if (is.null(interpro_table)) {
    message("This should not be possible, but the interpro table is still null.")
  } else if (nrow(interpro_table) > 0) {
    orgdb_args[["interpro"]] <- interpro_table
  }
  if (is.null(pathway_table)) {
    message("This should not be possible, but the pathway table is still null.")
  } else if (nrow(pathway_table) > 0) {
    orgdb_args[["pathway"]] <- pathway_table
  }
  if (is.null(kegg_table)) {
    message("This should not be possible, but the kegg table is still null.")
  } else if (nrow(kegg_table) > 0) {
    orgdb_args[["kegg"]] <- kegg_table
  }

  ## Make sure no duplicated stuff snuck through, or makeOrgPackage throws an error.
  ## Make sure that every GID field is character, too
  ## -- otherwise you get 'The type of data in the 'GID'
  ## columns must be the same for all data.frames.'
  used_columns <- c()
  for (i in 1:length(orgdb_args)) {
    argname <- names(orgdb_args)[i]
    if (class(orgdb_args[[i]])[1] == "data.frame") {
      ## Make sure that the column names in this data frame are unique.
      ## This starts at 2 because the first column should _always_ by 'GID'
      for (cn in 2:length(colnames(orgdb_args[[i]]))) {
        colname <- colnames(orgdb_args[[i]])[cn]
        if (colname %in% used_columns) {
          new_colname <- glue::glue("{toupper(argname)}_{colname}")
          colnames(orgdb_args[[i]])[cn] <- new_colname
          used_columns <- c(used_columns, new_colname)
        } else {
          used_columns <- c(used_columns, colname)
        }
      }
      ## First swap out NA to ""
      na_tmp <- orgdb_args[[i]]
      na_set <- is.na(na_tmp)
      na_tmp[na_set] <- ""
      orgdb_args[[i]] <- na_tmp
      ## Then remove duplicated elements.
      orgdb_dups <- duplicated(orgdb_args[[i]])
      if (sum(orgdb_dups) > 0) {
        tmp <- orgdb_args[[i]]
        tmp <- tmp[!orgdb_dups, ]
        orgdb_args[[i]] <- tmp
      }
      ## Finally, make sure all GID columns are characters
      orgdb_args[[i]][["GID"]] <- as.character(orgdb_args[[i]][["GID"]])
    } ## End checking for data.frames
  }

  ## The following lines are because makeOrgPackage fails stupidly if the directory exists.
  backup_path <- file.path(dir, glue::glue("{pkgname}.bak"))
  first_path <- file.path(dir, pkgname)
  if (file.exists(backup_path)) {
    message(backup_path, " already exists, deleting it.")
    ret <- unlink(backup_path, recursive=TRUE)
  }
  if (file.exists(first_path)) {
    message(first_path, " already exists, backing it up.")
    ret <- file.rename(first_path, backup_path)
  }

  lib_result <- requireNamespace("AnnotationForge")
  att_result <- try(attachNamespace("AnnotationForge"), silent=TRUE)
  message(sprintf("- Calling makeOrgPackage for %s", entry[["Species"]]))
  orgdb_path <- try(do.call("makeOrgPackage", orgdb_args))
  if (class(orgdb_path) == "try-error") {
    return(NULL)
  }

  ## Fix name in sqlite metadata table
  dbpath <- file.path(
    orgdb_path, "inst/extdata", sub(".db", ".sqlite", basename(orgdb_path)))
  message(sprintf("- Fixing sqlite Orgdb sqlite database %s", dbpath))

  ## make sqlite database editable
  Sys.chmod(dbpath, mode="0644")
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=dbpath)
  ## update SPECIES field
  query <- sprintf('UPDATE metadata SET value="%s" WHERE name="SPECIES";', entry[["Species"]])
  sq_result <- RSQLite::dbSendQuery(conn=db, query)
  cleared <- RSQLite::dbClearResult(sq_result)
  ## update ORGANISM field
  query <- sprintf('UPDATE metadata SET value="%s" WHERE name="ORGANISM";', entry[["Species"]])
  sq_result <- RSQLite::dbSendQuery(conn=db, query)
  cleared <- RSQLite::dbClearResult(sq_result)
  ## lock it back down
  Sys.chmod(dbpath, mode="0444")
  closed <- RSQLite::dbDisconnect(db)

  ## Clean up any strangeness in the DESCRIPTION file
  orgdb_path <- clean_pkg(orgdb_path)
  orgdb_path <- clean_pkg(orgdb_path, removal="_", replace="")
  orgdb_path <- clean_pkg(orgdb_path, removal="_like", replace="like")
  testthat::expect_equal(first_path, orgdb_path)
  ## And install the resulting package.
  inst <- try(devtools::install(orgdb_path))
  if (class(inst) != "try-error") {
    built <- try(devtools::build(orgdb_path, quiet=TRUE))
    if (class(built) != "try-error") {
      final_path <- move_final_package(orgdb_path, type="orgdb", dir=dir)
      final_deleted <- unlink(x=orgdb_path, recursive=TRUE, force=TRUE)
    }
  }

  ## Probably should return something more useful/interesting than this, perhaps
  ## the dimensions of the various tables in the orgdb?
  ## return the path to the sqlite database
  retlist <- list(
    "orgdb_name" = pkgname
  )
  return(retlist)
}
