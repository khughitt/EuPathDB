#' Create an orgdb SQLite database from the tables in eupathdb.
#'
#' This function has passed through multiple iterations as the preferred
#' method(s) for accessing data in the eupathdb has changed.  It currently uses
#' my empirically defined set of queries against the eupathdb webservices.  As a
#' result, I have made some admittedly bizarre choices when creating the
#' queries.  Check through eupath_webservices.r for some amusing examples of how
#' I have gotten around the idiosyncrasies in the eupathdb.  Final note, I confirmed
#' with Cristina that it is not possible to acquire data specific to a given version
#' of the eupathdb.
#'
#' @param entry If not provided, then species will get this, it contains all the information.
#' @param build_dir Where to put all the various temporary files.
#' @param install Install the resulting package?
#' @param kegg_abbreviation If known, provide the kegg abbreviation.
#' @param reinstall Re-install an already existing orgdb?
#' @param overwrite Overwrite a partial installation?
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @param godb_source Which table to use for the putative union of the GO tables.
#' @return Currently only the name of the installed package.  This should
#'  probably change.
#' @author Keith Hughitt with significant modifications by atb.
#' @export
make_eupath_orgdb <- function(entry, build_dir = "EuPathDB", install = TRUE,
                              kegg_abbreviation = NULL, reinstall = FALSE, overwrite = FALSE,
                              verbose = FALSE, copy_s3 = FALSE, godb_source = NULL) {
  ## Pull out the metadata for this species.
  if ("character" %in% class(entry)) {
    entry <- get_eupath_entry(entry)
  }
  taxa <- make_taxon_names(entry)
  ## Figure out the package name to use: (e.g. "org.Cbaileyi.TAMU.09Q1.v46.eg.db")
  pkgnames <- get_eupath_pkgnames(entry)
  pkgname <- pkgnames[["orgdb"]]
  if (isTRUE(pkgnames[["orgdb_installed"]]) & !isTRUE(reinstall)) {
    message(" ", pkgname, " is already installed.")
    retlist <- list(
      "orgdb_name" = pkgname)
    return(retlist)
  }

  message("Starting creation of ", pkgname, ".")
  ## Create working directory if necessary
  if (!dir.exists(build_dir)) {
    created <- dir.create(build_dir, recursive = TRUE)
  }

  ## If available, get the kegg abbreviation, otherwise do not try to collect kegg annotations.
  do_kegg <- TRUE
  if (is.null(kegg_abbreviation)) {
    kegg_abbreviation <- get_kegg_orgn(glue::glue("{taxa[['genus']]} {taxa[['species']]}"))
    if (length(kegg_abbreviation) == 0) {
      do_kegg <- FALSE
    }
  }

  ## I am almost certain that wrapping these in a try() is no longer necessary.
  gene_table <- try(post_eupath_annotations(entry, build_dir = build_dir, overwrite = overwrite))
  if ("try-error" %in% class(gene_table)) {
    gene_table <- data.frame()
    warning(" Unable to create an orgdb for this species.")
    return(NULL)
  }
  if (nrow(gene_table) == 0) {
    gene_table <- data.frame()
    warning(" Unable to create an orgdb for this species.")
    return(NULL)
  }
  colnames(gene_table)[1] <- "GID"

  ## I do not think you can disable this, the package creation later fails horribly without it.
  gene_table <- remove_eupath_nas(gene_table, "annot")

  ## Get the GO data from the GO and GOSlim tables
  go_table <- data.frame()
  go_table <- try(post_eupath_go_table(entry, build_dir = build_dir, overwrite = overwrite))
  if ("try-error" %in% class(go_table)) {
    go_table <- data.frame()
  }
  goslim_table <- data.frame()
  goslim_table <- try(post_eupath_goslim_table(entry, build_dir = build_dir, overwrite = overwrite))
  if ("try-error" %in% class(goslim_table)) {
    goslim_table <- data.frame()
  }

  ## Gather orthologs
  gene_ids <- gene_table[["GID"]]
  ortholog_table <- data.frame()
  if (!is.null(gene_table[["ANNOT_GENE_ORTHOMCL_NAME"]])) {
    ortholog_table <- gene_table[, c("GID", "ANNOT_GENE_ORTHOMCL_NAME")]
    colnames(ortholog_table) <- c("GID", "ORTHOLOGS_GROUP_ID")
  } else {
    ortholog_table <- as.data.frame(gene_table[, c("GID")])
    ortholog_table[["ORTHOLOGS_GROUP_ID"]] <- ""
    colnames(ortholog_table) <- c("GID", "ORTHOLOGS_GROUP_ID")
  }
  ortholog_table <- try(post_eupath_ortholog_table(entry = entry,
                                                   ortholog_table = ortholog_table,
                                                   build_dir = build_dir,
                                                   gene_ids = gene_ids,
                                                   overwrite = overwrite))
  if ("try-error" %in% class(ortholog_table)) {
    ortholog_table <- data.frame()
  }

  ## Get the PDB table
  pdb_table <- data.frame()
  pdb_table <- try(post_eupath_pdb_table(entry = entry,
                                         build_dir = build_dir,
                                         overwrite = overwrite))
  if ("try-error" %in% class(pdb_table)) {
    pdb_table <- data.frame()
  }

  ## The linkout table for entrez cross references papers.
  linkout_table <- data.frame()
  linkout_table <- try(post_eupath_linkout_table(entry = entry,
                                                 build_dir = build_dir,
                                                 overwrite = overwrite))
  if ("try-error" %in% class(linkout_table)) {
    linkout_table <- data.frame()
  }

  ## The pubmed table for publications.
  pubmed_table <- data.frame()
  pubmed_table <- try(post_eupath_pubmed_table(entry = entry,
                                               build_dir = build_dir,
                                               overwrite = overwrite))
  if ("try-error" %in% class(pubmed_table)) {
    pubmed_table <- data.frame()
  }

  ## Interpro-specific annotations/cross references.
  interpro_table <- data.frame()
  interpro_table <- try(post_eupath_interpro_table(entry = entry,
                                                   build_dir = build_dir,
                                                   overwrite = overwrite))
  if ("try-error" %in% class(interpro_table)) {
    interpro_table <- data.frame()
  }

  ## The pathway data
  pathway_table <- data.frame()
  pathway_table <- try(post_eupath_pathway_table(entry = entry,
                                                 build_dir = build_dir,
                                                 overwrite = overwrite))
  if ("try-error" %in% class(pathway_table)) {
    pathway_table <- data.frame()
  }

  ## KEGG data
  kegg_table <- data.frame()
  if (isTRUE(do_kegg)) {
    kegg_table <- try(load_kegg_annotations(species = taxa[["genus_species"]],
                                            flatten = FALSE,
                                            abbreviation = kegg_abbreviation[1]))
    if ("try-error" %in% class(kegg_table)) {
      kegg_table <- data.frame()
    } else if ("data.frame" %in% class(kegg_table) & nrow(kegg_table) == 0) {
      kegg_table <- data.frame()
    } else {
      colnames(kegg_table) <- glue::glue("KEGGREST_{toupper(colnames(kegg_table))}")
      colnames(kegg_table)[[1]] <- "GID"
    }
    ## I found a problem, KEGG GIDs no longer have the TcCLB in the beginning.
    ## (For example): tcr:397937.5 should be TcCLB.397937.5
    ## Unfortunately, CLBrener is a particularly evil example, since
    ## a lot (approximately 1/2) of GIDs are from the _other_ haplotype...
    k_gid <- kegg_table[["GID"]]
    g_gid <- gene_table[["GID"]]
    found_gids <- sum(k_gid %in% g_gid)
    if (found_gids == 0) {
      message("Attempting to match the kegg GIDs to the EuPathDB GIDs...")
      extra_string <- ""
      count <- 0
      searching <- TRUE
      ## Give it the first 20 genes to try to find a match
      while (searching) {
        count <- count + 1
        if (count > 20) {
          break
        }
        k <- k_gid[count]
        found <- grep(x = g_gid, pattern = k)
        if (sum(found) == 0) {
          next
        } else {
          f <- found[1]
          g <- g_gid[f]
          message("Found a gid: ", g, ".")
          pat <- paste0("^(.+)", k, "$")
          message("Regexing ", pat, " against ", g, ".")
          extra_string <- gsub(pattern = pat,
                               replacement = "\\1",
                               x = g)
          message("The missing string is: ", extra_string)
          ## Finished searching!
          searching <- FALSE
        }
      }
      if (extra_string != "") {
        kegg_table[["GID"]] <- paste0(extra_string, kegg_table[["GID"]])
      }
    } ## End checking for matching GIDs
  } ## End if we should try getting kegg data.

  ## Create the baby table of chromosomes
  chromosome_table <- gene_table[, c("GID", "ANNOT_SEQUENCE_ID")]
  colnames(chromosome_table) <- c("GID", "CHR_ID")
  chromosome_table[["CHR_ID"]] <- as.factor(chromosome_table[["CHR_ID"]])
  type_table <- gene_table[, c("GID", "ANNOT_GENE_TYPE")]
  colnames(type_table) <- c("GID", "GENE_TYPE")

  ## Compile list of arguments for makeOrgPackage call
  pkg_version_string <- format(Sys.time(), "%Y.%m")
  orgdb_args <- list(
    "version" = pkg_version_string,
    "maintainer" = entry[["Maintainer"]],
    "author" = entry[["Maintainer"]],
    "outputDir" = build_dir,
    "tax_id" = as.character(entry[["TaxonomyID"]]),
    "genus" = taxa[["genus"]],
    "species" = glue::glue("{taxa[['species_strain']]}.v{entry[['SourceVersion']]}"),
    ##"goTable" = NULL,
    "goTable" = "godb_xref",
    "gene_info" = gene_table,
    "chromosome" = chromosome_table,
    "type" = type_table
  )

  ## add any non-empty tables, this is sort of our last sanity check before
  ## making the package.
  if (is.null(go_table)) {
    message(" This should not be possible, but the go table is still null.")
  } else if (nrow(go_table) > 0) {
    orgdb_args[["go_table"]] <- go_table
  }

  if (is.null(goslim_table)) {
    message(" This should not be possible, but the goslim table is still null.")
  } else if (nrow(goslim_table) > 0) {
    orgdb_args[["goslim_table"]] <- goslim_table
  }

  if (is.null(interpro_table)) {
    message(" This should not be possible, but the interpro table is still null.")
  } else if (nrow(interpro_table) > 0) {
    orgdb_args[["interpro_table"]] <- interpro_table
  }

  if (is.null(kegg_table)) {
    message(" This should not be possible, but the kegg table is still null.")
  } else if (nrow(kegg_table) > 0) {
    orgdb_args[["kegg_table"]] <- kegg_table
  }

  if (is.null(linkout_table)) {
    message(" This should not be possible, but the linkout table is still null.")
  } else if (nrow(linkout_table) > 0) {
    orgdb_args[["linkout_table"]] <- linkout_table
  }

  if (is.null(ortholog_table)) {
    message(" This should not be possible, but the ortholog table is still null.")
  } else if (nrow(ortholog_table) > 0) {
    orgdb_args[["ortholog_table"]] <- ortholog_table
  }

  if (is.null(pathway_table)) {
    message(" This should not be possible, but the pathway table is still null.")
  } else if (nrow(pathway_table) > 0) {
    orgdb_args[["pathway_table"]] <- pathway_table
  }

  if (is.null(pdb_table)) {
    message(" This should not be possible, but the pdb table is still null.")
  } else if (nrow(pdb_table) > 0) {
    orgdb_args[["pdb_table"]] <- pdb_table
  }

  if (is.null(pubmed_table)) {
    message(" This should not be possible, but the pubmed table is still null.")
  } else if (nrow(pubmed_table) > 0) {
    orgdb_args[["pubmed_table"]] <- pubmed_table
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
      ## This should no longer be needed
      ## First swap out NA to ""
      na_tmp <- orgdb_args[[i]]
      na_set <- is.na(na_tmp)
      na_tmp[na_set] <- ""
      orgdb_args[[i]] <- na_tmp

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

  ## I am reading the AnnotationForge documentation and learning some ways to improve this.
  ## Here is one interesting section (arguments for makeOrgPackage()):
  ## goTable: By default, this is NULL, but if one of your ... data.frames has GO
  ## annotations, then this name will be the name of that argument. When you specify this,
  ## makeOrgPackage will process that data.frame to remove extra GO terms (that
  ## are too new for the current GO.db) and also will generate a table for GOALL
  ## data (based on ancestor terms for each mapping from GO.db). This table will
  ## also be checked to make sure that it has exactly THREE columns, that must be
  ## named GID, GO and EVIDENCE. These must correspond to the gene IDs, GO
  ## IDs and evidence codes respectively. GO IDs should be formatted like this to
  ## work with other DBs in the project: 'GO:XXXXXXX'.
  ##
  ## It appears to me that this is pretty much perfect for the goslim data.
  godb_table <- data.frame()
  if (is.null(godb_source)) {
    message("Setting the godb source to the union of go and goslim.")
    if (nrow(goslim_table) > 0) {
        godb_table <- goslim_table[, c("GID", "GOSLIM_GO_ID")]
        godb_table[["EVIDENCE"]] <- "GOSlim"
      colnames(godb_table) <- c("GID", "GO", "EVIDENCE")
    }
    if (nrow(go_table) > 0) {
      tmp_table <- go_table[, c("GID", "GODB_GO_ID", "GODB_EVIDENCE_CODE")]
      colnames(tmp_table) <- c("GID", "GO", "EVIDENCE")
      godb_table <- rbind(godb_table, tmp_table)
    }
  } else if (godb_source == "goslim") {
    if (nrow(goslim_table) > 0) {
        godb_table <- goslim_table[, c("GID", "GOSLIM_GO_ID")]
        godb_table[["EVIDENCE"]] <- "GOSlim"
      colnames(godb_table) <- c("GID", "GO", "EVIDENCE")
    }
  } else {
    if (nrow(go_table) > 0) {
      godb_table <- go_table[, c("GID", "GO_ID", "GO_EVIDENCE_CODE")]
      colnames(godb_table) <- c("GID", "GO", "EVIDENCE")
      godb_table <- rbind(godb_table, tmp_table)
    }
  }
  if (nrow(godb_table) > 0) {
    godb_idx <- order(godb_table[["GID"]])
    godb_table <- godb_table[godb_idx, ]
    dup_idx <- duplicated(godb_table)
    godb_table <- godb_table[!dup_idx, ]
    godb_table[["EVIDENCE"]] <- as.factor(godb_table[["EVIDENCE"]])
    message("Adding the goTable argument with: ", nrow(godb_table), " rows.")
    orgdb_args[["godb_xref"]] <- godb_table
  }

  ## The following lines are because makeOrgPackage fails stupidly if the directory exists.
  backup_path <- file.path(build_dir, glue::glue("{pkgname}.bak"))
  first_path <- file.path(build_dir, pkgname)
  if (file.exists(backup_path)) {
    message(backup_path, " already exists, deleting it.")
    ## Something which bit me in the ass for file operations in R, always
    ## set a return value and check it.
    ret <- unlink(backup_path, recursive = TRUE)
  }
  if (file.exists(first_path)) {
    message(first_path, " already exists, backing it up.")
    ret <- file.rename(first_path, backup_path)
  }

  ## Now lets finally make the package!
  lib_result <- requireNamespace("AnnotationForge")
  att_result <- try(attachNamespace("AnnotationForge"), silent = TRUE)
  message(" Calling makeOrgPackage() for ", entry[["Species"]])
  orgdb_path <- ""
  if (isTRUE(verbose)) {
    orgdb_path <- try(do.call("makeOrgPackage", orgdb_args))
  } else {
    orgdb_path <- suppressMessages(try(do.call("makeOrgPackage", orgdb_args)))
  }
  if (class(orgdb_path) == "try-error") {
    return(NULL)
  }

  ## Fix name in sqlite metadata table
  dbpath <- file.path(
    orgdb_path, "inst", "extdata", sub(".db", ".sqlite", basename(orgdb_path)))
  ## make sqlite database editable
  Sys.chmod(dbpath, mode = "0644")
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = dbpath)
  ## update SPECIES field
  query <- glue::glue('UPDATE metadata SET value="{entry[["Species"]]}" WHERE name="SPECIES";')

  sq_result <- RSQLite::dbSendQuery(conn = db, query)
  cleared <- RSQLite::dbClearResult(sq_result)
  ## update ORGANISM field
  query <- glue::glue('UPDATE metadata SET value="{entry[["Species"]]}" WHERE name="ORGANISM";')
  sq_result <- RSQLite::dbSendQuery(conn = db, query)
  cleared <- RSQLite::dbClearResult(sq_result)
  ## lock it back down
  Sys.chmod(dbpath, mode = "0444")
  closed <- RSQLite::dbDisconnect(db)

  ## Clean up any strangeness in the DESCRIPTION file
  orgdb_path <- clean_pkg(orgdb_path)

  ## The following 2 lines are only for Tcuzi Esmeraldo-like and NonEsmeraldo-like
  ## Since writing them, I have improved the logic in make_taxon_names() above,
  ## I therefore suspect these lines are not necessary.
  ## FIXME: See if you can remove the following two lines!
  orgdb_path <- clean_pkg(orgdb_path, removal = "_", replace = "")
  orgdb_path <- clean_pkg(orgdb_path, removal = "_like", replace = "like")
  testthat::expect_equal(first_path, orgdb_path)

  if (isTRUE(copy_s3)) {
    s3_file <- entry[["OrgdbFile"]]
    if (file.exists(s3_file)) {
      removed <- file.remove(s3_file)
    }
    copied <- copy_s3_file(src_dir=orgdb_path, type="orgdb", s3_file=s3_file)
    if (isTRUE(copied)) {
      message(" Successfully copied the orgdb sqlite database to the s3 staging directory.")
    } else {
      stop(" Could not copy S3 data.")
    }
  }

  ## And install the resulting package.
  ## I think installing the package really should be optional, but in the case of orgdb/txdb,
  ## without them there is no organismdbi, which makes things confusing.
  if (isTRUE(install)) {
    install_path <- file.path(getwd(), orgdb_path)
    inst <- suppressWarnings(try(devtools::install_local(install_path)))

    if (! "try-error" %in% class(inst)) {
      ## I am tired of reading about unportable filenames here, so adding the suppress.
      built <- try(suppressWarnings(devtools::build(orgdb_path, quiet = TRUE)))
      if (! "try-error" %in% class(built)) {
        final_path <- move_final_package(orgdb_path, type = "orgdb", build_dir = build_dir)
        final_deleted <- unlink(x = orgdb_path, recursive = TRUE, force = TRUE)
      }
    }
  }

  message("Finished creation of ", pkgname, ".")
  ## Probably should return something more useful/interesting than this, perhaps
  ## the dimensions of the various tables in the orgdb?
  return(pkgname)
}
