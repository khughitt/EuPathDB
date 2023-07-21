check_rda <- function(name, entry, overwrite) {
  retlist <- list()
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  savefile <- get_rda_filename(entry, name)
  retlist[["savefile"]] <- savefile

  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("  Delete the file ", savefile, " to regenerate.")
      result <- new.env()
      load(savefile, envir = result)
      retlist[["result"]] <- result[["result"]]
    }
  }
  return(retlist)
}

#' Provide the filename for a temporary rda file for saving data while running.
get_rda_filename <- function(entry, name) {
  possible <- c("annotations", "go", "goslim", "interpro", "linkout",
                "ortholog", "pathway", "pdb", "pubmed")
  if (name == "all") {
    savefiles <- c()
    for (one in possible) {
      a_savefile <- get_rda_filename(entry, one)
      savefiles <- c(savefiles, a_savefile)
    }
    return(savefiles)
  }

  if (! name %in% possible) {
    stop("The table ", name, " is not expected.")
  }

  rdadir <- file.path(build_dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_{name}_table.rda"))
  return(savefile)
}
