#' Query ortholog tables from the eupathdb one gene at a time.
#'
#' Querying the full ortholog table at eupathdb.org fails mysteriously.
#' This is a horrible brute-force approach to get around this.
#'
#' @param entry An entry from the eupathdb metadata to use for other parameters.
#' @param dir Directory to which to save intermediate data (currently unused).
#' @param gene_ids List of gene IDs to query.
#' @param overwrite Overwrite the savefile?
#' @export
get_orthologs_all_genes <- function(entry=NULL, dir="EuPathDB", gene_ids=NULL, overwrite=TRUE) {
  if (is.null(entry)) {
    stop("Needs an entry from the eupathdb.")
  }
  species <- entry[["TaxonUnmodified"]]

  if (is.null(gene_ids)) {
    ## query body as a structured list
    field_list <- c(
      "primary_key")
    parameters <- list(
      "organism" = jsonlite::unbox(species),
      "min_molecular_weight" = jsonlite::unbox("1"),
      "max_molecular_weight" = jsonlite::unbox("10000000000000000")
    )
    message("Getting the set of possible genes.")
    result <- post_eupath_raw(entry,
                              question="GeneQuestions.GenesByMolecularWeight",
                              parameters=parameters,
                              columns=field_list)
    ##gene_ids <- result[[1]]
  }

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("We can save some time by reading the savefile.")
      message("Delete the file ", savefile, " to regenerate.")
      all_orthologs <- new.env()
      load(savefile, envir=all_orthologs)
      all_orthologs <- all_orthologs[["savelist"]]
      return(all_orthologs)
    }
  }

  all_orthologs <- data.frame()
  message("Downloading orthologs one gene at a time. Checkpointing if it fails.")
  ortho_savefile <- file.path(dir, glue::glue("ortho_checkpoint_{entry[['Genome']]}.rda"))
  savelist <- list(
    "number_finished" = 0,
    "all_orthologs" = all_orthologs)
  if (file.exists(ortho_savefile)) {
    ortho_progress <- new.env()
    load(ortho_savefile, envir=ortho_progress)
    savelist <- ortho_progress[["savelist"]]
    all_orthologs <- savelist[["all_orthologs"]]
  } else {
    save(savelist, file=ortho_savefile)
  }
  current_gene <- savelist[["number_finished"]] + 1
  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style=3)
  }
  for (i in current_gene:length(gene_ids)) {
    if (isTRUE(show_progress)) {
      pct_done <- i / length(gene_ids)
      setTxtProgressBar(bar, pct_done)
    }
    gene <- gene_ids[i]
    ## I keep getting weird timeouts, so I figure I will give the eupath
    ## webservers a moment.
    ## Sys.sleep(0.1)
    orthos <- get_orthologs_one_gene(entry=entry, gene=gene)
    all_orthologs <- rbind(all_orthologs, orthos)
    message("Downloading: ", gene, " ", i, "/", length(gene_ids),
            ", and checkpointing to ", ortho_savefile)
    savelist[["all_orthologs"]] <- all_orthologs
    savelist[["number_finished"]] <- i
    save(savelist, file=ortho_savefile)
  }
  if (isTRUE(show_progress)) {
    close(bar)
  }
  message("Saving annotations to ", savefile)
  save(all_orthologs, file=savefile)
  return(all_orthologs)
}
