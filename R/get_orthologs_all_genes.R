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
get_orthologs_all_genes <- function(entry=NULL, dir="EuPathDB", gene_ids=NULL,
                                    overwrite=TRUE, species_list=NULL) {
  if (is.null(entry)) {
    stop("Needs an entry from the eupathdb.")
  }
  species <- entry[["TaxonUnmodified"]]
  rdadir <- file.path(dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive=TRUE)
  }

  if (is.null(gene_ids)) {
    ## query body as a structured list
    field_list <- c(
      "primary_key")
    parameters <- list(
      "organism" = jsonlite::unbox(species),
      "min_molecular_weight" = jsonlite::unbox("1"),
      "max_molecular_weight" = jsonlite::unbox("10000000000000000"))
    message("Getting the set of possible genes.")
    result <- post_eupath_raw(entry,
                              question="GeneQuestions.GenesByMolecularWeight",
                              parameters=parameters,
                              columns=field_list)
  }

  if (is.null(species_list)) {
    ## Get the set of species before asking for genes to save queries
    provider <- tolower(entry[["DataProvider"]])
    service_directory <- prefix_map(provider)
    question <- "GenesOrthologousToAGivenGene"
    params_uri <- glue::glue(
                          "http://{provider}.org/{service_directory}/webservices/GeneQuestions/{question}.wadl")
    sp_result <- xml2::read_html(params_uri)
    ## An example of what I want to take the organisms from...
    ## <param name=\"organism\" type=\"xsd:string\" required=\"true\" default=\"\"
    ## repeating=\"true\">\n<doc title=\"prompt\"></doc><doc
    ## title=\"help\"></doc><doc title=\"default\"></doc><doc
    ## title=\"MultiValued\">Provide one or more values. Use comma as a
    ## delimter.</doc><option value=\"Coprinopsis\"><doc
    ## title=\"description\"></doc></option>\n
    orgs_and_crap <- sp_result %>%
      rvest::html_nodes(xpath="//option") %>%
      rvest::html_attrs()
    ## The species list ends at the first instance of the element 'none'
    orgs <- as.character(orgs_and_crap)
    first_none <- which(orgs == "none")[1]
    ## So, keep only the elements 1 until the nth - 1
    species_list <- orgs[1:first_none - 1]
  }

  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
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
  ortho_savefile <- file.path(rdadir, glue::glue("ortho_checkpoint_{entry[['Genome']]}.rda"))
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

    orthos <- get_orthologs_one_gene(entry=entry, gene=gene, species_list=species_list)
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

  colnames(all_orthologs) <- c("GID", "ORTHOLOG_GID", "ORTHOLOG_ORGANISM",
                               "ORTHOLOG_URL", "ORTHOLOG_COUNT", "ORTHOLOG_PARALOG_COUNT")
  all_orthologs[["ORTHOLOG_GROUP_ID"]] <- gsub(pattern="^.*>(.*)<\\/a>$",
                                               replacement="\\1", x=all_orthologs[["ORTHOLOG_URL"]])
  all_orthologs[["ORTHOLOG_URL"]] <- gsub(pattern="^.*href=", replacement="",
                                          x=all_orthologs[["ORTHOLOG_URL"]])
  all_orthologs[["ORTHOLOG_URL"]] <- gsub(pattern="(^.*?)(>.*)$", replacement="\\1",
                                          x=all_orthologs[["ORTHOLOG_URL"]])
  all_orthologs[["ORTHOLOG_ORGANISM"]] <- as.factor(all_orthologs[["ORTHOLOG_ORGANISM"]])

  message("Saving annotations to ", savefile)
  save(all_orthologs, file=savefile)
  return(all_orthologs)
}
