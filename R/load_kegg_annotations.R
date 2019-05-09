#' Create a data frame of pathways to gene IDs from KEGGREST
#'
#' This seeks to take the peculiar format from KEGGREST for pathway<->genes and
#' make it easier to deal with.
#'
#' @param species  String to use to query KEGG abbreviation.
#' @param abbreviation  If you already know the abbreviation, use it.
#' @param flatten  Flatten nested tables?
#' @return  dataframe with rows of KEGG gene IDs and columns of NCBI gene IDs
#'   and KEGG paths.
#' @author atb
#' @export
load_kegg_annotations <- function(species="coli", abbreviation=NULL, flatten=TRUE) {
  chosen <- NULL
  if (!is.null(abbreviation)) {
    species <- NULL
  }
  if (is.null(abbreviation) & is.null(species)) {
    stop("This requires either a species or 3 letter kegg id.")
  } else if (!is.null(abbreviation)) {
    chosen <- abbreviation
  } else {
    ## Then the species was provided.
    abbreviation <- get_kegg_orgn(species)
    if (length(abbreviation) == 0) {
      stop("Unable to find a matching abbreviation for the search: ", species, ".")
    }
    message("The possible abbreviations are: ", toString(abbreviation), ".")
    message("Choosing the first one: ", abbreviation[[1]])
    chosen <- abbreviation[[1]]
  }

  ## Getting a list of genes is easy, as they are unique.
  genes_vector <- try(KEGGREST::keggConv("ncbi-geneid", chosen))
  if (class(genes_vector) == "try-error") {
    if (grepl(pattern="HTTP 400", x=genes_vector)) {
      warning("KEGGREST returned bad request.")
      return(data.frame())
    }
  }
  genes_df <- kegg_vector_to_df(genes_vector, final_colname="ncbi_geneid", flatten=flatten)

  prot_vector <- KEGGREST::keggConv("ncbi-proteinid", chosen)
  prot_df <- kegg_vector_to_df(prot_vector, final_colname="ncbi_proteinid", flatten=flatten)

  uniprot_vector <- KEGGREST::keggConv("uniprot", chosen)
  uniprot_df <- kegg_vector_to_df(uniprot_vector, final_colname="uniprotid", flatten=flatten)

  path_vector <- KEGGREST::keggLink("pathway", chosen)
  path_df <- kegg_vector_to_df(path_vector, final_colname="pathways", flatten=flatten)

  if (isTRUE(flatten)) {
    result <- merge(genes_df, prot_df, by="GID", all=TRUE)
    rownames(result) <- result[["ID"]]
    result <- merge(result, uniprot_df, by="GID", all=TRUE)
    rownames(result) <- result[["ID"]]
    result <- merge(result, path_df, by="GID", all=TRUE)
    rownames(result) <- result[["ID"]]
  } else {
    result <- merge(genes_df, prot_df, by="GID", all=TRUE)
    result <- merge(result, uniprot_df, by="GID", all=TRUE)
    result <- merge(result, path_df, by="GID", all=TRUE)
  }

  result[["ncbi_geneid"]] <- gsub(
    pattern="ncbi-geneid:", replacement="", x=result[["ncbi_geneid"]])
  result[["ncbi_proteinid"]] <- gsub(
    pattern="ncbi-proteinid:", replacement="", x=result[["ncbi_proteinid"]])
  result[["uniprotid"]] <- gsub(pattern="up:", replacement="", x=result[["uniprotid"]])
  result[["pathways"]] <- gsub(pattern="path:", replacement="", x=result[["pathways"]])
  result[["kegg_geneid"]] <- glue::glue("{chosen}:{result[['GID']]}")
  ## Now we have a data frame of all genes <-> ncbi-ids, pathways
  result_nas <- is.na(result)
  result[result_nas] <- ""
  rownames(result) <- make.names(result[["GID"]], unique=TRUE)
  message("Returning KEGGREST annotations.")
  return(result)
}
