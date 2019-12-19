#' Given 2 species names from the eupathdb, make orthology tables betwixt them.
#'
#' The eupathdb provides such a tremendous wealth of information.  For me
#' though, it is difficult sometimes to boil it down into just the bits of
#' comparison I want for 1 species or between 2 species.  A singularly common
#' question I am asked is: "What are the most similar genes between species x
#' and y among these two arbitrary parasites?"  There are lots of ways to poke
#' at this question: run BLAST/fasta36, use biomart, query the ortholog tables
#' from the eupathdb, etc.  However, in all these cases, it is not trivial to
#' ask the next question:  What about: a:b and b:a?
#' This function attempts to address that for the case of two eupath species
#' from the same domain. (tritrypdb/fungidb/etc.)  It does however assume that
#' the sqlite package has been installed locally, if not it suggests you run the
#' make_organismdbi function in order to do that.
#'
#' One other important caveat: this function assumes queries in the format
#' 'table_column' where in this particular instance, the table is further
#' assumed to be the ortholog table.
#'
#' @param db Species name (subset) from one eupath database.
#' @param master Primary keytype to use for indexing the various tables.
#' @param query_species A list of exact species names to search for.  If uncertain
#'   about them, add print_speciesnames=TRUE and be ready for a big blob of
#'   text.  If left null, then it will pull all species.
#' @param id_column What column in the database provides the set of ortholog IDs?
#' @param org_column What column provides the species name?
#' @param url_column What column provides the orthomcl group ID?
#' @param count_column Name of the column with the count of species represented.
#' @param print_speciesnames Dump the species names for diagnostics?
#' @param webservice Which eupathdb project to query?
#' @return A big table of orthoMCL families, the columns are:
#'  \enumerate{
#'   \item  GID: The gene ID
#'   \item  ORTHOLOG_ID: The gene ID of the associated ortholog.
#'   \item  ORTHOLOG_SPECIES: The species of the associated ortholog.
#'   \item  ORTHOLOG_URL: The OrthoMCL group ID's URL.
#'   \item  ORTHOLOG_COUNT: The number of all genes from all species represented in
#'   this group.
#'   \item  ORTHOLOG_GROUP: The family ID
#'   \item  QUERIES_IN_GROUP: How many of the query species are represented in this
#'   group?
#'   \item  GROUP_REPRESENTATION: ORTHOLOG_COUNT / the number of possible species.
#'  }
#' @author atb
#' @export
extract_eupath_orthologs <- function(db, master="GID", query_species=NULL,
                                     id_column="ORTHOLOGS_GID",
                                     org_column="ORTHOLOGS_ORGANISM",
                                     url_column="ORTHOLOGS_PRODUCT",
                                     count_column="ORTHOLOGS_COUNT",
                                     print_speciesnames=FALSE,
                                     webservice="eupathdb") {

  load_pkg <- function(name, ...) {
    first_try <- try(do.call("library", as.list(name)), silent=TRUE)
    if (class(first_try) == "try-error") {
      metadata <- download_eupath_metadata(webservice=webservice)
      entry <- get_eupath_entry(name)
      pkg_names <- get_eupath_pkgnames(entry)
      first_pkg <- pkg_names[["orgdb"]]
      tt <- try(do.call("library", as.list(first_pkg)), silent=TRUE)
      if (class(tt) == "try-error") {
        message("Did not find the package: ",
                first_pkg,
                ". Will not be able to do reciprocal hits.")
        message("Perhaps try invoking make_eupath_organismdbi().")
        pkg <- NULL
      } else {
        message("Loaded: ", first_pkg)
        pkg <- get(first_pkg)
      }
      return(pkg)
    } else {
      pkg <- get(name)
      return(pkg)
    }
  }  ## End internal function 'load_pkg()'

  pkg <- NULL
  if (class(db)[1] == "OrgDb") {
    pkg <- db
  } else if ("character" %in% class(db)) {
    pkg <- load_pkg(db)
  } else {
    stop("I only understand orgdbs or the name of a species.")
  }

  columns <- c(id_column, org_column, url_column, count_column)
  gene_set <- AnnotationDbi::keys(pkg, keytype=master)
  column_set <- AnnotationDbi::columns(pkg)
  column_intersect <- columns %in% column_set
  if (sum(column_intersect) == length(columns)) {
    message("Found all the required columns!")
  } else {
    missing_idx <- ! columns %in% column_set
    missing <- columns[missing_idx]
    message("Some columns were missing: ", toString(missing))
    message("Removing them, which may end badly.")
    columns <- columns[column_intersect]
  }
  all_orthos <- AnnotationDbi::select(x=pkg, keytype=master,
                                      keys=gene_set, columns=columns)
  all_orthos[["ORTHOLOGS_GROUP_ID"]] <- gsub(pattern="^.*>(.*)<\\/a>$",
                                            replacement="\\1", x=all_orthos[[url_column]])
  all_orthos[[org_column]] <- as.factor(all_orthos[[org_column]])
  num_possible <- 1
  species_names <- levels(all_orthos[[org_column]])
  if (is.null(query_species)) {
    query_species <- species_names
  } else if (! query_species %in% species_names) {
    warning("Did not find the desired species in the set of all species.")
    query_species <- species_names
  }
  num_possible <- length(species_names)
  message("There are ", num_possible, " possible species in this group.")

  if (isTRUE(print_speciesnames)) {
    print(toString(species_names))
    return(invisible())
  }

  ## Now pull out the species of interest
  found_species <- 0
  for (sp in query_species) {
    if (sp %in% all_orthos[[org_column]]) {
      message("Found species: ", sp)
    } else {
      message("Did not find species: ", sp)
    }
  }
  kept_orthos_idx <- all_orthos[[org_column]] %in% query_species
  kept_orthos <- all_orthos[kept_orthos_idx, ]
  ## The following is not possible if we used the orthologslite table.
  ## In fact, the orthologslite table is (I am realizing) quite a disappointment.
  ## I might remove that query and just force the much slower orthologs table as it
  ## provides much more useful information.
  if (is.null(all_orthos[["ORTHOLOGS_COUNT"]])) {
    kept_orthos_dt <- data.table::as.data.table(kept_orthos)
  } else {
    colnames(kept_orthos) <- c(master, "ORTHOLOGS_ID", "ORTHOLOGS_SPECIES",
                               "ORTHOLOGS_URL", "ORTHOLOGS_COUNT", "ORTHOLOGS_GROUP")
    kept_orthos[["ORTHOLOGS_COUNT"]] <- as.integer(kept_orthos[["ORTHOLOGS_COUNT"]])
    kept_orthos_dt <- data.table::as.data.table(kept_orthos) %>%
      dplyr::group_by_(master) %>%
      dplyr::add_count_(master)
    colnames(kept_orthos_dt) <- c(master, "ORTHOLOGS_ID", "ORTHOLOGS_SPECIES",
                                  "ORTHOLOGS_URL", "ORTHOLOGS_COUNT", "ORTHOLOGS_GROUP",
                                  "QUERIES_IN_GROUP")
    kept_orthos_dt[["ORTHOLOGS_REPRESENTATION"]] <- kept_orthos_dt[["ORTHOLOGS_COUNT"]] / num_possible
    num_queries <- length(query_species)
  }
  return(kept_orthos_dt)
}
