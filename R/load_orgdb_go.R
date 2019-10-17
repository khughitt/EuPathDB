#' Retrieve GO terms associated with a set of genes.
#'
#' AnnotationDbi provides a reasonably complete set of GO mappings between gene
#' ID and ontologies.  This will extract that table for a given set of gene
#' IDs.
#'
#' Tested in test_45ann_organdb.R
#' This is a nice way to extract GO data primarily because the Orgdb data sets
#' are extremely fast and flexible, thus by changing the keytype argument, one
#' may use a lot of different ID types and still score some useful ontology data.
#'
#' @param orgdb OrganismDb instance.
#' @param gene_ids Identifiers of the genes to retrieve annotations.
#' @param keytype  The mysterious keytype returns yet again to haunt my dreams.
#' @param columns  The set of columns to request.
#' @return Data frame of gene IDs, go terms, and names.
#' @seealso \pkg{AnnotationDbi} \pkg{GO.db} \pkg{magrittr}
#'  \code{\link[AnnotationDbi]{select}} \code{\link[dplyr]{tbl_df}}
#' @examples
#' \dontrun{
#'  go_terms <- load_go_terms(org, c("a","b"))
#' }
#' @author I think Keith provided the initial implementation of this, but atb
#'   messed with it pretty extensively.
#' @export
load_orgdb_go <- function(orgdb=NULL, gene_ids=NULL, keytype="gid",
                          columns=c("go", "evidence")) {
  if (is.null(orgdb)) {
    message("Assuming Homo.sapiens.")
    org_pkgstring <- "library(Homo.sapiens); orgdb <- Homo.sapiens"
    eval(parse(text=org_pkgstring))
  } else if ("character" %in% class(orgdb)) {
    org_pkgstring <- glue::glue("library({orgdb}); orgdb <- {orgdb}")
    eval(parse(text=org_pkgstring))
  }
  tt <- requireNamespace("GO.db")
  keytype <- toupper(keytype)
  columns <- toupper(columns)
  if (is.null(gene_ids)) {
    gene_ids <- try(AnnotationDbi::keys(orgdb, keytype=keytype), silent=TRUE)
    if (class(gene_ids) == "try-error") {
      avail_types <- AnnotationDbi::keytypes(orgdb)
      if ("GID" %in% avail_types) {
        message("The chosen keytype was not available.  Using 'GID'.")
        keytype <- "GID"
        gene_ids <- AnnotationDbi::keys(orgdb, keytype=keytype)
      } else {
        keytype <- avail_types[[1]]
        message("Neither the chosen keytype, nor 'GID' was available.
The available keytypes are: ", toString(avail_types), "choosing ", keytype, ".")
        gene_ids <- AnnotationDbi::keys(orgdb, keytype=keytype)
      }
    }
  }
  if (class(orgdb)[[1]] == "OrganismDb") {
    message("This is an organismdbi, that should be ok.")
  } else if (class(orgdb)[[1]] == "OrgDb" | class(orgdb)[[1]] == "orgdb") {
    message("This is an orgdb, good.")
  } else {
    stop("This requires either an organismdbi or orgdb instance, not ", class(orgdb)[[1]])
  }
  available_columns <- AnnotationDbi::columns(orgdb)
  chosen_columns <- c()
  for (col in columns) {
    if (col %in% available_columns) {
      chosen_columns <- c(chosen_columns, col)
    }
  }
  if (is.null(chosen_columns)) {
    stop("Did not find any of: ", toString(columns),
         " in the set of available columns: ", toString(available_columns))
  }
  go_terms <- try(AnnotationDbi::select(x=orgdb,
                                        keys=gene_ids,
                                        keytype=keytype,
                                        columns=chosen_columns))
  if (class(go_terms) == "try-error") {
    if (grep(pattern="Invalid keytype", x=go_terms[[1]])) {
      message("Here are the possible keytypes:")
      message(toString(AnnotationDbi::keytypes(orgdb)))
      stop()
    }
  }
  ## Deduplicate
  go_terms <- go_terms[!duplicated(go_terms), ]
  if ("GO" %in% chosen_columns) {
    go_terms <- go_terms[!is.na(go_terms[["GO"]]), ]
    go_term_names <- AnnotationDbi::select(x=GO.db::GO.db,
                                           keys=unique(go_terms[["GO"]]),
                                           columns=c("TERM", "GOID", "ONTOLOGY"))
    go_terms <- merge(go_terms, go_term_names, by.x="GO", by.y="GOID")
  }

  ## Remove redundant annotations which differ only in source/evidence
  ## and rename ONTOLOGYALL column
  go_terms <- unique(dplyr::tbl_df(go_terms) %>% stats::na.omit())
  return(go_terms)
}
