#' Retrieve GO terms associated with a set of genes.
#'
#' AnnotationDbi provides a reasonably complete set of GO mappings between gene
#' ID and ontologies.  This will extract that table for a given set of gene
#' IDs.
#'
#' This is a nice way to extract GO data primarily because the Orgdb data sets
#' are extremely fast and flexible, thus by changing the keytype argument, one
#' may use a lot of different ID types and still score some useful ontology data.
#'
#' @param orgdb OrganismDb instance.
#' @return Data frame of gene IDs, go terms, and names.
#' @seealso \pkg{AnnotationDbi} \pkg{GO.db} \pkg{magrittr}
#'  \code{\link[AnnotationDbi]{select}} \code{\link[dplyr]{tbl_df}}
#' @examples
#' \dontrun{
#'  go_terms <- load_go_terms(org, c("a","b"))
#' }
#' @author I think Keith provided the initial implementation of this, but atb
#'  messed with it pretty extensively.
#' @export
load_orgdb_go <- function(orgdb, table = "go") {
  ## Hey, redo this using attachNamespace, doofus.
  org_pkgstring <- glue::glue("library({orgdb}); orgdb <- {orgdb}")
  eval(parse(text = org_pkgstring))

  query <- glue("SELECT * FROM genes;")
  gids <- DBI::dbGetQuery(BiocGenerics::dbconn(orgdb), query)
  if (table == "go") {
    query <- glue("SELECT * FROM {table};")
  } else {
    query <- glue("SELECT * FROM {table}_table;")
  }
  go <- DBI::dbGetQuery(BiocGenerics::dbconn(orgdb), query)

  godf <- merge(gids, go, by = "_id", all.y = TRUE)
  godf[["_id"]] <- NULL
  return(godf)
}
setGeneric("load_orgdb_go")
