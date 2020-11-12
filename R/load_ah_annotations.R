#' Shortcut for loading annotation data from AnnotationHub, making some EupathDB assumptions.
#'
#' @param species String containing a unique portion of the desired species.
#' @param service Which eupath webservice is desired?
#' @param type Data type to load.
#' @param eu_version Gather data from a specific eupathdb version?
#' @param wanted_fields If not provided, this will gather all columns starting
#'  with 'annot'.
#' @return Big huge data frame of annotation data.
#' @export
load_ah_annotations <- function(species = "Leishmania major strain Friedlin", service = "TriTrypDB",
                                type = "OrgDb", eu_version = NULL, wanted_fields = NULL) {
    ah <- AnnotationHub::AnnotationHub()
    queries <- c()
    if (!is.null(species)) {
        queries <- c(queries, species)
    }
    if (!is.null(service)) {
        queries <- c(queries, service)
    }
    if (!is.null(eu_version)) {
        queries <- c(queries, eu_version)
    }
    q <- AnnotationHub::query(ah, queries)
    meta <- AnnotationHub::mcols(q)
    if (length(q) == 0) {
        stop("No records matched the query: ", toString(queries))
    }
    if (!is.null(type)) {
        meta_idx <- meta[["rdataclass"]] == type
        if (sum(meta_idx) == 0) {
            stop("No records matched the query: ", toString(queries), " and type ", type, ".")
        } else {
            q <- q[meta_idx]
        }
    }
    if (length(q) == 1) {
        meta <- AnnotationHub::mcols(q)
        message("1 record matched the query, using ", rownames(meta), ": ",
                meta[["description"]], ".")
    } else {
        message("More than 1 entry was retrieved:")
        print(meta)
        message("Arbitarily choosing the first.")
        q <- q[[1]]
    }
  org <- load_orgdb_annotations(q, keytype = "gid", fields = wanted_fields)[["genes"]]
  colnames(org) <- gsub(pattern = "^annot_", replacement = "", x = colnames(org))
  kept_columns <- !duplicated(colnames(org))
  org <- org[, kept_columns]
  return(org)
}
