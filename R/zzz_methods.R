## I finally somewhat understand S4 methods.

#' Load orgdb data if a pkglist is provided.
#'
#' @param x The list result of make_eupath_orgdb.
#' @export
setMethod(
  "load_orgdb_annotations", signature = signature(orgdb = "list"),
  definition = function(orgdb = NULL, gene_ids = NULL, include_go = FALSE, keytype = "gid",
                        location_column = "annot_location_text", type_column = "annot_gene_type",
                        name_column = "annot_gene_product", fields = NULL, sum_exon_widths = FALSE) {
    orgdb_name <- orgdb[["orgdb_pkgname"]]
    load_orgdb_annotations(orgdb_name, gene_ids = gene_ids, include_go = include_go,
                           keytype = keytype, location_column = location_column,
                           type_column = type_column, name_column = name_column,
                           fields = fields, sum_exon_widths = sum_exon_widths)
  })


#' Load orgdb GO data if a pkglist is provided.
#'
#' @param x The list result of make_eupath_orgdb.
#' @export
setMethod(
  "load_eupath_go", signature = signature(query = "list"),
  definition = function(query, webservice = "tritrypdb", eu_version = NULL,
                        wanted_fields = NULL, gene_ids = NULL, columns = c("go", "evidence")) {
    orgdb_name <- orgdb[["orgdb_pkgname"]]
    load_eupath_go(orgdb_name, webservice = webservice, eu_version = eu_version,
                   wanted_fields = wanted_fields, gene_ids = gene_ids, columns = columns)
  })

#setGeneric("load_orgdb_go",
#           signature = signature(orgdb = "character"),
#           function(orgdb) standardGeneric("load_orgdb_go"))

#' Load orgdb GO data if a pkglist is provided.
#'
#' @param x The list result of make_eupath_orgdb.
#' @export
setMethod(
  "load_orgdb_go", signature = signature(orgdb = "list"),
  definition = function(orgdb) {
    orgdb_name <- orgdb[["orgdb_pkgname"]]
    load_orgdb_go(orgdb_name)
  })
