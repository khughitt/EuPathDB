#' Extract query-able fields from the EupathDb.
#'
#' This parses the result of a query to Eupath's webservice:
#' 'GenesByMolecularWeight' and uses it to get a list of fields which are
#' acquireable elsewhere.
#'
#' @param webservice Eupathdb, tritrypdb, fungidb, etc...
#' @param excludes List of fields to ignore.
#' @return List of parameters.
<<<<<<< HEAD:R/get_eupath_fields.R
get_eupath_fields <- function(webservice, excludes = NULL) {
=======
get_eupathdb_fields <- function(webservice, excludes=NULL) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/get_eupathdb_fields.R
  if (is.null(excludes)) {
    excludes <- c("dbp_image", "random_int")
  }
  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }
  request_url <- glue::glue(
                         "http://{webservice}.{tld}/webservices/GeneQuestions/GenesByMolecularWeight.wadl")
  request <- curl::curl(request_url)
  result <- xml2::read_xml(request)
  ##close(request)
  fields <- rvest::xml_nodes(result, xpath = '//*[@name="o-fields"]')[[1]] %>%
    xml2::xml_children() %>%
    xml2::xml_attr("value")
  drop_idx <- is.na(fields)
  fields <- fields[!drop_idx]
  drop_idx <- fields == "none"
  fields <- fields[!drop_idx]
  drop_idx <- grepl(pattern = "^pan_", x = fields)
  fields <- fields[!drop_idx]
  exclude_idx <- fields %in% excludes
  fields <- fields[!exclude_idx]
  return(fields)
}
