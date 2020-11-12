get_eupath_gene_types <- function(webservice = NULL) {
  if (is.null(webservice)) {
    webservice <- "fungidb"
  }
  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }
  request_url <- glue::glue("https://{webservice}.{tld}/a/service/record-types/transcript/searches/GenesByGeneType")
  test <- "https://tritrypdb.org/a/service/record-types/transcript/searches/GenesByGeneType"
  request <- curl::curl(request_url)
  result <- jsonlite::fromJSON(request_url)
  vocabulary <- result[["searchData"]][["parameters"]][["vocabulary"]]
  types <- vocabulary[[2]][, 1]
  return(types)
}
