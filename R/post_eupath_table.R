#' Queries one of the EuPathDB APIs using a POST request.
#'
#' This should return a dataframe representation of one table at the eupathdb.
#' It should also simplify the column names into something a bit more
#' consistent.
#'
#' @param entry The single metadatum containing the base url of the provider, species, etc.
#' @param tables Name of the table at the eupathdb webservice.
#' @param table_name The name of the table in the local SQLite instance. This is provided to make
#'  for prettier labeling.
#' @param minutes A timeout when querying the eupathdb.
#' @return list containing response from API request.
#'
#' @author Keith Hughitt
#' @export
post_eupath_table <- function(entry, tables = "GOTerms", table_name = NULL, minutes = 30) {
  if (is.null(entry)) {
    stop("   This requires a eupathdb entry.")
  }

  ## determine appropriate prefix to use
  provider <- tolower(entry[["DataProvider"]])
  uri_prefix <- prefix_map(provider)
  ## construct API query
  tld <- "org"
  if (provider == "schistodb") {
    tld <- "net"
  }

  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/service/record-types/gene/searches/GenesByTaxonGene/reports/tableTabular")
  species <- entry[["TaxonUnmodified"]]
  query_body <- list(
      "searchConfig" = list(
          "parameters" = list("organism" = jsonlite::unbox(species)),
          "wdkWeight" = jsonlite::unbox(10)),
      "reportConfig" = list(
          "tables" = c(tables),
          "includeHeader" = jsonlite::unbox(TRUE),
          "attachmentType" = jsonlite::unbox("csv")))
  body <- jsonlite::toJSON(query_body)
  result <- httr::POST(url = base_url, body = body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))
  if (result[["status_code"]] == "422") {
    warning("   The provided species does not have the table.")
    return(data.frame())
  } else if (result[["status_code"]] != "200") {
    warning("   An error status code was returned.")
    return(data.frame())
  } else if (length(result[["content"]]) < 100) {
    warning("   A minimal amount of content was returned.")
  }
  cont <- httr::content(result, encoding = "UTF-8", as = "text")
  handle <- textConnection(cont)
  result <- read.csv(handle)

  ## If nothing was received, return nothing.
  if (nrow(result) == 0) {
    return(data.frame())
  }

  new_colnames <- toupper(colnames(result))
    ## Get rid of spurious end .
  new_colnames <- gsub(pattern = "\\.$", replacement = "", x = new_colnames)
  ## Get rid of internal .'s
  new_colnames <- gsub(pattern = "\\.", replacement = "_", x = new_colnames)
  ## Get rid of double _
  new_colnames <- gsub(pattern = "__", replacement = "_", x = new_colnames)
  colnames(result) <- new_colnames
  colnames(result)[1] <- "GID"
  ## remove duplicated rows
  result <- result[!duplicated(result), ]
  if (!is.null(table_name)) {
    for (c in 2:length(colnames(result))) {
      col_name <- colnames(result)[c]
      new_col <- glue::glue("{toupper(table_name)}_{toupper(col_name)}")
      colnames(result)[c] <- new_col
    }
  }
  close(handle)
  return(result)
}
