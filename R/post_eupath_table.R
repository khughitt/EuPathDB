#' Queries one of the EuPathDB APIs using a POST request and returns a
#' dataframe representation of the result.
#' Note: As of 2017/07/13, POST requests are not yet supported on EuPathDB.
#' Note: 2017/07/13 POST queries can only use the new API
#'
#' @param query_body String of additional query arguments
#' @param entry The single metadatum containing the base url of the provider, species, etc.
#' @param table_name The name of the table to extract, this is provided to make
#'   for prettier labeling.
#' @param minutes A timeout when querying the eupathdb.
#' @return list containing response from API request.
#'
#' More information
#' ----------------
#' 1. https://tritrypdb.org/tritrypdb/serviceList.jsp
#' @author Keith Hughitt
#' @export
post_eupath_table <- function(query_body, entry, table_name=NULL, minutes=30) {
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
  api_uri <- glue::glue("https://{provider}.{tld}/{uri_prefix}/service/answer/report")
  body <- jsonlite::toJSON(query_body)
  result <- httr::POST(url=api_uri, body=body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))
  if (result[["status_code"]] == "422") {
    warning("   The provided species does not have a table of weights.")
    return(data.frame())
  } else if (result[["status_code"]] != "200") {
    warning("   An error status code was returned.")
    return(data.frame())
  } else if (length(result[["content"]]) < 100) {
    warning("   A minimal amount of content was returned.")
  }

  result <- httr::content(result, encoding="UTF-8")
  connection <- textConnection(result)
  ## An attempt to work around EOFs in the data.
  ##result <- read.delim(connection, sep="\t")
  result <- read.delim(connection, sep="\t",
                       quote="", stringsAsFactors=FALSE)
  ## If nothing was received, return nothing.
  if (nrow(result) == 0) {
    return(data.frame())
  }

  ## If a column is just 'X', then I think it can go away.
  non_stupid_columns <- colnames(result) != "X"
  result <- result[, non_stupid_columns]

  ## simplify column names, the are downloaded with
  ## annoyingly stupid names like:
  ## > colnames(dat)
  ## [1] "X.Gene.ID."                        "X.pathway_source_id."
  ## [3] "X.Pathway."                        "X.Pathway.Source."
  ## [5] "X.EC.Number.Matched.in.Pathway."   "X.expasy_url."
  ## [7] "X...Reactions.Matching.EC.Number."
  new_colnames <- toupper(colnames(result))
  ## Get rid of dumb X. prefix
  new_colnames <- gsub("^X\\.+", replacement="", x=new_colnames)
  ## Get rid of spurious end .
  new_colnames <- gsub("\\.$", replacement="", x=new_colnames)
  ## Get rid of internal .'s
  new_colnames <- gsub("\\.", replacement="_", x=new_colnames)
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
  close(connection)
  return(result)
}
