#' This peculiar and slow querying of orthologs is due to me crashing the
#' eupathdb web servers.
#'
#' Therefore, I wrote this, which queries one gene at a time.  I think it would
#' be nice to change this to query multiple genes at a time.
#'
#' @param entry Metadata entry.
#' @param gene What gene to query?
#' @param dir Where to put the checkpoint file?
#' @return table of orthologs for our one gene.
get_orthologs_one_gene <- function(entry=NULL, gene="LmjF.01.0010", dir="EuPathDB") {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  provider <- tolower(entry[["DataProvider"]])
  service_directory <- prefix_map(provider)
  question <- "GenesOrthologousToAGivenGene"
  params_uri <- glue::glue(
    "http://{provider}.org/{service_directory}/webservices/GeneQuestions/{question}.wadl")
  result <- xml2::read_html(params_uri)
  test <- rvest::html_nodes(result, "param")
  param_string <- rvest::html_attr(x=test, name="default")[[1]]

  parameters <- list(
    "organism" = jsonlite::unbox(param_string),
    "single_gene_id" = jsonlite::unbox(gene))
  columns <- c("primary_key", "organism", "orthomcl_link",
               "gene_ortholog_number", "gene_paralog_number")
  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox(glue::glue("GeneQuestions.{question}")),
      "parameters" = parameters,
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "includeHeaders" = jsonlite::unbox("true"),
        "attributes" = columns,
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("fullRecord")
    ))
  api_uri <- glue::glue("https://{provider}.org/{service_directory}/service/answer/report")
  body <- jsonlite::toJSON(query_body)
  result <- httr::POST(
                    url=api_uri,
                    body=body,
                    httr::content_type("application/json"))

  if (result[["status_code"]] == "422") {
    warning("There is a missing parameter.")
    return(data.frame())
  } else if (result[["status_code"]] == "400") {
    warning("An invalid format configuration was provided.")
    return(data.frame())
  } else if (result[["status_code"]] != "200") {
    warning("An error status code was returned.")
    return(data.frame())
  }

  cont <- httr::content(result, encoding="UTF-8")
  if (is.null(cont)) {
    return(data.frame())
  }
  entries <- strsplit(
    x=cont, split="\n\n------------------------------------------------------------\n\n")[[1]]
  connection <- textConnection(entries[1])
  stuff <- read.delim(connection, sep="\n", header=FALSE)
  mypattern <- "^(.+?)\\: (.+)?$"
  ## If I am going to make column names, I need first to get the first part of
  ## stuff: otherstuff
  column_names <- gsub(pattern=mypattern, replacement="\\1", x=stuff[["V1"]], perl=TRUE)
  ## Then get rid of any punctuation, as there is a column '# TM domains' -- that is bad.
  column_names <- gsub(pattern="[[:punct:]]", replacement="", x=column_names)
  ## Get rid of any extraneous spaces from removing punctuation, but since I
  ## cannot be certain that there is no punctuation in the middle of words, just
  ## look at the beginning of the strings.
  column_names <- gsub(pattern="^ +", replacement="", x=column_names)
  ## Finally, I do not accept column names with spaces.
  column_names <- gsub(pattern=" ", replacement="_", x=column_names)
  column_names[1] <- "Ortholog_ID"
  information <- data.frame(row.names=1:length(entries))
  for (col in column_names) {
    new_col <- rep(NA, times=length(entries))
    information <- cbind(information, new_col)
  }
  colnames(information) <- column_names
  ## Now fill in the data using the other side of my regular expression.
  for (c in 1:length(entries)) {
    stuff <- read.delim(textConnection(entries[c]), sep="\n", header=FALSE)
    material <- gsub(pattern="^(.+?)\\: (.+)?$", replacement="\\2", x=stuff[["V1"]])
    information[c, ] <- material
  }

  ## remove duplicated rows
  information <- information[!duplicated(information), ]
  ## Now fill in the original ID
  information[["GID"]] <- gene
  ## The dplyr way of moving a column to the front.
  ## information <- information %>% dplyr::select(GID, everything())
  ## The base way of moving a column to the front
  new_order <- c(
    which(colnames(information) == "GID"), which(colnames(information) != "GID"))
  information <- information[, new_order]
  colnames(information) <- toupper(colnames(information))
  close(connection)
  return(information)
}
