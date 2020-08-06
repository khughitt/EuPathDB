#' The new eupath system provides 3 output types for downloading data.  This
#' uses the raw one.
#'
#' For the life of me, I could not figure out how to query the big text tables
#' as the tabular format.  Every query I sent came back telling me I gave it
#' incorrect parameter despite the fact that I was copy/pasting the example
#' given me by the eupathdb maintainers. So, I got mad and asked it for the raw
#' format, and so this function was born.
#'
#' @param entry Annotation entry for a given species
#' @param question Which query to try?  Molecular weight is the easiest, as it
#'  was their example.
#' @param parameters Query parameters when posting
#' @param table_name Used to make sure all columns are unique by prefixing them
#'  with the table name.
#' @param columns Columns for which to ask.
#' @param minutes How long to wait until giving up and throwing an error.
#' @return A hopefully huge table of eupath data.
<<<<<<< HEAD:R/post_eupath_raw.R
post_eupath_raw <- function(entry, question = "GeneQuestions.GenesByMolecularWeight",
=======
post_eupathdb_raw <- function(entry, question = "GeneQuestions.GenesByMolecularWeight",
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
                            parameters = NULL, table_name = NULL, columns = NULL,
                            minutes = 10) {
  species <- entry[["TaxonUnmodified"]]
  provider <- tolower(entry[["DataProvider"]])

  ## determine appropriate prefix to use
  uri_prefix <- prefix_map(provider)

<<<<<<< HEAD:R/post_eupath_raw.R
  ## Set default search parameters
=======
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
  if (is.null(parameters)) {
    parameters <- list("organism" = jsonlite::unbox(species))
  }

  ## If the user does not ask for specific columns, get them all!
  query_columns <- columns
  if (is.null(columns)) {
    query_columns <- get_eupathdb_fields(provider)
  }

  ## Define the 3 pieces of a eupathdb json query and send it.
  answerlist <- list(
    "questionName" = jsonlite::unbox(question),
    "parameters" = parameters,
    "viewFilters" = list(),
    "filters" = list())
  formattinglist <- list(
    "formatConfig" = list(
      "includeHeaders" = jsonlite::unbox("true"),
      "attributes" = query_columns,
      "attachmentType" = jsonlite::unbox("plain")),
    "format" = jsonlite::unbox("fullRecord"))
  query_body <- list(
    "answerSpec" = answerlist,
    "formatting" = formattinglist)
  body <- jsonlite::toJSON(query_body)

  ## Figure out the appropriate url and query
  tld <- "org"
  if (provider == "schistodb") {
    tld <- "net"
  }

  # construct query URI
  api_uri <- glue::glue("https://{provider}.{tld}/{uri_prefix}/service/answer/report")
<<<<<<< HEAD:R/post_eupath_raw.R
  result <- httr::POST(url = api_uri, body=body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))

  ## Test the result to see that we actually got data.
  if (result[["status_code"]] == "422") {
    warn(sprintf("API request failed for %s (code = 422): ", entry[["Taxon"]]))
=======

  # submit POST request using JSON 
  result <- httr::POST(url = api_uri, body = body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))

  # check response status code to make sure request succeeded
  if (result[["status_code"]] == "422") {
    warning(sprintf("   The provided species (%s) does not have a table of weights.", species))
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
    return(data.frame())
  } else if (result[["status_code"]] == "400") {
    ## likely due to bad formatConfig
    warn(sprintf("API Request failed for %s (code = 400): ", entry[["Taxon"]]))
  } else if (result[["status_code"]] == "404") {
    warn(sprintf("API Request failed for %s (code = 404): ", entry[["Taxon"]]))
  } else if (result[["status_code"]] != "200") {
    warn(sprintf("API Request failed for %s (code = %d): ",
                 entry$Taxon, result[["status_code"]]))
    return(data.frame())
  } else if (length(result[["content"]]) < 100) {
    warn("Very small amount of content returned for :", entry[["Taxon"]])
  }

  cont <- httr::content(result, encoding = "UTF-8")
  ## Sadly, most of that stuff is completely unwanted.  This is because we are
  ## using the 'fullRecord' format, as it is the only format I have been able to
  ## get to work so far. This format is newline separated fields with entries
  ## separated by 4 returns with dashes... Ergo the following line, which
  ## separates the entries by the dashes/returns into individual strings with
  ## the newlines remaining inside them.  So we will need to use some regular
  ## expressions in order to extract the column names and data.
  entries <- strsplit(
    x = cont, split = "\n\n------------------------------------------------------------\n\n")[[1]]
<<<<<<< HEAD:R/post_eupath_raw.R

=======
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
  ## We will read the first entry in order to extract the column names.
  connection <- textConnection(entries[1])
  a_row <- read.delim(connection, sep = "\n", header = FALSE, quote = "", stringsAsFactors = FALSE)
  a_row[["V1"]] <- as.character(a_row[["V1"]])

  ## My regular expression pattern needs to by greedy in the correct places
  ## because for reasons passing all understanding, some fields have colons inside them...
  mypattern <- "^(.+?\\:) (.+)?$"
  ## If I am going to make column names, I need first to get the first part of
  ## stuff: otherstuff
  regex_column_names <- gsub(pattern = mypattern, replacement = "\\1",
                             x = a_row[["V1"]], perl = TRUE)
  ## At least one column is completely nutty, in that it includes return characters inside its data.
  ## 'Cellular localization images:' contains <span>stuff\nstuff\nstuff</span>
  ## which of course causes my read.delim above to think it is three separate columns.
  ## Therefore I will remove it here.
  good_column_idx <- grepl(x = regex_column_names, pattern = ":$")
  regex_column_names <- regex_column_names[good_column_idx]
  ## Get rid of the colons
  regex_column_names <- gsub(pattern = ":", replacement = "", x = regex_column_names)
<<<<<<< HEAD:R/post_eupath_raw.R
  ## No spaces!
  regex_column_names <- gsub(pattern = "\\s+", replacement = "_", x = regex_column_names)
  ## No number signs '#'!
  regex_column_names <- gsub(pattern = "#", replacement = "num", x = regex_column_names)
  ## No dashes allowed!
  regex_column_names <- gsub(pattern = "-", replacement = "_", x = regex_column_names)
  ## Nor slashes!
  regex_column_names <- gsub(pattern = "/", replacement = "", x = regex_column_names)
  ## No parens!
  regex_column_names <- gsub(pattern = "\\(|\\)", replacement = "", x = regex_column_names)
  ## No single-quotes!  These are used for 5' and 3', so I am replacing it with 'p'.
=======
  ## spaces
  regex_column_names <- gsub(pattern = "\\s+", replacement = "_", x = regex_column_names)
  ## number signs '#'
  regex_column_names <- gsub(pattern = "#", replacement = "num", x = regex_column_names)
  ## dashes
  regex_column_names <- gsub(pattern = "-", replacement = "_", x = regex_column_names)
  ## slashes
  regex_column_names <- gsub(pattern = "/", replacement = "", x = regex_column_names)
  ## parens
  regex_column_names <- gsub(pattern = "\\(|\\)", replacement = "", x = regex_column_names)
  ## and single-quotes
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
  regex_column_names <- gsub(pattern = "'", replacement = "p", x = regex_column_names)

  if (length(query_columns) == length(regex_column_names)) {
    column_names <- query_columns
  } else {
    column_names <- make.names(regex_column_names, unique = TRUE)
  }

  ## Create an empty data frame into which we will dump the text.
  column_names[1] <- "GID"
  information <- data.frame(row.names = 1:length(entries), stringsAsFactors = FALSE)
  for (col in column_names) {
    new_col <- rep(NA, times = length(entries))
    information <- cbind(information, new_col)
  }
  colnames(information) <- column_names

  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style = 3)
  }

  ## Now fill in the data using the other side of my regular expression.
  for (c in 1:length(entries)) {
    if (isTRUE(show_progress)) {
      pct_done <- c / length(entries)
      setTxtProgressBar(bar, pct_done)
    }
    a_row <- read.delim(textConnection(entries[c]), sep = "\n", header = FALSE,
                        quote = "", stringsAsFactors = FALSE)
    a_row[["V1"]] <- as.character(a_row[["V1"]])
    test_error <- grepl(pattern = "\\*\\*\\* ERROR \\*\\*\\*", x = a_row)
    if (isTRUE(test_error)) {
      warning("   Downloading the annotation data failed.")
      next
    }
<<<<<<< HEAD:R/post_eupath_raw.R

=======
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_raw.R
    material <- gsub(pattern = mypattern, replacement = "\\2", x = a_row[["V1"]])
    information[c, ] <- material
  }
  if (isTRUE(show_progress)) {
    close(bar)
  }

  ## remove duplicated rows
  information <- information[!duplicated(information), ]
  ## In some cases we will want to prefix the columns with the table name...
  if (!is.null(table_name)) {
    for (c in 2:length(colnames(information))) {
      col_name <- colnames(information)[c]
      prefix_string <- glue::glue("{toupper(table_name)}_")
      ## Use if() test this to avoid column names like 'GO_GO_ID'
      foundp <- grepl(pattern = glue::glue("^{prefix_string}"), x = toupper(col_name))
      if (!foundp) {
        new_col <- glue::glue("{toupper(table_name)}_{toupper(col_name)}")
        colnames(information)[c] <- new_col
      }
    }
  }
  close(connection)
  return(information)
}
