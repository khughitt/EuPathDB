#' Haha this is nasty, extract interesting data columns from the eupathdb wall of json.
#'
#' @param webservice Where to download from.
#' @param regexes A set of regexes used to prune uninteresting columns.
get_semantic_columns <- function(webservice = "tritrypdb", regexes = NULL) {
  webservice <- tolower(webservice)
  url <- glue::glue("https://{webservice}.org/{webservice}/service/record-types?format=expanded")
  stuff <- httr::GET(url)
  cont <- httr::content(stuff, encoding="UTF-8", as="text")
  result <- jsonlite::fromJSON(cont, flatten=TRUE)
  ## result is a massive dataframe.
  ## result[["attributes"]] is a massive list, each element of this list
  ## contains the metadata for some clickable element in a eupathdb database as a dataframe.
  ## The first of these elements is the largest, and contains the metadata for all the columns
  ## In all of the data tables for transcripts.  The 'name' column of this dataframe is actually
  ## the column name and therefore the thing I care about.
  ## Most of these column names are not of interest, because soooo many of them are TPM values
  ## for the various rnaseq/proteomics/etc data sets.
  all_data_column_names <- result[["attributes"]][[1]][["name"]]
  ## Keeping in mind that I don't want most of these columns, I made the following variable of
  ## regexes to match stuff I don't care about.  I will remove stuff from the bottom us.
  remove_regexes <- c("_model$",  ## gene models from a bunch of experiments I don't understand.
                      "_cds$", ## The cds models from the same experiments.
                      "_image$", ## A few columns of image data end thus.
                      "_int$", ## The random_int column, yeah I am sure that is useful
                      "^uri$", ## I am guessing this is a url to something, who cares
                      "Abbrev$", ## Abbreviations
                      "^gff_",  ## I think these are used to create the gff download files
                      "Temp$",  ## A few columns end in Temp and appear to be useless
                      "_\\d+$",  ## These are the bazillion tpm columns
                      "_graph$",  ## There are a whole bunch of image data columns which end thus.
                      "^JBrowse$"  ## JBrowse is the genome browser they use, I dunno what is in it
                      )
  wanted_column_names <- all_data_column_names
  for (r in 1:length(remove_regexes)) {
    removal <- remove_regexes[r]
    idx <- grepl(x=wanted_column_names, pattern=removal)
    wanted_column_names <- wanted_column_names[!idx]
  }
  return(wanted_column_names)
}
