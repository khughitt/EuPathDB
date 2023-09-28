#' check that the result from a post makes sense.
check_post_result <- function(result) {
    ## Test the result to see that we actually got data.
    if (result[["status_code"]] == "422") {
      warning("All post attempts failed, returning NULL.")
      return(NULL)
    } else if (result[["status_code"]] == "400") {
      ## likely due to bad formatConfig
      warning("Likely problem with the formatConfig.")
      return(NULL)
    } else if (result[["status_code"]] == "404") {
      warning("The URL looks wrong, got a 404.")
      return(NULL)
    } else if (result[["status_code"]] != "200") {
      warning("Some other unknown error.")
      return(NULL)
    } else if (length(result[["content"]]) < 100) {
      warning("Very small amount of content returned.")
      return("next")
    } else {
      cont <- httr::content(result, encoding = "UTF-8", as = "text")
      return(cont)
    }
}

#' Choose a successful organism string when sending a POST
choose_successful_post <- function(post_attempts, base_url) {
  retlist <- list("chosen_post" = NULL,
                  "result" = NULL)
  for (a in seq_along(post_attempts)) {
    attempt_name <- names(post_attempts)[a]
    attempt_string <- post_attempts[[attempt_name]]
    result <- httr::POST(url = base_url, body = attempt_string,
                         httr::content_type("application/json"),
                         httr::timeout(1200))
    if (result[["status_code"]] == "422") {
      message("POST: ", attempt_name,
              " failed, trying again with a differently formatted species.")
      ## Adding a sleep so that I see when there is a failure.
      Sys.sleep(3)
    } else {
      message("POST: ", attempt_name, " worked for this species.")
      retlist[["result"]] <- result
      retlist[["chosen_post"]] <- attempt_string
      retlist[["species"]] <- attempt_name
      return(retlist)
    }
  } ## Ideally we will return before getting here; but if not, then the NULL will get sent.
  return(retlist)
}

#' Create a GET query to try out.
#' Here is the string theoretically expected for a GET:
#' Note that it is also problematic because of its inconsistent use of html encoded quotes and brackets.
#' https://fungidb.org/fungidb/service/record-types/transcript/searches/GenesByTaxon/reports/standard?organism=%5B%22Allomyces%20macrogynus%20ATCC%2038327%22%2C%22Blastocladiomycota%22%5D&reportConfig={"attributes":["primary_key"],"tables":[]}
#'    get_string <- glue(
#'       '{base_url}?organism=%5B%22{species_coded}%22%5D&reportConfig={{"attributes":[{group_string}],"tables":[]}')
#'    result <- httr::GET(url = get_string)
make_get_query <- function() {
  message("Not implemented, I will come back to this the next time posts start failing.")
}

#' Create POST queries to try out.
#'
#' This function was written in an attempt to address some of the peculiarities
#' when POSTING for non-standard eupathdb species.
#' Here are my notes while I was trying to figure out what is going on:
#'
#' Ohhhhh I see the problem, in a fashion similar to the bug with Schizosaccharomyces pombe,
#' the TriTrypDB is not including the '/' symbols in the strain ID when it returns the metadata.
#'
#' Here is the string returned when examining the API help:
#'  'https://fungidb.org/fungidb/app/web-services-help?searchName=GenesByTaxon&weight=10&organism=%5B%22Allomyces%20macrogynus%20ATCC%2038327%22%2C%22Blastocladiomycota%22%5D'
#' So, if I use URLencode, I should get a compatible string?
#'   species_coded <- URLencode(species)
#'   help_url <- glue("https://fungidb.org/fungidb/app/web-services-help?searchName=GenesByTaxon&weight=10&organism={species_coded}")
#' Note that the json printed here is not really... valid.  But unless something very
#' close to it is provided the API will fail.
#' The most problematic part is of course the json escaped quotes.
#' As far as I can tell, my json is exactly identical to their json,
#' but still it does not always work.
#'
#' {
#' "searchConfig": {
#' "parameters": {
#'     "organism": "[\"Allomyces macrogynus ATCC 38327\",\"Blastocladiomycota\"]"
#'   },
#'   "wdkWeight": 10
#' },
#' "reportConfig": {
#'   "attributes": [
#'       "primary_key"
#'       ],
#'   "tables": []
#' }
#' }
#' Looking more into why some species fail, it looks like
#' there is sometimes a mismatch between the species reported by the metadata provided by
#' eupathdb and the 'official' species at the genomeInfoDB/TaxonomyDB
#' In the case of 'Porospora cf. gigantea A', for example, the eupathdb metadata calls it
#' 'Porospora gigantea A' without the 'cf.' -- sadly, if one attempts to use that as the
#' organism ID, everything will fail.
#'
#' FIXME: This should be changed to only test the 'searchConfig' portion of the body.
#' Then the successful searchConfig may be passed to the other post_eupath_table() calls.
#'
#' The help documentation at cryptodb does not put [] around the species; but it still doesn't work.
#' even copy/pasting their stupid example.
#' Interestingly, when I tested using another species (Gregarina niphandrodes Unknown), the GET/POST strings returned
#' _do_ have the [] along with escaped quotation marks as per post_json_v1.
#' __AND__ it appended to that list the genus: \"Gregarinidae\"
#' post_json_scalar <- glue('{{
#'  "searchConfig": {{
#'    "parameters": {{
#'      "organism": "{species}"
#'    }},
#'    "wdkWeight": 10
#'  }},
#'  "reportConfig": {{
#'    "attributes": [ {group_string} ],
#'    "tables": []
#'  }}
#' }}')
make_post_attempts <- function(attempts, search) {
  retlist <- list()
  for (a in attempts) {
    retlist[[a]] <- glue('{{
  "searchConfig": {{
    "parameters": {{
      "organism": "[\\"{a}\\"]"
    }},
    "wdkWeight": 10
  }},
  "reportConfig": {{
    "attributes": [ {search} ],
    "tables": []
  }}
}}')
  }
  return(retlist)
}

#' Create a series of POST requests which download all the annotation data for a species.
#'
#' The only way I have figured out how to download mass data from the eupathdb
#' is to ask for a raw dump of all available data using the GenesByGeneType
#' WADL.  Therefore, this function iterates over the various sequence types that
#' I have noticed at the eupathdb and does that for each type.
#'
#' @param entry Eupathdb annotation entry.
#' @param build_dir Location to dump the resulting data.
#' @param overwrite Overwrite existing data if it exists?
post_eupath_annotations <- function(entry = NULL, overwrite = FALSE,
                                    verbose = FALSE, split = 13) {
  rda <- check_rda("annotations", entry, overwrite)
  savefile <- rda[["savefile"]]
  if (!is.null(rda[["result"]])) {
    if (isTRUE(verbose)) {
      message("Returning primary annotation the data from a previous savefile.")
    }
    return(rda[["result"]])
  }

  ## query body as a structured list
  ## This list was generated by going to:
  ## view-source:http://tritrypdb.org/webservices/GeneQuestions/GenesByMolecularWeight.wadl
  ## scrolling down to the 'o-fields' section, and writing down the most likely
  ## useful column names.
  ## I later came through and wrote a function function to automagically populate this list.

  ## One might reasonably wonder why I have two species variables here
  ## The first is the one produced by the available metadata from the eupathdb.
  ## The second is from the TaxonomyDB or maybe genomeInfoDB, I get them mixed up, whatever.
  ## It turns out that sometimes the eupathdb returns a slightly mangled string
  ## as the taxonomy name and so, if one were to automatically use that string
  ## to query the database, every query will fail.  In those instances, it appears
  ## that the second string works.  As a result, later on in this function you will note
  ## that I check for a return code '422' and in that instance use the second string as the query.
  species <- entry[["TaxonUnmodified"]]
  species_ah <- entry[["AH_Genus_Species"]]
  species_sanitized <- entry[["Taxon"]]

  webservice <- tolower(entry[["DataProvider"]])
  ## Use a query to find what annotation types are available: protein coding vs. rRNA vs. etc...
  types <- get_eupath_gene_types(webservice = webservice)
  result <- data.frame()

  ## Excepting schistodb, all the services are .orgs which is a .net.
  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }
  ## Finalize the URL to query using the webservice, tld, etc.
  service_directory <- prefix_map(webservice)
  ## download_json <- glue("{build_dir}/{species_filename}.json")
  base_url <- glue("https://{webservice}.{tld}/{service_directory}/service/record-types/transcript/searches/GenesByTaxon/reports/standard")
  wanted_columns <- get_semantic_columns(webservice = webservice)
  ## Split up the set of desired columns into smaller pieces to try to not make the server sad.
  split_columns <- split(wanted_columns, ceiling(seq_along(wanted_columns) / split))
  all_records <- data.table()
  chosen_post <- NULL
  chosen_species <- NULL
  splits <- length(split_columns)
  for (g in seq_len(splits)) {
    group <- split_columns[[g]]
    group_string <- gsub(pattern = " ", replacement = "",
                         x = toString(paste0('"', group, '"')))
    attempts <- c(species, species_ah, species_sanitized)
    post_attempts <- make_post_attempts(attempts, group_string)

    if (is.null(chosen_post)) {
      post_information <- choose_successful_post(post_attempts, base_url)
      chosen_post <- post_information[["chosen_post"]]
      chosen_species <- post_information[["species"]]
      result <- post_information[["result"]]
      if (is.null(result)) {
        warning("No POST attempts were able to get data.")
        return(NULL)
      }
    } else {
      attempt_string <- post_attempts[[chosen_species]]
      result <- httr::POST(url = base_url, body = attempt_string,
                           httr::content_type("application/json"),
                           httr::timeout(1200))
    }

    cont <- check_post_result(result)
    if (is.null(cont)) {
      return(NULL)
    } else if (cont == "next") {
      next
    }
    removed <- rm(list = "result")
    json_result <- try(jsonlite::fromJSON(cont, flatten = TRUE))
    if ("try-error" %in% class(json_result)) {
      warning("Failed to convert result to json.")
      return(NULL)
    }
    removed <- rm(list = "cont")
    ## Every record contains an id, some fields, and tables.
    records <- json_result[["records"]]
    removed <- rm(list = "json_result")
    colnames(records) <- gsub(pattern = "^attributes\\.", replacement = "",
                              x = colnames(records))
    colnames(records) <- gsub(pattern = "\\.", replacement = "_",
                              x = colnames(records))
    records <- expand_list_columns(records)
    ## Drop some annoying columns
    records[["recordClassName"]] <- NULL
    ## Drop duplicate IDs
    dups <- duplicated(records[["displayName"]])
    keepers <- !dups
    if (sum(dups) > 0) {
      message("This surprises me, but there were ", sum(dups),
              " dropped duplicate displayNames.")
    }
    records <- records[keepers, ]

    if (g == 1) {
      all_records <- data.table(records)
      ## "displayName" "overview" "gene_location_text" "gene_name" "organism" "
      ## "gene_transcript_count" "lc_project_id" "gene_exon_count" "chromosome"
      ## "primary_key" "gene_type" "project_id" "is_deprecated" "gene_source_id" "transcript_link"
      ## "sequence_id" "is_pseudo" "snpoverview" "gene_product" "source_id" "gene_ortholog_number"
    } else {
      ## shared_columns <- colnames(records) %in% colnames(all_records)
      ## records[, shared_columns] <- NULL
      records[["gene_source_id"]] <- NULL
      records[["source_id"]] <- NULL
      records[["project_id"]] <- NULL
      records <- data.table(records)
      all_records <- merge(all_records, records, by = "displayName")
    }
    ## This sleep is to give the webservers a moment of peace between requests.
    ## I found that some of them were not returning data after ~ the 4-5th request without it.
    if (isTRUE(verbose)) {
      message("Finished POST number ", g, ".")
    }
    snooze <- Sys.sleep(1)
  } ## End of my nasty hack to get around some webservices crashing
  ## when I ask for all the columns.
  message("Finished looping over subgroups of columns.")
  records <- all_records
  removed <- rm(list = "all_records")
  colnames(records) <- paste0("annot_", colnames(records))

  ## Change entries which say 'N/A' to the actual NA value.
  ## I therefore need to make an index of the various strings I have found
  ## which mean NA and then check for the actual NAs in the data.
  ## The resulting index of NA-like values will sadly contain a bunch of NAs,
  ## so set those to FALSE and then set what is left to NA.
  na_idx <- records == "N/A" | records == "NA" | grepl(x = records, pattern = "^#N/A")
  false_idx <- is.na(records)
  na_idx[false_idx] <- FALSE
  records[na_idx] <- NA

  records <- utils::type.convert(records, as.is = FALSE)
  ## type.convert is quite fast, but aggressive when it comes to
  ## setting things to factors. So use a simple heuristic to send some back.
  character_column_idx <- grepl(x = colnames(records),
                                pattern = "cds|sequence|description|text|link")
  recast_columns <- colnames(records)[character_column_idx]
  for (col in recast_columns) {
    records[[col]] <- as.character(records[[col]])
  }
  ## While I am at it, get rid of the links
  link_column_idx <- grepl(x = colnames(records), pattern = "link")
  link_columns <- colnames(records)[link_column_idx]
  for (col in link_columns) {
    records[[col]] <- gsub(x = records[[col]], pattern = "^\\s*<a href=(\"|\\\")", replacement = "")
    records[[col]] <- gsub(x = records[[col]], pattern = "\\s+.*(\"|\\\")>.*", replacement = "")
    records[[col]] <- gsub(x = records[[col]], pattern = "(\"|\\\")>.*", replacement = "")
  }
  message("  Finished sanitizing the annotation data, performing final cleanups.")

  ## Hopefully the data is consistent now
  ## The last thing to do is send the NAs to a contextually sensible value
  ## so that sqlite will not throw an error.
  ## e.g. if a column is numeric, set NA to 0
  ##      if a column is a character, set NA to ""
  for (col_num in seq_along(colnames(records))) {
    cname <- colnames(records)[col_num]
    na_idx <- is.na(records[[col_num]])
    if (is.character(records[[col_num]])) {
      records[na_idx, col_num] <- ""
    } else if (is.numeric(records[[col_num]])) {
      records[na_idx, col_num] <- 0
    }
  }

  ## orgdbs likes uppercase column names
  colnames(records) <- toupper(colnames(records))
  ## Drop a few extra dumb columns
  drop_columns <- c("ANNOT_ORGANISM", "ANNOT_ORGANISM_FULL", "ANNOT_ORGANISM_TEXT",
                    "ANNOT_RECORDCLASSNAME", "ANNOT_PROJECT_ID", "ANNOT_PROJECT",
                    "ANNOT_LC_PROJECT_ID", "ANNOT_GENE_SOURCE_ID", "ANNOT_SOURCE_ID")
  for (d in drop_columns) {
    if (!is.null(records[[d]])) {
      records[[d]] <- NULL
    }
  }

  ## Get rid of duplicated entries
  dup_idx <- duplicated(records)
  if (sum(dup_idx) > 0) {
    message("  Dropped ", sum(dup_idx), " duplicated entries.")
    records <- records[!dup_idx, ]
  }

  colnames(records)[1] <- "GID"
  records[["GID"]] <- as.character(records[["GID"]])
  message("  Saving ", savefile, " with ", nrow(records), " rows and ", ncol(records), " columns.")
  attr(records, "species") <- chosen_species
  result <- records
  save(result, file = savefile)
  return(records)
}
