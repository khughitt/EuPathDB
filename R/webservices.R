#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite  Overwrite existing data?
#' @param webservice  Optional alternative webservice for hard-to-find species.
#' @param dir  Where to put the json.
#' @param use_savefile  Make a savefile of the data for future reference.
#' @param ...  Catch any extra arguments passed here, currently unused.
#' @return  Dataframe with lots of rows for the various species in eupathdb.
#' @author  Keith Hughitt
#' @export
download_eupath_metadata <- function(overwrite=FALSE, webservice="eupathdb",
                                     dir="eupathdb", use_savefile=TRUE, ...) {
  ## Get EuPathDB version (same for all databases)
  arglist <- list(...)
  savefile <- glue::glue("{webservice}_metadata-v{format(Sys.time(), '%Y%m')}.rda")

  if (!file.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }
  if (isTRUE(use_savefile)) {
    savefile <- file.path(dir, savefile)
    if (isTRUE(overwrite)) {
      file.remove(savefile)
    }
    if (file.exists(savefile)) {
      metadata <- new.env()
      loaded <- load(savefile, envir=metadata)
      metadata <- metadata[["metadata"]]
      return(metadata)
    }
  }

  db_version <- NULL
  if (is.null(arglist[["version"]])) {
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
  } else {
    db_version <- arglist[["version"]]
  }
  .data <- NULL  ## To satisfy R CMD CHECK
  shared_tags <- c("Annotation", "EuPathDB", "Eukaryote", "Pathogen", "Parasite")
  tags <- list(
    "AmoebaDB" = c(shared_tags, "Amoeba"),
    "CryptoDB" = c(shared_tags, "Cryptosporidium"),
    "FungiDB" = c(shared_tags, "Fungus", "Fungi"),
    "GiardiaDB" = c(shared_tags, "Giardia"),
    "MicrosporidiaDB" = c(shared_tags, "Microsporidia"),
    "PiroplasmaDB" = c(shared_tags, "Piroplasma"),
    "PlasmoDB" = c(shared_tags, "Plasmodium"),
    "ToxoDB" = c(shared_tags, "Toxoplasmosis"),
    "TrichDB" = c(shared_tags, "Trichomonas"),
    "TriTrypDB" = c(shared_tags, "Trypanosome", "Kinetoplastid", "Leishmania"))
  tag_strings <- lapply(tags, function(x) {
    paste(x, collapse=",")
  })

  ## construct API request URL
  base_url <- glue::glue("https://{webservice}.org/{webservice}/webservices/")
  query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
  request_url <- glue::glue("{base_url}{query_string}")

  ## retrieve organism metadata from EuPathDB
  metadata_json <- glue::glue("{dir}/metadata.json")
  ## It turns out that not all eupathdb hosts have moved to https...
  file <- try(download.file(url=request_url, destfile=metadata_json), silent=TRUE)
  if (class(file) == "try-error") {
    ## Try again without https?
    message("Downloading the https file failed, not all eupathdb services have migrated to https,
trying http next.")
    base_url <- glue::glue("http://{webservice}.org/{webservice}/webservices/")
    query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
    request_url <- glue::glue("{base_url}{query_string}")
    ## retrieve organism metadata from EuPathDB
    metadata_json <- glue::glue("{dir}/metadata.json")
    file <- download.file(url=request_url, destfile=metadata_json)
  }

  result <- try(jsonlite::fromJSON(metadata_json), silent=TRUE)
  if (class(result)[1] == "try-error") {
    stop("There was a parsing failure when reading the metadata.")
  }
  records <- result[["response"]][["recordset"]][["records"]]
  message("Downloaded: ", request_url)

  ## convert to a dataframe
  dat <- data.frame(t(sapply(records[["fields"]], function(x) {
    x[, "value"] })),
    stringsAsFactors=FALSE)
  colnames(dat) <- records[["fields"]][[1]][["name"]]

  ## shared metadata
  ## I wish I were this confident with %>% and transmute, I always get confused by them
  ## A funny little oddity in the TriTrypdb (20181103)
  ## The current version of the database remains 39; but the sourceUrl returned
  ## by the above json query is 40.  As a result, attempted downloads fail due
  ## to the mismatch in filenames/directories.
  SourceUrl <- NULL  ## Because I still don't get NSE/SE semantics with mutate()!!
  shared_metadata <- dat %>%
    dplyr::transmute(
             "BiocVersion" = as.character(BiocInstaller::biocVersion()),
             "Genome" = sub(".gff", "", basename(.data[["URLgff"]])),
             "NumGenes"=.data[["genecount"]],
             "NumOrthologs" = .data[["orthologcount"]],
             "SourceType" = "GFF",
             "SourceUrl" = .data[["URLgff"]],
             "SourceVersion" = db_version,
             "Species" = .data[["organism"]],
             "TaxonomyId" = .data[["ncbi_tax_id"]],
             "Coordinate_1_based" = TRUE,
             "DataProvider" = .data[["project_id"]],
             "Maintainer" = "Keith Hughitt <khughitt@umd.edu>") %>%
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all,
                     pattern="Current_Release",
                     replacement=glue::glue("release-{db_version}")) %>%
    dplyr::mutate("SourceUrl" = gsub(pattern="DB-(\\d\\d)_",
                                     replacement=glue::glue("DB-{db_version}_"),
                                     x=SourceUrl))

  ## Add project-specific tags for each entry
  shared_metadata[["Tags"]] <- sapply(shared_metadata[["DataProvider"]],
                                      function(x) {
                                        tag_strings[[x]] })

  ## replace missing taxonomy ids with NAs
  shared_metadata[["TaxonomyId"]][shared_metadata[["TaxonomyId"]] == ""] <- NA

  ## overide missing taxonomy ids for strains where it can be assigned; ideally
  ## OrgDb and GRanges objects should not depend on taxonomy id information since
  ## this precludes the inclusion of a lot of prokaryotic resources.
  known_taxon_ids <- data.frame(
    species=c("Ordospora colligata OC4",
              "Trypanosoma cruzi CL Brener Esmeraldo-like",
              "Trypanosoma cruzi CL Brener Non-Esmeraldo-like"),
    taxonomy_id=c("1354746", "353153", "353153"),
    stringsAsFactors=FALSE)

  taxon_mask <- shared_metadata[["Species"]] %in% known_taxon_ids[["species"]]
  ind <- match(shared_metadata[taxon_mask, "Species"], known_taxon_ids[["species"]])
  shared_metadata[taxon_mask, ][["TaxonomyId"]] <- as.character(
    known_taxon_ids[["taxonomy_id"]][ind])

  ## exclude remaining species which are missing taxonomy information from
  ## metadata; cannot construct GRanges/OrgDb instances for them since they are
  ## have no known taxonomy id, and are not in available.species()
  na_ind <- is.na(shared_metadata[["TaxonomyId"]])
  ## I think I will try to hack around this problem.
  shared_metadata[["TaxonomyId"]] <- as.numeric(shared_metadata[["TaxonomyId"]])

  ## remove any organisms for which no GFF is available
  ## gff_exists <- sapply(shared_metadata[["SourceUrl"]],
  ##                      function(url) { httr::HEAD(url)[["status_code"]] == 200 })
  ## remove any organisms for which no GFF is available
  ## Once again, I will attempt a workaround, probably via bioconductor.
  ## gff_exists <- sapply(shared_metadata$SourceUrl,
  ##                      function(url) { HEAD(url)$status_code == 200 })
  ##shared_metadata <- shared_metadata[gff_exists,]

  ## generate separate metadata table for OrgDB and GRanges targets
  granges_metadata <- shared_metadata %>%
    dplyr::mutate(
             Title=glue::glue("Transcript information for {.data[['Species']]}"),
             Description=glue::glue("{.data[['DataProvider']]} \\
{.data[['SourceVersion']]} transcript information for {.data[['Species']]}}"),
             RDataClass="GRanges",
             DispatchClass="GRanges",
             ResourceName=sprintf("GRanges.%s.%s%s.rda", gsub("[ /.]+", "_", .data[["Species"]]),
                                  tolower(.data[["DataProvider"]]),
                                  .data[["SourceVersion"]], "rda")) %>%
    dplyr::mutate(DataPath=file.path("EuPathDB", "GRanges",
                                     .data[["BiocVersion"]], .data[["ResourceName"]]))

  metadata <- shared_metadata %>%
    dplyr::mutate(
             "Title"=sprintf("Genome wide annotations for %s", .data[["Species"]]),
             "Description"=sprintf("%s %s annotations for %s",
                                   .data[["DataProvider"]],
                                   .data[["SourceVersion"]],
                                   .data[["Species"]]),
             "RDataClass"="OrgDb",
             "DispatchClass"="SQLiteFile",
             "ResourceName"=sprintf(
               "org.%s.%s.db.sqlite", gsub("[ /.]+", "_", .data[["Species"]]),
               tolower(substring(.data[["DataProvider"]], 1, nchar(.data[["DataProvider"]]) - 2)))
           ) %>%
    dplyr::mutate("RDataPath"=file.path("EuPathDB", "OrgDb",
                                        .data[["BiocVersion"]],
                                        .data[["ResourceName"]]))

  if (isTRUE(use_savefile)) {
    if (isTRUE(overwrite) | !file.exists(savefile)) {
      saved <- save(list="metadata", file=savefile)
    }
  }

  return(metadata)
}

#' Extract query-able fields from the EupathDb.
#'
#' This parses the result of a query to Eupath's webservice:
#' 'GenesByMolecularWeight' and uses it to get a list of fields which are
#' acquireable elsewhere.
#'
#' @param webservice Eupathdb, tritrypdb, fungidb, etc...
#' @param excludes List of fields to ignore.
#' @return List of parameters.
get_eupath_fields <- function(webservice, excludes=NULL) {
  if (is.null(excludes)) {
    excludes <- c("dbp_image", "random_int")
  }
  request_url <- glue::glue(
     "http://{webservice}.org/webservices/GeneQuestions/GenesByMolecularWeight.wadl")
  request <- curl::curl(request_url)
  result <- xml2::read_xml(request)
  fields <- rvest::xml_nodes(result, xpath='//*[@name="o-fields"]')[[1]] %>%
    xml2::xml_children() %>%
    xml2::xml_attr("value")
  drop_idx <- is.na(fields)
  fields <- fields[!drop_idx]
  drop_idx <- fields == "none"
  fields <- fields[!drop_idx]
  drop_idx <- grepl(pattern="^pan_", x=fields)
  fields <- fields[!drop_idx]
  exclude_idx <- fields %in% excludes
  fields <- fields[!exclude_idx]
  return(fields)
}

#' The new eupath system provides 3 output types for downloading data.  This
#' uses the raw one.
#'
#' For the life of me, I could not figure out how to query the big text tables
#' as the tabular format.  Every query I sent came back telling me I gave it
#' incorrect parameter despite the fact that I was copy/pasting the example
#' given me by the eupathdb maintainers. So, I got mad and asked it for the raw
#' format, and so this function was born.
#'
#' @param entry  Annotation entry for a given species
#' @param question  Which query to try?  Molecular weight is the easiest, as it
#'   was their example.
#' @param parameters  Query parameters when posting
#' @param table_name  Used to make sure all columns are unique by prefixing them
#'   with the table name.
#' @param columns  Columns for which to ask.
#' @param minutes  How long to wait until giving up and throwing an error.
#' @return  A hopefully huge table of eupath data.
post_eupath_raw <- function(entry, question="GeneQuestions.GenesByMolecularWeight",
                            parameters=NULL, table_name=NULL, columns=NULL,
                            minutes=40) {
  species <- entry[["Species"]]
  provider <- tolower(entry[["DataProvider"]])
  ## determine appropriate prefix to use
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  uri_prefix <- provider
  if (uri_prefix %in% names(prefix_mapping)) {
    uri_prefix <- prefix_mapping[[uri_prefix]]
  }

  if (is.null(parameters)) {
    parameters <- list("organism" = jsonlite::unbox(species))
  }

  query_columns <- columns
  if (is.null(columns)) {
    query_columns <- get_eupath_fields(uri_prefix)
  }

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
  api_uri <- glue::glue("http://{provider}.org/{uri_prefix}/service/answer/report")
  ## In the past, this post would return me a table of all the data in the
  ## eupathdb for the given species. Unfortunately, now it only gives me back
  ## a few columns, including location, gene product, and a few others.
  result <- httr::POST(url=api_uri, body=body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))
  if (result[["status_code"]] == "422") {
    warning("The provided species does not have a table of weights.")
    return(data.frame())
  } else if (result[["status_code"]] == "400") {
    warning("Status 400 was returned, likely a bad formatConfig.")
  } else if (result[["status_code"]] != "200") {
    warning("An error status code was returned.")
    return(data.frame())
  } else if (length(result[["content"]]) < 100) {
    warning("A minimal amount of content was returned.")
  }

  ## Get the content, this will take a while, as the result from eupathdb might
  ## be > 50 Mb of stuff.
  cont <- httr::content(result, encoding="UTF-8")
  ## Sadly, most of that stuff is completely unwanted.  This is because we are
  ## using the 'fullRecord' format, as it is the only format I have been able to
  ## get to work so far. This format is newline separated fields with entries
  ## separated by 4 returns with dashes... Ergo the following line, which
  ## separates the entries by the dashes/returns into individual strings with
  ## the newlines remaining inside them.  So we will need to use some regular
  ## expressions in order to extract the column names and data.
  entries <- strsplit(
    x=cont, split="\n\n------------------------------------------------------------\n\n")[[1]]
  ## We will read the first entry in order to extract the column names.
  entry <- read.delim(textConnection(entries[1]), sep="\n", header=FALSE)
  entry[["V1"]] <- as.character(entry[["V1"]])
  ## My regular expression pattern needs to by greedy in the correct places
  ## because for reasons passing all understanding, some fields have colons inside them...
  mypattern <- "^(.+?\\:) (.+)?$"
  ## If I am going to make column names, I need first to get the first part of
  ## stuff: otherstuff
  regex_column_names <- gsub(pattern=mypattern, replacement="\\1",
                             x=entry[["V1"]], perl=TRUE)
  ## At least one column is completely nutty, in that it includes return characters inside its data.
  ## 'Cellular localization images:' contains <span>stuff\nstuff\nstuff</span>
  ## which of course causes my read.delim above to think it is three separate columns.
  ## Therefore I will remove it here.
  good_column_idx <- grepl(x=regex_column_names, pattern=":$")
  regex_column_names <- regex_column_names[good_column_idx]
  ## Get rid of the colons
  regex_column_names <- gsub(pattern=":", replacement="", x=regex_column_names)
  ## spaces
  regex_column_names <- gsub(pattern="\\s+", replacement="_", x=regex_column_names)
  ## number signs '#'
  regex_column_names <- gsub(pattern="#", replacement="num", x=regex_column_names)
  ## dashes
  regex_column_names <- gsub(pattern="-", replacement="_", x=regex_column_names)
  ## slashes
  regex_column_names <- gsub(pattern="/", replacement="", x=regex_column_names)
  ## parens
  regex_column_names <- gsub(pattern="\\(|\\)", replacement="", x=regex_column_names)
  ## and single-quotes
  regex_column_names <- gsub(pattern="'", replacement="p", x=regex_column_names)

  if (length(query_columns) == length(regex_column_names)) {
    column_names <- query_columns
  } else {
    column_names <- make.names(regex_column_names, unique=TRUE)
  }

  ## Create an empty data frame into which we will dump the text.
  column_names[1] <- "GID"
  information <- data.frame(row.names=1:length(entries))
  for (col in column_names) {
    new_col <- rep(NA, times=length(entries))
    information <- cbind(information, new_col)
  }
  colnames(information) <- column_names

  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style=3)
  }
  ## Now fill in the data using the other side of my regular expression.
  for (c in 1:length(entries)) {
    if (isTRUE(show_progress)) {
      pct_done <- c / length(entries)
      setTxtProgressBar(bar, pct_done)
    }
    entry <- read.delim(textConnection(entries[c]), sep="\n", header=FALSE)
    entry[["V1"]] <- as.character(entry[["V1"]])
    material <- gsub(pattern=mypattern, replacement="\\2", x=entry[["V1"]])
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
      foundp <- grepl(pattern=glue::glue("^{prefix_string}"), x=toupper(col_name))
      if (!foundp) {
        new_col <- glue::glue("{toupper(table_name)}_{toupper(col_name)}")
        colnames(information)[c] <- new_col
      }
    }
  }
  return(information)
}

#' Queries one of the EuPathDB APIs using a POST request and returns a
#' dataframe representation of the result.
#' Note: As of 2017/07/13, POST requests are not yet supported on EuPathDB.
#' Note: 2017/07/13 POST queries can only use the new API
#'
#' @param query_body String of additional query arguments
#' @param species Species name if missing an entry
#' @param entry The single metadatum containing the base url of the provider, species, etc.
#' @param table_name  The name of the table to extract, this is provided to make
#'   for prettier labeling.
#' @param minutes  A timeout when querying the eupathdb.
#' @param ...  Extra arguments for stuff like download_metadtata()
#' @return list containing response from API request.
#'
#' More information
#' ----------------
#' 1. https://tritrypdb.org/tritrypdb/serviceList.jsp
#' @author Keith Hughitt
#' @export
post_eupath_table <- function(query_body, entry, table_name=NULL, minutes=5) {
  if (is.null(entry)) {
    stop("This requires a eupathdb entry.")
  }

  ## determine appropriate prefix to use
  species <- entry[["Species"]]
  provider <- tolower(entry[["DataProvider"]])
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  uri_prefix <- provider
  if (uri_prefix %in% names(prefix_mapping)) {
    uri_prefix <- prefix_mapping[[uri_prefix]]
  }

  ## construct API query
  api_uri <- glue::glue("http://{provider}.org/{uri_prefix}/service/answer/report")
  body <- jsonlite::toJSON(query_body)
  result <- httr::POST(url=api_uri, body=body,
                       httr::content_type("application/json"),
                       httr::timeout(minutes * 60))
  if (result[["status_code"]] == "422") {
    warning("The provided species does not have a table of weights.")
    return(data.frame())
  } else if (result[["status_code"]] != "200") {
    warning("An error status code was returned.")
    return(data.frame())
  } else if (length(result[["content"]]) < 100) {
    warning("A minimal amount of content was returned.")
  }

  result <- httr::content(result, encoding="UTF-8")
  result <- read.delim(textConnection(result), sep="\t")
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
  return(result)
}

#' Gather all available annotation data for a given eupathdb species.
#'
#' This function fills in the parameters to post_eupath_raw() so that one can
#' download all the available data for a given parasite into one massive table.
#' It should also provide some constraints to the data rather than leaving it
#' all as characters.  Caveat:  I manually filled in the list 'field_list' to
#' include the variable names and their text associations.  This is likely to
#' change in future releases of the tritrypdb.  It is probably possible to
#' automagically fill it in.  In addition, I am using GenesByMolecularWeight to
#' get the data, which is a bit weird.
#'
#' @param species  guess.
#' @param entry  The full annotation entry.
#' @param dir  FIXME: I want to write some intermediate data to dir in case of
#'   transient error.
#' @param ...  Used for downloading metadata.
#' @return  A big honking table.
post_eupath_annotations <- function(entry=NULL, dir="eupathdb", ...) {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  species <- entry[["Species"]]
  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_annotations.rda"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    result <- new.env()
    load(savefile, envir=result)
    result <- result[["result"]]
    return(result)
  }

  ## query body as a structured list
  ## This list was generated by going to:
  ## view-source:http://tritrypdb.org/webservices/GeneQuestions/GenesByMolecularWeight.wadl
  ## scrolling down to the 'o-fields' section, and writing down the most likely
  ## useful column names. These are written one per line in an attempt to make
  ## looking for new/changed columns from one eupathdb release to the next
  ## easier.

  parameters <- list(
    "organism" = jsonlite::unbox(species),
    "min_molecular_weight" = jsonlite::unbox("1"),
    "max_molecular_weight" = jsonlite::unbox("10000000000000000")
  )
  question <- "GeneQuestions.GenesByMolecularWeight"
  table_name <- "annot"
  result <- post_eupath_raw(entry, question=question,
                            parameters=parameters, table_name=table_name)
  colnames(result) <- tolower(colnames(result))
  numeric_columns <- c(
    "annot_gene_exon_count",
    "annot_gene_transcript_count",
    "annot_gene_ortholog_number",
    "annot_gene_paralog_number",
    "annot_gene_total_hts_snps",
    "annot_gene_hts_nonsynonymous_snps",
    "annot_gene_hts_synonymous_snps",
    "annot_gene_hts_noncoding_snps",
    "annot_gene_hts_stop_codon_snps",
    "annot_gene_hts_nonsyn_syn_ratio",
    "annot_transcript_length",
    "annot_exon_count",
    "annot_cds_length",
    "annot_tm_count",
    "annot_molecular_weight",
    "annot_isoelectric_point",
    "annot_five_prime_utr_length",
    "annot_three_prime_utr_length")
  for (col in numeric_columns) {
    if (!is.null(result[[col]])) {
      result[[col]] <- as.numeric(result[[col]])
    }
  }
  factor_columns <- c(
    "annot_chromosome",
    "annot_gene_type",
    "annot_is_pseudo",
    "annot_strand",
    "annot_tm_count",
    "annot_exon_count")
  for (col in factor_columns) {
    if (!is.null(result[[col]])) {
      result[[col]] <- as.factor(result[[col]])
    }
  }
  colnames(result) <- toupper(colnames(result))

  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}

#'  Use the post interface to get GO data.
#'
#' @param species  guess.
#' @param entry  The full annotation entry.
#' @param dir  FIXME: I want to write some intermediate data to dir in case of
#'   transient error.
#' @param ... Extra options when downloading metadata.
#' @return  A big honking table.
post_eupath_go_table <- function(entry=NULL, dir="eupathdb", ...) {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  species <- entry[["Species"]]
  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_go_table.rda"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    result <- new.env()
    load(savefile, envir=result)
    result <- result[["result"]]
    return(result)
  }

  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = "GOTerms",
        "includeEmptyTables" = jsonlite::unbox("true"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  result <- post_eupath_table(query_body, entry, table_name="go")
  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}

#'  Use the post interface to get ortholog data.
#'
#' @param species  guess.
#' @param entry  The full annotation entry.
#' @param dir  FIXME: I want to write some intermediate data to dir in case of
#'   transient error.
#' @param ... Extra options for downloading metadata.
#' @return  A big honking table.
post_eupath_ortholog_table <- function(entry=NULL, dir="eupathdb") {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  species <- entry[["Species"]]

  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina
  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = "Orthologs",
        ## "includeEmptyTables" = jsonlite::unbox("true"),
        "includeEmptyTables" = jsonlite::unbox("false"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    result <- new.env()
    load(savefile, envir=result)
    result <- result[["result"]]
    return(result)
  }

  result <- post_eupath_table(query_body, entry, table_name="orthologs")

  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}

#'  Use the post interface to get interpro data.
#'
#' @param species  guess.
#' @param entry  The full annotation entry.
#' @param dir  FIXME: I want to write some intermediate data to dir in case of
#'   transient error.
#' @param ... Extra options when downloading metadata.
#' @return  A big honking table.
post_eupath_interpro_table <- function(entry=NULL, dir="eupathdb", ...) {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }

  species <- entry[["Species"]]
  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina
  query_body <- list(
    ## 3 elements, answerSpec, formatting, format.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = "InterPro",
        "includeEmptyTables" = jsonlite::unbox("true"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_interpro_table"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    result <- new.env()
    load(savefile, envir=result)
    result <- result[["result"]]
    return(result)
  }

  result <- post_eupath_table(query_body, entry, table_name="interpro")
  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}

#'  Use the post interface to get pathway data.
#'
#' @param species  guess.
#' @param entry  The full annotation entry.
#' @param dir  FIXME: I want to write some intermediate data to dir in case of
#'   transient error.
#' @param ... Extra options when downloading metadata
#' @return  A big honking table.
post_eupath_pathway_table <- function(entry=NULL, dir="eupathdb", ...) {
  if (is.null(entry)) {
    stop("Need a eupathdb entry.")
  }

  species <- entry[["Species"]]
  ## query body as a structured list
  ## Parameters taken from the pdf "Exporting Data - Web Services.pdf" received
  ## from Cristina
  query_body <- list(
    ## 2 elements, answerSpec, formatting.
    "answerSpec" = list(
      "questionName" = jsonlite::unbox("GeneQuestions.GenesByTaxonGene"),
      "parameters" = list("organism" = jsonlite::unbox(species)),
      "viewFilters" = list(),
      "filters" = list()
    ),
    "formatting" = list(
      "formatConfig" = list(
        "tables" = "MetabolicPathways",
        "includeEmptyTables" = jsonlite::unbox("false"),
        "attachmentType" = jsonlite::unbox("plain")
      ),
      "format" = jsonlite::unbox("tableTabular")
    ))

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_pathway_table.rda"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    result <- new.env()
    load(savefile, envir=result)
    result <- result[["result"]]
    return(result)
  }

  result <- post_eupath_table(query_body, entry, table_name="pathway")

  message("Saving annotations to ", savefile)
  save(result, file=savefile)
  return(result)
}

#' Query ortholog tables from the eupathdb one gene at a time.
#'
#' Querying the full ortholog table at eupathdb.org fails mysteriously.
#' This is a horrible brute-force approach to get around this.
#'
#' @param species What species to query?
#' @param dir Directory to which to save intermediate data (currently unused).
#' @param gene_ids List of gene IDs to query.
#' @param entry An entry from the eupathdb metadata to use for other parameters.
#' @param ... Extra parameters for downloading eupathdb metadata.
#' @export
get_orthologs_all_genes <- function(entry=NULL, dir="eupathdb", gene_ids=NULL) {
  if (is.null(entry)) {
    stop("Needs an entry from the eupathdb.")
  }
  species <- entry[["Species"]]

  if (is.null(gene_ids)) {
    ## query body as a structured list
    field_list <- c(
      "primary_key")
    parameters <- list(
      "organism" = jsonlite::unbox(species),
      "min_molecular_weight" = jsonlite::unbox("1"),
      "max_molecular_weight" = jsonlite::unbox("10000000000000000")
    )
    message("Getting the set of possible genes.")
    result <- post_eupath_raw(entry,
                              question="GeneQuestions.GenesByMolecularWeight",
                              parameters=parameters,
                              columns=field_list)
    ##gene_ids <- result[[1]]
  }

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}ortholog_table.rda"))
  if (file.exists(savefile)) {
    message("We can save some time by reading the savefile.")
    message("Delete the file ", savefile, " to regenerate.")
    all_orthologs <- new.env()
    load(savefile, envir=all_orthologs)
    all_orthologs <- all_orthologs[["savelist"]]
    return(all_orthologs)
  }

  all_orthologs <- data.frame()
  message("Downloading orthologs one gene at a time. Checkpointing if it fails.")
  ortho_savefile <- file.path(dir, glue::glue("ortho_checkpoint_{entry[['Genome']]}.rda"))
  savelist <- list(
    "number_finished" = 0,
    "all_orthologs" = all_orthologs)
  if (file.exists(ortho_savefile)) {
    ortho_progress <- new.env()
    load(ortho_savefile, envir=ortho_progress)
    savelist <- ortho_progress[["savelist"]]
    all_orthologs <- savelist[["all_orthologs"]]
  } else {
    save(savelist, file=ortho_savefile)
  }
  current_gene <- savelist[["number_finished"]] + 1
  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style=3)
  }
  for (i in current_gene:length(gene_ids)) {
    if (isTRUE(show_progress)) {
      pct_done <- i / length(gene_ids)
      setTxtProgressBar(bar, pct_done)
    }
    gene <- gene_ids[i]
    ## I keep getting weird timeouts, so I figure I will give the eupath
    ## webservers a moment.
    Sys.sleep(0.3)
    orthos <- get_orthologs_one_gene(entry=entry, gene=gene,
                                     webservice=webservice)
    all_orthologs <- rbind(all_orthologs, orthos)
    message("Downloading: ", gene, " ", i, "/", length(gene_ids),
            ", and checkpointing to ", ortho_savefile)
    savelist[["all_orthologs"]] <- all_orthologs
    savelist[["number_finished"]] <- i
    save(savelist, file=ortho_savefile)
  }
  if (isTRUE(show_progress)) {
    close(bar)
  }
  message("Saving annotations to ", savefile)
  save(all_orthologs, file=savefile)
  return(all_orthologs)
}

#' This peculiar and slow querying of orthologs is due to me crashing the
#' eupathdb web servers.
#'
#' Therefore, I wrote this, which queries one gene at a time.  I think it would
#' be nice to change this to query multiple genes at a time.
#'
#' @param species What species to query?
#' @param gene What gene to query?
#' @param dir Where to put the checkpoint file?
#' @param entry Metadata entry.
#' @return table of orthologs for our one gene.
get_orthologs_one_gene <- function(entry=NULL, gene="LmjF.01.0010",
                                   dir="eupathdb", ...) {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  species <- entry[["Species"]]
  provider <- tolower(entry[["DataProvider"]])
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  uri_prefix <- provider
  if (uri_prefix %in% names(prefix_mapping)) {
    uri_prefix <- prefix_mapping[[uri_prefix]]
  }

  question <- "GenesOrthologousToAGivenGene"
  params_uri <- glue::glue(
    "http://{provider}.org/{uri_prefix}/webservices/GeneQuestions/{question}.wadl")
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
  api_uri <- glue::glue("http://{provider}.org/{uri_prefix}/service/answer/report")
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
  stuff <- read.delim(textConnection(entries[1]), sep="\n", header=FALSE)
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

  return(information)
}

#' Search KEGG identifiers for a given species name.
#'
#' KEGG identifiers do not always make sense.  For example, how am I supposed to
#' remember that Leishmania major is lmj?  This takes in a human readable string
#' and finds the KEGG identifiers that match it.
#'
#' @param species Search string (Something like 'Homo sapiens').
#' @param short Only pull the orgid?
#' @return Data frame of possible KEGG identifier codes, genome ID numbers,
#'   species, and phylogenetic classifications.
#' @seealso \pkg{RCurl}
#' @examples
#' \dontrun{
#'  fun = get_kegg_orgn('Canis')
#'  ## >     Tid     orgid      species                   phylogeny
#'  ## >  17 T01007   cfa Canis familiaris (dog) Eukaryotes;Animals;Vertebrates;Mammals
#' }
#' @export
get_kegg_orgn <- function(species="Leishmania", short=TRUE) {
  all_organisms <- RCurl::getURL("http://rest.kegg.jp/list/organism")
  org_tsv <- textConnection(all_organisms)
  all <- read.table(org_tsv, sep="\t", quote="", fill=TRUE)
  close(org_tsv)
  colnames(all) <- c("Tid", "orgid", "species", "phylogeny")
  ## Look for the search string in the species column
  candidates <- all[grepl(species, all[["species"]]), ]
  if (isTRUE(short)) {
    candidates <- as.character(candidates[["orgid"]])
  }
  return(candidates)
}

## EOF
