## This file should contain any function which directly performs a GET request
## to one of the eupathdb project web servers.

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

  service_directory <- prefix_map(webservice)
  ## construct API request URL
  base_url <- glue::glue("https://{webservice}.org/{service_directory}/webservices/")
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
    base_url <- glue::glue("http://{webservice}.org/{service_directory}/webservices/")
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

#' Query ortholog tables from the eupathdb one gene at a time.
#'
#' Querying the full ortholog table at eupathdb.org fails mysteriously.
#' This is a horrible brute-force approach to get around this.
#'
#' @param entry An entry from the eupathdb metadata to use for other parameters.
#' @param dir Directory to which to save intermediate data (currently unused).
#' @param gene_ids List of gene IDs to query.
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

  savefile <- file.path(dir, glue::glue("{entry[['Genome']]}_ortholog_table.rda"))
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
    Sys.sleep(0.4)
    orthos <- get_orthologs_one_gene(entry=entry, gene=gene)
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
#' @param entry Metadata entry.
#' @param gene What gene to query?
#' @param dir Where to put the checkpoint file?
#' @return table of orthologs for our one gene.
get_orthologs_one_gene <- function(entry=NULL, gene="LmjF.01.0010", dir="eupathdb") {
  if (is.null(entry)) {
    stop("Need an entry from the eupathdb.")
  }
  species <- entry[["Species"]]
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

prefix_map <- function(prefix) {
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  new_prefix <- prefix
  if (prefix %in% names(prefix_mapping)) {
    new_prefix <- prefix_mapping[[prefix]]
  }
  return(new_prefix)
}

## EOF
