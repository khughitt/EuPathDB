#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite Overwrite existing data?
#' @param webservice Optional alternative webservice for hard-to-find species.
#' @param bioc_version Manually set the bioconductor release if desired.
#' @param build_dir Where to put the json.
#' @param eu_version Choose a specific eupathdb version?
#' @param limit_n Maximum number of valid entries to return.
#' @param verbose Print helper message about species matching?
#' @return Dataframe with lots of rows for the various species in eupathdb.
#' @author Keith Hughitt
#' @export
download_eupath_metadata <- function(overwrite = TRUE, webservice = "eupathdb",
                                     bioc_version = NULL, eu_version = NULL,
                                     verbose = FALSE, build_dir = "build") {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]

  if (isFALSE(overwrite)) {
    message("Checking for existing metadata csv file.")
    file_lst <- get_metadata_filename(webservice, bioc_version, eu_version)
    metadata_df <- readr::read_csv(file = file_lst[["all"]], col_types = readr::cols())

    retlist <- list(
      "valid" = metadata_df,
      "invalid" = data.frame())
    return(retlist)
  }

  ## Choose which service(s) to query, if it is 'eupathdb' do them all.
  webservice <- tolower(webservice)
  if (webservice == "eupathdb") {
    meta_lst <- get_all_metadata()
    return(meta_lst)
  }

  ## Create the build directory if it is not already there.
  if (!dir.exists(build_dir)) {
    created <- dir.create(build_dir, recursive = TRUE)
  }

  .data <- NULL  ## To satisfy R CMD CHECK
  ## Excepting schistodb, all the services are .orgs which is a .net.
  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }

  ## Finalize the URL to query using the webservice, tld, etc.
  service_directory <- prefix_map(webservice)
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/service/record-types/organism/searches/GenomeDataTypes/reports/standard")

  ## FIXME: Set this up as a configurable datastructure that I can modify without being sad.
  post_string <- '{
  "searchConfig": {
    "parameters": {},
    "wdkWeight": 10
  },
  "reportConfig": {
    "attributes": [
      "primary_key",
      "URLgff",
      "isOrganellar_flag",
      "project_id",
      "URLproteinFasta",
      "annotation_source",
      "annotation_version",
      "chromosomeCount",
      "contigCount",
      "genome_source",
      "genome_version",
      "megabps",
      "is_reference_strain",
      "ncbi_tax_id",
      "URLGenomeFasta",
      "genecount",
      "pseudogenecount",
      "chipchipgenecount",
      "ecnumbercount",
      "estcount",
      "gocount",
      "arraygenecount",
      "othergenecount",
      "orthologcount",
      "popsetcount",
      "codinggenecount",
      "proteomicscount",
      "rnaseqcount",
      "rtpcrcount",
      "snpcount",
      "tfbscount",
      "communitycount"
    ],
    "tables": []
  }
}
'
  result <- httr::POST(url = base_url, body = post_string,
                       httr::content_type("application/json"),
                       httr::timeout(120))
  cont <- check_post_result(result)
  result <- try(jsonlite::fromJSON(cont, flatten = TRUE))
  if (class(result)[1] == "try-error") {
    stop("There was a parsing failure when reading the metadata.")
  }

  ## Every record contains and id, some fields, and tables.
  records <- result[["records"]]
  colnames(records) <- gsub(pattern = "^attributes\\.", replacement = "",
                            x = colnames(records))

  ## Once again, this is filling in schisto.org, which is weird.
  ## It turns out the metadata returned still says schistodb.org even though it is .net.
  ## I am pretty sure I wrote Cristina about this and it may be fixed now.
  records <- mutate_if(
    records,
    is.character,
    stringr::str_replace_all, pattern = "SchistoDB.org", replacement = "SchistoDB.net")

  ## The NULL is because NSE semantics are still a bit nonsensical to me.
  ## The goal here though is to create a final table of the expected metadata from the
  ## various bits and pieces downloaded along with the things we know a priori (like the
  ## eupathdb version, bioconductor version, etc.
  SourceUrl <- NULL
  records <- expand_list_columns(records)

  ## The full set of columns changed again and are now (rearranged to alphabetic order):
  ## "annotation_version"
  ## "annotation_source"
  ## "arraygenecount"
  ## "chipchipgenecount"
  ## "chromosomeCount"
  ## "codinggenecount"
  ## "communitycount"
  ## "contigCount"
  ## "displayName"
  ## "ecnumbercount"
  ## "estcount"
  ## "genecount"
  ## "genome_source"
  ## "genome_version"
  ## "gocount"
  ## "id"
  ## "isOrganellar_flag"
  ## "is_reference_strain"
  ## "megabps"
  ## "ncbi_tax_id"
  ## "orthologcount"
  ## "othergenecount"
  ## "popsetcount"
  ## "primary_key"
  ## "proteomicscount"
  ## "project_id"
  ## "provider_id"
  ## "pseudogenecount"
  ## "recordClassName"
  ## "rnaseqcount"
  ## "rtpcrcount"
  ## "snpcount"
  ## "source_id"
  ## "tableErrors"
  ## "tfbscount"
  ## "URLGenomeFasta"
  ## "URLgff"
  ## "URLproteinFasta"

  ## This transmute is being performed in an attempt to standardize the column names.
  ## In creating it, I took the original column names, alphabetized them, rewrote them as
  ## CamelCase, changed ones which are explicitly numeric/URLs, and added a few which are
  ## used by AnnotationHub.  I think we can assume they will change more as time passes, given
  ## that they have changed at least once in the recent past.

  ## Here are columns which do not yet have a home, but were included in the older versions
  ## of this before they changed it circa 202011
  ## "TaxonomyId" = .data[["ncbi_taxon_url.displayText"]],
  ## "SpeciesName" = .data[["species"]],
  ## "StrainName" = .data[["strain"]],
  ## "DataProvider" = .data[["provider_id"]],
  ## "Annotated" = .data[["is_annotated_genome"]],
  ## "Published" = .data[["is_published"]],
  ## "NumOrthologs" = .data[["orthologcount"]],
  ## "SourceUrl" = .data[["URLgff"]],
  ## "SourceType" = "GFF",
  ## "GenomeUrl" = .data[["URLGenomeFasta"]],
  ## "NumGO" = .data[["gocount"]],
  ## "ProjectID" = .data[["project_id"]],
  ## "NumEST" = .data[["estcount"]],
  ## "NumEC" = .data[["ecnumbercount"]],
  ## "NumCoding" = .data[["codinggenecount"]],
  ## "ReferenceStrain" = .data[["is_reference_strain"]],
  ## "NumSNPs" = .data[["snpcount"]],
  ## "NumPseudoGenes" = .data[["pseudogenecount"]],
  ## "ProteinUrl" = .data[["URLproteinFasta"]],
  ## "DataSource" = .data[["data_source"]],
  ## "DataVersion" = .data[["database_version"]],
  ## "Coordinate_1_based" = TRUE,
  ## "Maintainer" = "Keith Hughitt <khughitt@umd.edu>") %>%

  ## Thus I am not going to chain the following operations, because until everything stabilizes
  ## I am just going to have to come back in here and fix weird stuff.
  metadata <- records %>%
    dplyr::transmute(
      ## "annotation_version"
      "AnnotationVersion" = .data[["annotation_version"]],
      ## "annotation_source"
      "AnnotationSource" = .data[["annotation_source"]],
      ## Not a column from the POST
      "BiocVersion" = as.character(bioc_version),
      ## "project_id"
      "DataProvider" = .data[["project_id"]],
      "Genome" = sub(pattern = ".gff", replacement = "", x = basename(.data[["URLgff"]])),
      ## "genome_source"
      "GenomeSource" = .data[["genome_source"]],
      ## "genome_version"
      "GenomeVersion" = .data[["genome_version"]],
      ## "arraygenecount"
      "NumArrayGene" = .data[["arraygenecount"]],
      ## "chipchipgenecount"
      "NumChipChipGene" = .data[["chipchipgenecount"]],
      ## "chromosomeCount"
      "NumChromosome" = .data[["chromosomeCount"]],
      ## "codinggenecount"
      "NumCodingGene" = .data[["codinggenecount"]],
      ## "communitycount"
      "NumCommunity" = .data[["communitycount"]],
      ## "contigCount"
      "NumContig" = .data[["contigCount"]],
      ## "ecnumbercount"
      "NumEC" = .data[["ecnumbercount"]],
      ## "estcount"
      "NumEST" = .data[["estcount"]],
      ## "genecount"
      "NumGene" = .data[["genecount"]],
      ## "gocount"
      "NumGO" = .data[["gocount"]],
      ## "orthologcount"
      "NumOrtholog" = .data[["orthologcount"]],
      ## "othergenecount"
      "NumOtherGene" = .data[["othergenecount"]],
      ## "popsetcount"
      "NumPopSet" = .data[["popsetcount"]],
      ## "proteomicscount"
      "NumProteomics" = .data[["proteomicscount"]],
      ## "pseudogenecount"
      "NumPseudogene" = .data[["pseudogenecount"]],
      ## "rnaseqcount"
      "NumRNASeq" = .data[["rnaseqcount"]],
      ## "rtpcrcount"
      "NumRTPCR" = .data[["rtpcrcount"]],
      ## "snpcount"
      "NumSNP" = .data[["snpcount"]],
      ## "tfbscount"
      "NumTFBS" = .data[["tfbscount"]],
      ## "isOrganellar_flag"
      "Organellar" = .data[["isOrganellar_flag"]],
      ## "is_reference_strain"
      "ReferenceStrain" = .data[["is_reference_strain"]],
      ## "megabps"
      "MegaBP" = .data[["megabps"]],
      ## "primary_key"
      "PrimaryKey" =  gsub(pattern = "<[^>]*>", replacement = "", x = .data[["primary_key"]]),
      ## "project_id"
      "ProjectID" = .data[["project_id"]],
      ## "recordClassName"
      "RecordClassName" = .data[["recordClassName"]],
      ## "source_id"
      "SourceID" = .data[["source_id"]],
      "SourceVersion" = db_version,
      ## "ncbi_tax_id"
      "TaxonomyID" = .data[["ncbi_tax_id"]],
      ## "displayName"
      "TaxonomyName" = gsub(pattern = "<[^>]*>", replacement = "", x = .data[["displayName"]]),
      ## "URLGenomeFasta"
      "URLGenome" = .data[["URLGenomeFasta"]],
      ## "URLgff"
      "URLGFF" = .data[["URLgff"]],
      ## "URLproteinFasta"
      "URLProtein" = .data[["URLproteinFasta"]],
      "Coordinate_1_based" = TRUE,
      "Maintainer" = "Keith Hughitt <khughitt@umd.edu>")
  ##
  ## There remain a series of cleanups which will need to happen before this data is usable:
  ##
  ## 1. When downloading data directly, there is no 'Current_Release' directory (or at least there
  ##    was none when last I checked.
  metadata <- metadata %>%
    dplyr::mutate_if(is.character,
                     stringr::str_replace_all,
                     pattern = "Current_Release",
                     replacement = glue::glue("release-{db_version}"))
  ## 2. In the weeks leading up to a new release, the EuPathDB folks change the SourceURL column
  ##    to reflect the coming database version before it actually exists.  Thus during that time
  ##    downloads will fail unless the database version is substituted back in.
  ## Shush, R CMD check
  URLGFF <- URLGenome <- URLProtein <- NULL
  metadata <- metadata %>%
    dplyr::mutate("SourceUrl" = gsub(pattern = "DB-(\\d\\d)_",
                                     replacement = glue::glue("DB-{db_version}_"),
                                     x = URLGFF)) %>%
    dplyr::mutate("URLGFF" = gsub(pattern = "DB-(\\d\\d)_",
                                  replacement = glue::glue("DB-{db_version}_"),
                                  x = URLGFF)) %>%
    dplyr::mutate("URLGenome" = gsub(pattern = "DB-(\\d\\d)_",  ## Ibid.
                                     replacement = glue::glue("DB-{db_version}_"),
                                     x = URLGenome)) %>%
    dplyr::mutate("URLProtein" = gsub(pattern = "DB-(\\d\\d)_",  ## Ibid.
                                      replacement = glue::glue("DB-{db_version}_"),
                                      x = URLProtein))
  ## 3.  Add taxonomic tags
  tag_strings <- get_tags()
  metadata[["Tags"]] <- sapply(metadata[["DataProvider"]], function(x) {
    tag_strings[[x]]
  })

  ## overide missing taxonomy ids for strains where it can be assigned; ideally
  ## OrgDb and GRanges objects should not depend on taxonomy id information since
  ## this precludes the inclusion of a lot of prokaryotic resources.

  ## In addition, exclude remaining species which are missing taxonomy information from
  ## the metadata; cannot construct GRanges/OrgDb instances for them since they are
  ## have no known taxonomy id, and are not in available.species()

  ## This line is not currently used, it probably should be used to subset out the entries
  ## for which the taxonomy information is invalid.  I think I did not use it because
  ## I later added some heuristics to hunt down the missing entries, and actually doing
  ## the subset based on this results in the loss of too many species.
  na_idx <- is.na(metadata[["TaxonomyID"]])
  ## I think I will try to hack around this problem.
  metadata[["TaxonomyID"]] <- as.numeric(metadata[["TaxonomyID"]])

  ## generate separate metadata table for OrgDB and GRanges targets
  ## build_version_string <- format(Sys.time(), "%Y.%m")
  ## I am going to try to simplify the above and make sure that all filenames actually work.
  ## If my queries to Lori turn out acceptable, then I will delete a bunch of the stuff above.
  ## But for the moment, it will be a bit redundant.
  metadata[["BsgenomePkg"]] <- ""
  metadata[["BsgenomeFile"]] <- ""
  metadata[["GrangesPkg"]] <- ""
  metadata[["GrangesFile"]] <- ""
  metadata[["OrganismdbiPkg"]] <- ""
  metadata[["OrganismdbiFile"]] <- ""
  metadata[["OrgdbPkg"]] <- ""
  metadata[["OrgdbFile"]] <- ""
  metadata[["TxdbPkg"]] <- ""
  metadata[["TxdbFile"]] <- ""
  metadata[["Taxon"]] <- ""
  metadata[["Genus"]] <- ""
  metadata[["Species"]] <- ""
  metadata[["Strain"]] <- ""
  metadata[["GenusSpecies"]] <- ""
  metadata[["TaxonUnmodified"]] <- ""
  metadata[["GIDB_Genus_Species"]] <- ""
  metadata[["AH_Genus_Species"]] <- ""
  metadata[["Valid_Taxonomy_ID"]] <- FALSE
  metadata[["Valid_AH_Species"]] <- FALSE
  metadata[["Matched_Taxonomy"]] <- "unmatched"
  metadata[["Matched_GIDB"]] <- "unmatched"
  metadata[["Matched_AH"]] <- "unmatched"

  ## Load the taxonomy ID number database in order to check/fix messed up/missing IDs.
  message("Loading taxonomy and species database to cross reference against the download.")
  all_taxa_ids <- GenomeInfoDb::loadTaxonomyDb()
  ah_species <- AnnotationHubData::getSpeciesList()
  ## Load the taxonomy ID number database in order to check/fix messed up/missing IDs.
  matched_taxonomy_numbers <- 0
  unmatched_taxonomy_numbers <- 0
  ## Include the package names for the various data types along with the most likely
  ## useful separations of the taxon name (e.g. The Genus, Species, Strain, etc.)
  for (i in seq_len(nrow(metadata))) {
    metadatum <- metadata[i, ]
    ## In most invocations of make_taxon_names and get_eupath_pkgnames,
    ## we use the column 'TaxonUnmodified', because we are modifying Species to
    ## match what is acquired from GenomeInfoDb::loadTaxonomyDb().
    ## But, right now we are in the process of making that match, so use the
    ## Species column here.
    pkg_names <- get_eupath_pkgnames(metadatum, column = "TaxonomyName")
    species_info <- make_taxon_names(metadatum, column = "TaxonomyName")
    metadatum["BsgenomePkg"] <- pkg_names[["bsgenome"]]
    metadatum["BsgenomeFile"] <- file.path(
      build_dir, "S3", "BSgenome", metadatum["BiocVersion"],
      metadatum["BsgenomePkg"], "single_sequences.2bit")
    metadatum["GrangesPkg"] <- pkg_names[["granges"]]
    metadatum["GrangesFile"] <- file.path(
      build_dir, "S3", "GRanges", metadatum["BiocVersion"], metadatum["GrangesPkg"])
    metadatum["OrganismdbiPkg"] <- pkg_names[["organismdbi"]]
    metadatum["OrganismdbiFile"] <- file.path(
      build_dir, "S3", "OrganismDbi", metadatum["BiocVersion"],
      metadatum["OrganismdbiPkg"], "graphInfo.rda")
    metadatum["OrgdbPkg"] <- pkg_names[["orgdb"]]
    metadatum["OrgdbFile"] <- file.path(
      build_dir, "S3", "OrgDb", metadatum["BiocVersion"],
      gsub(x = metadatum["OrgdbPkg"], pattern = "db$", replacement = "sqlite"))
    metadatum["TxdbPkg"] <- pkg_names[["txdb"]]
    metadatum["TxdbFile"] <- file.path(
      build_dir, "S3", "TxDb", metadatum["BiocVersion"],
      glue::glue("{metadatum['TxdbPkg']}.sqlite"))
    metadatum["GenusSpecies"] <- gsub(x = species_info[["genus_species"]],
                                        pattern = "\\.", replacement = " ")
    metadatum["Strain"] <- species_info[["strain"]]
    metadatum["Genus"] <- species_info[["genus"]]
    metadatum["Species"] <- species_info[["species"]]
    metadatum["Taxon"] <- gsub(x = species_info[["taxon"]],
                               pattern = "\\.", replacement = " ")
    metadatum["TaxonUnmodified"] <- species_info[["unmodified"]]

    taxonomy_lst <- xref_taxonomy_number(
      metadatum, all_taxa_ids, taxon_number_column = "TaxonomyID",
      metadata_taxon_column = "TaxonUnmodified", verbose = verbose)
    taxonomy_number <- taxonomy_lst[["ID"]]
    metadatum[["Matched_Taxonomy"]] <- taxonomy_lst[["status"]]
    if (is.null(taxonomy_number)) {
      ## Then we could not make a match.
      unmatched_taxonomy_numbers <- unmatched_taxonomy_numbers + 1
    } else if (isTRUE(taxonomy_number)) {
      matched_taxonomy_numbers <- matched_taxonomy_numbers + 1
      metadatum[["Valid_Taxonomy_ID"]] <- TRUE
      ## Then the existing number matches genomeInfodb.
    } else if (is.numeric(taxonomy_number)) {
      metadatum[["TaxonomyID"]] <- taxonomy_number
      matched_taxonomy_numbers <- matched_taxonomy_numbers + 1
      metadatum[["Valid_Taxonomy_ID"]] <- TRUE
    } else {
      message("Should not fall through to here.")
    }

    gidb_lst <- xref_gidb_species(
      metadatum, taxon_number_column = "TaxonomyID", all_taxa_ids, verbose = verbose)
    metadatum[["GIDB_Genus_Species"]] <- gidb_lst[["ID"]]
    metadatum[["Matched_GIDB"]] <- gidb_lst[["status"]]

    ah_lst <- xref_ah_species(metadatum, ah_species, verbose = verbose)
    found_ah_species <- ah_lst[["ID"]]
    metadata[["Matched_AH"]] <- ah_lst[["status"]]
    if (!is.null(found_ah_species)) {
      metadatum[["Valid_AH_Species"]] <- TRUE
      metadatum[["AH_Genus_Species"]] <- found_ah_species
    }
    ## Hopefully now, the TaxonXref column contains only things which match getSpeciesList()
    ## and the TaxonomyID column contains only things in the GenomeInfoDb.

    ## Assuming all the information is now sanitized and sane,
    ## put the row back into the metadata df with the filled information.
    metadata[i, ] <- metadatum
  }

  valid_idx <- (TRUE == metadata[["Valid_Taxonomy_ID"]]) &
    (TRUE == metadata[["Valid_AH_Species"]])
  valid_entries <- metadata[valid_idx, ]
  invalid_entries <- metadata[!valid_idx, ]

  if (isTRUE(verbose)) {
    message("Writing ", nrow(valid_entries), " valid entries and ",
            nrow(invalid_entries), " invalid entries.")
  }

  ## Write out the metadata and finish up.
  written <- write_eupath_metadata(metadata = valid_entries,
                                   webservice = webservice,
                                   file_type = "valid",
                                   overwrite = overwrite)
  invalid_written <- write_eupath_metadata(metadata = invalid_entries,
                                           webservice = webservice,
                                           file_type = "invalid",
                                           overwrite = overwrite)
    retlist <- list(
    "valid" = valid_entries,
    "invalid" = invalid_entries)
  class(retlist) <- "downloaded_metadata"
  return(retlist)
}


download_real_taxa <- function(webservice, tld, service_directory) {
  funkytown <- "curl 'https://tritrypdb.org/tritrypdb/service/record-types/transcript/searches/GenesByTaxon?expandParams=true' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'x-client-wdk-timestamp: 1611591828791'  -H 'x-client-retry-count: 0'  -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.141 Safari/537.36'  -H 'content-type: application/json; charset=utf-8'  -H 'Accept: */*'  -H 'Sec-Fetch-Site: same-origin'  -H 'Sec-Fetch-Mode: cors'  -H 'Sec-Fetch-Dest: empty'  -H 'Referer: https://tritrypdb.org/tritrypdb/app/search/transcript/GenesByTaxon'  -H 'Accept-Language: en-US,en;q=0.9'  --compressed"
  curl_result <- system2("/bin/bash", args = c("-c", shQuote(funkytown)), stdout = TRUE)
}
