#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite Overwrite existing data?
#' @param webservice Optional alternative webservice for hard-to-find species.
#' @param bioc_version Manually set the bioconductor release if desired.
#' @param dir Where to put the json.
#' @param eu_version Choose a specific eupathdb version?
#' @param write_csv Write a csv file in the format expected by AnnotationHubData?
#' @param limit_n Maximum number of valid entries to return.
#' @param verbose Print helper message about species matching?
#' @return Dataframe with lots of rows for the various species in eupathdb.
#' @author Keith Hughitt
#' @export
download_eupath_metadata <- function(overwrite = FALSE, webservice = "eupathdb",
                                     bioc_version = NULL, build_dir = "EuPathDB",
                                     eu_version = NULL, write_csv = FALSE,
                                     limit_n = Inf, verbose = FALSE) {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]

  ## Choose which service(s) to query, if it is 'eupathdb' do them all.
  webservice <- tolower(webservice)
  if (webservice == "eupathdb") {
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb",
                  "schistodb", "toxodb", "trichdb", "tritrypdb")
    results <- list()
    valid_metadata <- data.frame()
    invalid_metadata <- data.frame()

    for (i in 1:length(projects)) {
    webservice <- projects[i]
    results[[webservice]] <- download_eupath_metadata(
        webservice = webservice, overwrite = overwrite, bioc_version = bioc_version,
        build_dir = build_dir, eu_version = eu_version, write_csv = FALSE)
    }

    for (r in results) {
        valid_metadata <- rbind(valid_metadata, r[["valid"]])
        invalid_metadata <- rbind(invalid_metadata, r[["invalid"]])
    }

    ## if enabled, limit metadata table to N entries;
    ## this can be helpful during development and testing
    if (limit_n < Inf && limit_n < nrow(valid_metadata)) {
      set.seed(1)
      info(sprintf("Limiting metadata results to %d entries", limit_n))

      ind <- sample(nrow(valid_metadata), limit_n)
      valid_metadata <- valid_metadata[ind, ]
    }

    if (isTRUE(write_csv)) {
      message("Writing metadata csv files.")
      written <- write_eupath_metadata(
          valid_metadata, service = "eupathdb", file_type = "valid",
          bioc_version = bioc_version, eu_version = eu_version, build_dir = build_dir)
    }
    return(list(
      "valid" = valid_metadata,
      "invalid" = invalid_metadata))
  }

  ## Create the build directory if it is not already there.
  if (!dir.exists(build_dir)) {
    dir.create(build_dir, recursive = TRUE)
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
    "SchistoDB" = c(shared_tags, "Schistosoma"),
    "ToxoDB" = c(shared_tags, "Toxoplasmosis"),
    "TrichDB" = c(shared_tags, "Trichomonas"),
    "TriTrypDB" = c(shared_tags, "Trypanosome", "Kinetoplastid", "Leishmania"))
  tag_strings <- lapply(tags, function(x) {
    paste(x, collapse=":")
  })

  ## Excepting schistodb, all the services are .orgs which is a .net.
  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }

  ## Finalize the URL to query using the webservice, tld, etc.
  service_directory <- prefix_map(webservice)
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/webservices/")
  query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
  request_url <- glue::glue("{base_url}{query_string}")

  ## retrieve organism metadata from EuPathDB
  metadata_json <- glue::glue("{dir}/metadata.json")

  ## It turns out that not all eupathdb hosts have moved to https.
  ## Therefore I wrapped this download to a try() and will check it.
  file <- try(download.file(url = request_url, destfile = metadata_json), silent = TRUE)
  if (class(file) == "try-error") {
    ## Try again without https?
    if (isTRUE(verbose)) {
      message("Downloading the https file failed, not all eupathdb services have migrated to https,
trying http next.")
    }
    base_url <- glue::glue("http://{webservice}.{tld}/{service_directory}/webservices/")
    query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
    request_url <- glue::glue("{base_url}{query_string}")
    ## retrieve organism metadata from EuPathDB
    metadata_json <- glue::glue("{dir}/metadata.json")
    file <- download.file(url = request_url, destfile = metadata_json)
  }

  ## We should now have some json data to poke through.
  result <- try(jsonlite::fromJSON(metadata_json), silent = TRUE)
  if (class(result)[1] == "try-error") {
    stop("There was a parsing failure when reading the metadata.")
  }

  ## Every record contains and id, some fields, and tables.
  records <- result[["response"]][["recordset"]][["records"]]

  ## Extract the fields to a dataframe
  dat <- data.frame(t(sapply(records[["fields"]], function(x) {
    x[, "value"] })),
    stringsAsFactors=FALSE)
  ## And get the name
  colnames(dat) <- records[["fields"]][[1]][["name"]]

  ## Once again, this is filling in schisto.org, which is weird.
  ## It turns out the metadata returned still says schistodb.org even though it is .net.
  ## I am pretty sure I wrote Cristina about this and it may be fixed now.
  dat <- mutate_if(
    dat,
    is.character,
    stringr::str_replace_all, pattern = "SchistoDB.org", replacement = "SchistoDB.net")

  ## The NULL is because NSE semantics are still a bit nonsensical to me.
  ## The goal here though is to create a final table of the expected metadata from the
  ## various bits and pieces downloaded along with the things we know a priori (like the
  ## eupathdb version, bioconductor version, etc.
  SourceUrl <- NULL
  metadata <- dat %>%
    dplyr::transmute(
             "BiocVersion" = as.character(bioc_version),
             "Genome" = sub(".gff", "", basename(.data[["URLgff"]])),
             "NumGenes"= .data[["genecount"]],
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
                     pattern = "Current_Release",
                     replacement = glue::glue("release-{db_version}")) %>%
    dplyr::mutate("SourceUrl" = gsub(pattern = "DB-(\\d\\d)_",
                                     replacement = glue::glue("DB-{db_version}_"),
                                     x = SourceUrl))

  ## Add project-specific tags for each entry
  metadata[["Tags"]] <- sapply(metadata[["DataProvider"]], function(x) {
    tag_strings[[x]]
  })

  ## replace missing taxonomy ids with NAs
  metadata[["TaxonomyId"]][metadata[["TaxonomyId"]] == ""] <- NA

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
  na_idx <- is.na(metadata[["TaxonomyId"]])
  ## I think I will try to hack around this problem.
  metadata[["TaxonomyId"]] <- as.numeric(metadata[["TaxonomyId"]])

  ## generate separate metadata table for OrgDB and GRanges targets
  version_string <- format(Sys.time(), "%Y.%m")
  ## I am going to try to simplify the above and make sure that all filenames actually work.
  ## If my queries to Lori turn out acceptable, then I will delete a bunch of the stuff above.
  ## But for the moment, it will be a bit redundant.
  metadata[["BsgenomePkg"]] <- ""
  metadata[["GrangesPkg"]] <- ""
  metadata[["OrganismdbiPkg"]] <- ""
  metadata[["OrgdbPkg"]] <- ""
  metadata[["TxdbPkg"]] <- ""
  metadata[["Taxon"]] <- ""
  metadata[["Genus"]] <- ""
  metadata[["Sp"]] <- ""
  metadata[["Strain"]] <- ""
  ## Also double-check the taxon IDs
  db_version <- metadata[1, "SourceVersion"]

  ## A couple changes to try to make the metadata I generate pass
  for (i in 1:nrow(metadata)) {
    metadatum <- metadata[i, ]
    ## In most invocations of make_taxon_names and get_eupath_pkgnames,
    ## we use the column 'TaxonUnmodified', because we are modifying Species to
    ## match what is acquired from GenomeInfoDb::loadTaxonomyDb().
    ## But, right now we are in the process of making that match, so use the
    ## Species column here.
    pkg_names <- get_eupath_pkgnames(metadatum, column = "Species")
    species_info <- make_taxon_names(metadatum, column = "Species")
    metadata[i, "BsgenomePkg"] <- pkg_names[["bsgenome"]]
    metadata[i, "BsgenomeFile"] <- file.path(
      dir, "BSgenome", metadata[i, "BiocVersion"],
      metadata[i, "BsgenomePkg"], "single_sequences.2bit")
    metadata[i, "GrangesPkg"] <- pkg_names[["granges"]]
    metadata[i, "GrangesFile"] <- file.path(
      dir, "GRanges", metadata[i, "BiocVersion"], metadata[i, "GrangesPkg"])
    metadata[i, "OrganismdbiPkg"] <- pkg_names[["organismdbi"]]
    metadata[i, "OrganismdbiFile"] <- file.path(
      dir, "OrganismDbi", metadata[i, "BiocVersion"],
      metadata[i, "OrganismdbiPkg"], "graphInfo.rda")
    metadata[i, "OrgdbPkg"] <- pkg_names[["orgdb"]]
    metadata[i, "OrgdbFile"] <- file.path(
      dir, "OrgDb", metadata[i, "BiocVersion"],
      gsub(x = metadata[i, "OrgdbPkg"], pattern = "db$", replacement = "sqlite"))
    metadata[i, "TxdbPkg"] <- pkg_names[["txdb"]]
    metadata[i, "TxdbFile"] <- file.path(
      dir, "TxDb", metadata[i, "BiocVersion"],
      glue::glue("{metadata[i, 'TxdbPkg']}.sqlite"))
    metadata[i, "Species"] <- gsub(x = species_info[["genus_species"]],
                                    pattern = "\\.", replacement = " ")
    metadata[i, "Strain"] <- species_info[["strain"]]
    metadata[i, "Genus"] <- species_info[["genus"]]
    metadata[i, "Sp"] <- species_info[["species"]]
    metadata[i, "Taxon"] <- gsub(x = species_info[["taxon"]],
                                  pattern = "\\.", replacement = " ")
    metadata[i, "TaxonUnmodified"] <- species_info[["unmodified"]]
  }

  ## Use the xref_() functions to try to ensure that we find valid taxonomy names
  ## and identifiers for as many species as possible.
  taxa_xref <- xref_taxonomy(metadata, verbose = verbose)
  species_xref <- xref_species(valid = taxa_xref[["matched_metadata"]],
                               invalid = taxa_xref[["unmatched_metadata"]],
                               verbose = verbose)

  ## if enabled, limit metadata table to N entries;
  if (limit_n < Inf & limit_n < nrow(species_xref[["valid"]])) {
    set.seed(1)
    info(sprintf("Limiting metadata results to %d entries", limit_n))
    ind <- sample(nrow(species_xref[["valid"]]), limit_n)
    species_xref[["valid"]] <- species_xref[["valid"]][ind, ]
  }

  ## Write out the metadata and finish up.
  if (isTRUE(write_csv)) {
    message("Writing EuPathDB metadata csv files.")
    written <- write_eupath_metadata(species_xref[["valid"]], webservice,
                                     bioc_version, db_version, file_type = "valid",
                                     build_dir = build_dir)
    invalid_written <- write_eupath_metadata(species_xref[["invalid"]], webservice,
                                             bioc_version, db_version, file_type="invalid",
                                             build_dir = build_dir)
  }
  retlist <- list(
    "valid" = species_xref[["valid"]],
    "invalid" = species_xref[["invalid"]])
  return(retlist)
}
