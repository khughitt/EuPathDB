#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite Overwrite existing data?
#' @param webservice Optional alternative webservice for hard-to-find species.
#' @param bioc_version Manually set the bioconductor release if desired.
#' @param dir Where to put the json.
#' @param eu_version Choose a specific eupathdb version?
#' @param write_csv Write a csv file in the format expected by AnnotationHubData?
#' @param verbose Print helper message about species matching?
#' @return Dataframe with lots of rows for the various species in eupathdb.
#' @author Keith Hughitt
#' @export
download_eupath_metadata <- function(overwrite=FALSE, webservice="eupathdb",
                                     bioc_version=NULL, dir="EuPathDB",
                                     eu_version=NULL, write_csv=FALSE,
                                     verbose=FALSE) {
  versions <- get_versions(bioc_version=bioc_version, eu_version=eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]
  webservice <- tolower(webservice)
  ## Get EuPathDB version (same for all databases)
  if (webservice == "eupathdb") {
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb",
                  "schistodb", "toxodb", "trichdb", "tritrypdb")
    results <- list()
    valid_metadata <- data.frame()
    invalid_metadata <- data.frame()
    for (c in 1:length(projects)) {
    webservice <- projects[c]
    results[[webservice]] <- download_eupath_metadata(webservice=webservice, overwrite=overwrite,
                                                      bioc_version=bioc_version, dir=dir,
                                                      eu_version=eu_version,
                                                      write_csv=FALSE)
    }

    for (r in results) {
        valid_metadata <- rbind(valid_metadata, r[["valid"]])
        invalid_metadata <- rbind(invalid_metadata, r[["invalid"]])
    }

    if (isTRUE(write_csv)) {
      message("Writing csv files.")
      written <- write_eupath_metadata(valid_metadata, service="eupathdb",
                                       type="valid", bioc_version=bioc_version,
                                       eu_version=eu_version)
    }
    return(list(
      "valid" = valid_metadata,
      "invalid" = invalid_metadata))
  }

  if (!dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
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

  tld <- "org"
  if (webservice == "schistodb") {
    tld <- "net"
  }

  service_directory <- prefix_map(webservice)
  ## construct API request URL
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/webservices/")
  query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
  request_url <- glue::glue("{base_url}{query_string}")

  ## retrieve organism metadata from EuPathDB
  metadata_json <- glue::glue("{dir}/metadata.json")
  ## It turns out that not all eupathdb hosts have moved to https...
  file <- try(download.file(url=request_url, destfile=metadata_json), silent=TRUE)
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
    file <- download.file(url=request_url, destfile=metadata_json)
  }

  result <- try(jsonlite::fromJSON(metadata_json), silent=TRUE)
  if (class(result)[1] == "try-error") {
    stop("There was a parsing failure when reading the metadata.")
  }
  records <- result[["response"]][["recordset"]][["records"]]
  ##message("Downloaded: ", request_url)

  ## convert to a dataframe
  dat <- data.frame(t(sapply(records[["fields"]], function(x) {
    x[, "value"] })),
    stringsAsFactors=FALSE)
  colnames(dat) <- records[["fields"]][[1]][["name"]]

  ## Once again, this is filling in schisto.org, which is weird.
  dat <- mutate_if(
    dat,
    is.character,
    stringr::str_replace_all, pattern="SchistoDB.org", replacement="SchistoDB.net")

  SourceUrl <- NULL  ## Because I still don't get NSE/SE semantics with mutate()!!
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
                     pattern="Current_Release",
                     replacement=glue::glue("release-{db_version}")) %>%
    dplyr::mutate("SourceUrl" = gsub(pattern="DB-(\\d\\d)_",
                                     replacement=glue::glue("DB-{db_version}_"),
                                     x=SourceUrl))

  ## Add project-specific tags for each entry
  metadata[["Tags"]] <- sapply(metadata[["DataProvider"]],
                               function(x) {
                                 tag_strings[[x]] })

  ## replace missing taxonomy ids with NAs
  metadata[["TaxonomyId"]][metadata[["TaxonomyId"]] == ""] <- NA

  ## overide missing taxonomy ids for strains where it can be assigned; ideally
  ## OrgDb and GRanges objects should not depend on taxonomy id information since
  ## this precludes the inclusion of a lot of prokaryotic resources.

  ## exclude remaining species which are missing taxonomy information from
  ## metadata; cannot construct GRanges/OrgDb instances for them since they are
  ## have no known taxonomy id, and are not in available.species()
  na_ind <- is.na(metadata[["TaxonomyId"]])
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
  for (it in 1:nrow(metadata)) {
    metadatum <- metadata[it, ]
    ## In most invocations of make_taxon_names and get_eupath_pkgnames,
    ## we use the column 'TaxonUnmodified', because we are modifying Species to
    ## match what is acquired from GenomeInfoDb::loadTaxonomyDb().
    ## But, right now we are in the process of making that match, so use the
    ## Species column here.
    pkg_names <- get_eupath_pkgnames(metadatum, column="Species")
    species_info <- make_taxon_names(metadatum, column="Species")
    metadata[it, "BsgenomePkg"] <- pkg_names[["bsgenome"]]
    metadata[it, "BsgenomeFile"] <- file.path(
      dir, "BSgenome", metadata[it, "BiocVersion"],
      metadata[it, "BsgenomePkg"], "single_sequences.2bit")
    metadata[it, "GrangesPkg"] <- pkg_names[["granges"]]
    metadata[it, "GrangesFile"] <- file.path(
      dir, "GRanges", metadata[it, "BiocVersion"], metadata[it, "GrangesPkg"])
    metadata[it, "OrganismdbiPkg"] <- pkg_names[["organismdbi"]]
    metadata[it, "OrganismdbiFile"] <- file.path(
      dir, "OrganismDbi", metadata[it, "BiocVersion"],
      metadata[it, "OrganismdbiPkg"], "graphInfo.rda")
    metadata[it, "OrgdbPkg"] <- pkg_names[["orgdb"]]
    metadata[it, "OrgdbFile"] <- file.path(
      dir, "OrgDb", metadata[it, "BiocVersion"],
      gsub(x=metadata[it, "OrgdbPkg"], pattern="db$", replacement="sqlite"))
    metadata[it, "TxdbPkg"] <- pkg_names[["txdb"]]
    metadata[it, "TxdbFile"] <- file.path(
      dir, "TxDb", metadata[it, "BiocVersion"],
      glue::glue("{metadata[it, 'TxdbPkg']}.sqlite"))
    metadata[it, "Species"] <- gsub(x=species_info[["genus_species"]],
                                    pattern="\\.", replacement=" ")
    metadata[it, "Strain"] <- species_info[["strain"]]
    metadata[it, "Genus"] <- species_info[["genus"]]
    metadata[it, "Sp"] <- species_info[["species"]]
    metadata[it, "Taxon"] <- gsub(x=species_info[["taxon"]],
                                  pattern="\\.", replacement=" ")
    metadata[it, "TaxonUnmodified"] <- species_info[["unmodified"]]
  }

  taxa_xref <- xref_taxonomy(metadata, verbose=verbose)
  species_xref <- xref_species(valid=taxa_xref[["matched_metadata"]],
                               invalid=taxa_xref[["unmatched_metadata"]],
                               verbose=verbose)
  if (isTRUE(write_csv)) {
    message("Writing csv files.")
    written <- write_eupath_metadata(species_xref[["valid"]], webservice,
                                     bioc_version, db_version, type="valid")
    invalid_written <- write_eupath_metadata(species_xref[["invalid"]], webservice,
                                             bioc_version, db_version, type="invalid")
  }
  retlist <- list(
    "valid" = species_xref[["valid"]],
    "invalid" = species_xref[["invalid"]])
  return(retlist)
}
x
