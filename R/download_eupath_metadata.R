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
    ##projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
    ##              "microsporidiadb", "piroplasmadb", "plasmodb",
    ##              "schistodb", "toxodb", "trichdb", "tritrypdb")
    ## Schistodb does not appear to have started using the new database schema yet.
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb",
                  "toxodb", "trichdb", "tritrypdb")
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
  }  ## End if we are asking for all services, it may be worth splitting this off.

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
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/service/record-types/organism/searches/GenomeDataTypes/reports/standard")

  query_string <- '?reportConfig=%7B%22attributes%22%3A%5B%22primary_key%22%2C%22wdk_weight%22%2C%22data_source%22%2C%22genecount_number%22%2C%22database_version%22%2C%22URLgff%22%2C%22is_annotated_genome%22%2C%22project_id%22%2C%22URLproteinFasta%22%2C%22is_published%22%2C%22release_policy%22%2C%22is_reference_strain%22%2C%22ncbi_taxon_url%22%2C%22overview%22%2C%22species%22%2C%22strain%22%2C%22URLGenomeFasta%22%2C%22pseudogenecount%22%2C%22chipchipgenecount%22%2C%22ecnumbercount%22%2C%22estcount%22%2C%22gocount%22%2C%22arraygenecount%22%2C%22othergenecount%22%2C%22orthologcount%22%2C%22popsetcount%22%2C%22codinggenecount%22%2C%22proteomicscount%22%2C%22rnaseqcount%22%2C%22rtpcrcount%22%2C%22snpcount%22%2C%22tfbscount%22%2C%22hasHTSIsolate%22%5D%2C%22tables%22%3A%5B%5D%7D'
  request_url <- glue::glue("{base_url}{query_string}")

  ## retrieve organism metadata from EuPathDB
  metadata_json <- glue::glue("{build_dir}/metadata.json")

  ## It turns out that not all eupathdb hosts have moved to https.
  ## Therefore I wrapped this download to a try() and will check it.
  file <- try(download.file(url = request_url, destfile = metadata_json), silent = TRUE)

  ## We should now have some json data to poke through.
  result <- try(jsonlite::fromJSON(metadata_json, flatten = TRUE), silent = TRUE)
  if (class(result)[1] == "try-error") {
    stop("There was a parsing failure when reading the metadata.")
  }

  ## Every record contains and id, some fields, and tables.
  records <- result[["records"]]
  colnames(records) <- gsub(pattern = "^attributes\\.", replacement = "", x = colnames(records))

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

  ## The new id column is a nested dataframe, which is super annnoying.
  ## In theory this should be pretty easy to deal with using dplyr/tidyr, but
  ## the way the internal dataframe is structured is a bit more than weird.
  records[["provider_id"]] <- ""
  records[["source_id"]] <- ""
  for (r in 1:nrow(records)) {
    internal <- records[r, "id"][[1]]
    records[r, "source_id"] <- internal[1, "value"]
    records[r, "provider_id"] <- internal[2, "value"]
  }

  ## The full set of columns are:
  ## displayName recordClassName id tableErrors is_annotated_genome is_published arraygenecount
  ## orthologcount URLgff URLGenomeFasta release_policy chipchipgenecount popsetcount gocount
  ## project_id estcount ecnumbercount proteomicscount codinggenecount is_reference_strain strain
  ## rtpcrcount snpcount overview pseudogenecount wdk_weight URLproteinFasta hasHTSIsolate
  ## genecount_number primary_key tfbscount data_source species database_version othergenecount
  ## rnaseqcount ncbi_taxon_url.displayText ncbi_taxon_url.url provider_id source_id
  ## I left off a fair number of columns out of lazyness and/or disinterest.
  metadata <- records %>%
    dplyr::transmute(
             "BiocVersion" = as.character(bioc_version),
             "Genome" = sub(pattern = ".gff", replacement = "", x = basename(.data[["URLgff"]])),
             "NumGenes"= .data[["genecount_number"]],
             "SourceVersion" = db_version,
             "TaxonomyName" = gsub(pattern = "<[^>]*>", replacement = "", x = .data[["displayName"]]),
             "TaxonomyId" = .data[["ncbi_taxon_url.displayText"]],
             "SpeciesName" = .data[["species"]],
             "StrainName" = .data[["strain"]],
             "DataProvider" = .data[["provider_id"]],
             "Annotated" = .data[["is_annotated_genome"]],
             "Published" = .data[["is_published"]],
             "NumOrthologs" = .data[["orthologcount"]],
             "SourceUrl" = .data[["URLgff"]],
             "SourceType" = "GFF",
             "GenomeUrl" = .data[["URLGenomeFasta"]],
             "NumGO" = .data[["gocount"]],
             "ProjectID" = .data[["project_id"]],
             "NumEST" = .data[["estcount"]],
             "NumEC" = .data[["ecnumbercount"]],
             "NumCoding" = .data[["codinggenecount"]],
             "ReferenceStrain" = .data[["is_reference_strain"]],
             "NumSNPs" = .data[["snpcount"]],
             "NumPseudoGenes" = .data[["pseudogenecount"]],
             "ProteinUrl" = .data[["URLproteinFasta"]],
             "DataSource" = .data[["data_source"]],
             "DataVersion" = .data[["database_version"]],
             "Coordinate_1_based" = TRUE,
             "Maintainer" = "Keith Hughitt <khughitt@umd.edu>") %>%
    dplyr::mutate_if(is.character,  ## This was added because there is no 'Current_Release' directory.
                     stringr::str_replace_all,
                     pattern = "Current_Release",
                     replacement = glue::glue("release-{db_version}")) %>%
    dplyr::mutate("SourceUrl" = gsub(pattern = "DB-(\\d\\d)_",  ## This is because during the weeks
                                     ## when they are getting ready for a new release, this field
                                     ## does not match the actual data (E.g. they increment this url
                                     ## before the data is actually released).
                                     replacement = glue::glue("DB-{db_version}_"),
                                     x = SourceUrl)) %>%
    dplyr::mutate("GenomeUrl" = gsub(pattern = "DB-(\\d\\d)_",  ## Ibid.
                                     replacement = glue::glue("DB-{db_version}_"),
                                     x = GenomeUrl)) %>%
    dplyr::mutate("ProteinUrl" = gsub(pattern = "DB-(\\d\\d)_",  ## Ibid.
                                      replacement = glue::glue("DB-{db_version}_"),
                                      x = ProteinUrl))

  ## Add project-specific tags for each entry
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
  metadata[["Str"]] <- ""

  ## A couple changes to try to make the metadata I generate pass
  for (i in 1:nrow(metadata)) {
    metadatum <- metadata[i, ]
    ## In most invocations of make_taxon_names and get_eupath_pkgnames,
    ## we use the column 'TaxonUnmodified', because we are modifying Species to
    ## match what is acquired from GenomeInfoDb::loadTaxonomyDb().
    ## But, right now we are in the process of making that match, so use the
    ## Species column here.
    pkg_names <- get_eupath_pkgnames(metadatum, column = "TaxonomyName")
    species_info <- make_taxon_names(metadatum, column = "TaxonomyName")
    metadata[i, "BsgenomePkg"] <- pkg_names[["bsgenome"]]
    metadata[i, "BsgenomeFile"] <- file.path(
      build_dir, "BSgenome", metadata[i, "BiocVersion"],
      metadata[i, "BsgenomePkg"], "single_sequences.2bit")
    metadata[i, "GrangesPkg"] <- pkg_names[["granges"]]
    metadata[i, "GrangesFile"] <- file.path(
      build_dir, "GRanges", metadata[i, "BiocVersion"], metadata[i, "GrangesPkg"])
    metadata[i, "OrganismdbiPkg"] <- pkg_names[["organismdbi"]]
    metadata[i, "OrganismdbiFile"] <- file.path(
      build_dir, "OrganismDbi", metadata[i, "BiocVersion"],
      metadata[i, "OrganismdbiPkg"], "graphInfo.rda")
    metadata[i, "OrgdbPkg"] <- pkg_names[["orgdb"]]
    metadata[i, "OrgdbFile"] <- file.path(
      build_dir, "OrgDb", metadata[i, "BiocVersion"],
      gsub(x = metadata[i, "OrgdbPkg"], pattern = "db$", replacement = "sqlite"))
    metadata[i, "TxdbPkg"] <- pkg_names[["txdb"]]
    metadata[i, "TxdbFile"] <- file.path(
      build_dir, "TxDb", metadata[i, "BiocVersion"],
      glue::glue("{metadata[i, 'TxdbPkg']}.sqlite"))
    metadata[i, "Sp"] <- gsub(x = species_info[["genus_species"]],
                                    pattern = "\\.", replacement = " ")
    metadata[i, "Str"] <- species_info[["strain"]]
    metadata[i, "Genus"] <- species_info[["genus"]]
    metadata[i, "Sp"] <- species_info[["species"]]
    metadata[i, "Taxon"] <- gsub(x = species_info[["taxon"]],
                                  pattern = "\\.", replacement = " ")
    metadata[i, "TaxonUnmodified"] <- species_info[["unmodified"]]
  }

  ## Use the xref_() functions to try to ensure that we find valid taxonomy names
  ## and identifiers for as many species as possible.
  ## There are two things we need to successfully cross reference:
  ##  1.  The taxonomy IDs from GenomeInfoDB
  ##  2.  The species names provided by AnnotationHubData's getSpeciesList().
  taxa_xref <- xref_taxonomy(metadata, verbose = verbose, species_column = "TaxonomyName")
  species_xref <- xref_species(valid = taxa_xref[["matched_metadata"]],
                               invalid = taxa_xref[["unmatched_metadata"]],
                               verbose = verbose)
  ## Hopefully now, the TaxonXref column contains only things which match getSpeciesList()
  ## and the TaxonomyId column contains only things in the GenomeInfoDb.

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
                                     file_type = "valid",
                                     build_dir = build_dir)
    invalid_written <- write_eupath_metadata(species_xref[["invalid"]], webservice,
                                             file_type="invalid",
                                             build_dir = build_dir)
  }
  retlist <- list(
    "valid" = species_xref[["valid"]],
    "invalid" = species_xref[["invalid"]])
  return(retlist)
}
