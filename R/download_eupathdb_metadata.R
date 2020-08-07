#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite Overwrite existing data?
#' @param webservice Optional alternative webservice for hard-to-find species.
#' @param bioc_version Manually set the bioconductor release if desired.
#' @param build_dir Where to put the json.
#' @param eupathdb_version Choose a specific eupathdb version?
#' @param write_csv Write a csv file in the format expected by AnnotationHubData?
<<<<<<< HEAD
#' @param limit_n Maximum number of valid entries to return.
=======
#' @param limit_n Maximum number of valid entries to return; useful for testing
>>>>>>> fc81572 (Some more refactoring / fixes)
#' @param verbose Print helper message about species matching?
#' @return Dataframe with lots of rows for the various species in eupathdb.
#' @author Keith Hughitt
#' @export
<<<<<<< HEAD:R/download_eupath_metadata.R
download_eupath_metadata <- function(overwrite = FALSE, webservice = "eupathdb",
<<<<<<< HEAD
                                     bioc_version = NULL, build_dir = "EuPathDB",
                                     eu_version = NULL, write_csv = FALSE,
                                     limit_n = Inf, verbose = FALSE) {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]
=======
=======
download_eupathdb_metadata <- function(overwrite = FALSE, webservice = "eupathdb",
<<<<<<< HEAD
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/download_eupathdb_metadata.R
                                     bioc_version = NULL, dir = "EuPathDB",
                                     eu_version = NULL, write_csv = FALSE,
                                     verbose = FALSE) {
  db_version <- NULL
  if (is.null(eu_version)) {
=======
                                       bioc_version = NULL, build_dir = "EuPathDB",
                                       eupathdb_version = NULL, write_csv = FALSE,
                                       limit_n = Inf, verbose = FALSE) {
  db_version <- NULL

  if (is.null(eupathdb_version)) {
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
    eupathdb_version <- gsub(x = db_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")
  } else {
    eupathdb_version <- gsub(x = eupathdb_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")
    db_version <- gsub(x = eupathdb_version, pattern = "^v", replacement = "")
    ## eupathdb_version
  }

  ## For when releasing a new bioconductor release which I don't yet have.
  if (is.null(bioc_version)) {
    bioc_version <- as.character(BiocManager::version())
  }
>>>>>>> 13b1ccc (updated email; coding style tweaks)

<<<<<<< HEAD
  ## Choose which service(s) to query, if it is 'eupathdb' do them all.
  webservice <- tolower(webservice)
=======
  # if webservice is set to "eupathdb", iterate over different front-ends and generate
  # metadata for each of them separately
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  if (webservice == "eupathdb") {
    ##projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
    ##              "microsporidiadb", "piroplasmadb", "plasmodb",
    ##              "schistodb", "toxodb", "trichdb", "tritrypdb")
    ## Schistodb does not appear to have started using the new database schema yet.
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb",
<<<<<<< HEAD
                  "toxodb", "trichdb", "tritrypdb")
=======
                  "schistodb", "toxodb", "trichdb", "tritrypdb")

>>>>>>> 13b1ccc (updated email; coding style tweaks)
    results <- list()
    valid_metadata <- data.frame()
    invalid_metadata <- data.frame()

<<<<<<< HEAD
<<<<<<< HEAD
    for (i in 1:length(projects)) {
    webservice <- projects[i]
    results[[webservice]] <- download_eupath_metadata(
        webservice = webservice, overwrite = overwrite, bioc_version = bioc_version,
        build_dir = build_dir, eu_version = eu_version, write_csv = FALSE)
=======
    for (c in 1:length(projects)) {
      webservice <- projects[c]

      results[[webservice]] <- download_eupathdb_metadata(webservice = webservice,
                                                        overwrite = overwrite,
                                                        bioc_version = bioc_version,
                                                        dir = dir, eu_version = eu_version,
                                                        write_csv = FALSE)
>>>>>>> 13b1ccc (updated email; coding style tweaks)
=======
    for (proj in 1:length(projects)) {
      webservice <- projects[proj]

      results[[webservice]] <- download_eupathdb_metadata(webservice = webservice,
                                                          overwrite = overwrite,
                                                          bioc_version = bioc_version,
                                                          build_dir = build_dir,
                                                          eupathdb_version = eupathdb_version,
                                                          write_csv = FALSE)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
    }

    for (entry in results) {
        valid_metadata <- rbind(valid_metadata, entry[["valid"]])
        invalid_metadata <- rbind(invalid_metadata, entry[["invalid"]])
    }

<<<<<<< HEAD
    ## if enabled, limit metadata table to N entries;
    ## this can be helpful during development and testing
=======
    # if enabled, limit metadata table to N entries;
    # this can be helpful during development and testing
>>>>>>> fc81572 (Some more refactoring / fixes)
    if (limit_n < Inf && limit_n < nrow(valid_metadata)) {
      set.seed(1)
      info(sprintf("Limiting metadata results to %d entries", limit_n))

      ind <- sample(nrow(valid_metadata), limit_n)
      valid_metadata <- valid_metadata[ind, ]
    }

    if (isTRUE(write_csv)) {
<<<<<<< HEAD
<<<<<<< HEAD
      message("Writing metadata csv files.")
      written <- write_eupath_metadata(
          valid_metadata, service = "eupathdb", file_type = "valid",
          bioc_version = bioc_version, eu_version = eu_version, build_dir = build_dir)
=======
      message("Writing csv files.")
      written <- write_eupathdb_metadata(valid_metadata, service = "eupathdb",
<<<<<<< HEAD
                                       type = "valid", bioc_version = bioc_version,
                                       eu_version = eu_version)
>>>>>>> 13b1ccc (updated email; coding style tweaks)
=======
                                         type = "valid", bioc_version = bioc_version,
=======
      info("Writing metadata csv files.")

      written <- write_eupathdb_metadata(valid_metadata, service = "eupathdb",
                                         file_type = "valid", bioc_version = bioc_version,
>>>>>>> fc81572 (Some more refactoring / fixes)
                                         eupathdb_version = eupathdb_version,
                                         build_dir = build_dir)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
    }
    return(list(
      "valid" = valid_metadata,
<<<<<<< HEAD
      "invalid" = invalid_metadata))
  }  ## End if we are asking for all services, it may be worth splitting this off.
=======
      "invalid" = invalid_metadata
    ))
  }
>>>>>>> fc81572 (Some more refactoring / fixes)

<<<<<<< HEAD
<<<<<<< HEAD
  ## Create the build directory if it is not already there.
  if (!dir.exists(build_dir)) {
    dir.create(build_dir, recursive = TRUE)
=======
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
>>>>>>> 13b1ccc (updated email; coding style tweaks)
=======
  # create build directory if needed
  if (!dir.exists(build_dir)) {
    dir.create(build_dir, recursive = TRUE)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
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
    paste(x, collapse = ":")
  })

<<<<<<< HEAD
  ## Excepting schistodb, all the services are .orgs which is a .net.
=======
  # most of the api's are located at .org sites
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  tld <- "org"

  # schistodb is the one exception and is hosted on a .net site
  if (webservice == "schistodb") {
      tld <- "net"
  }

  ## Finalize the URL to query using the webservice, tld, etc.
  service_directory <- prefix_map(webservice)
<<<<<<< HEAD
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/service/record-types/organism/searches/GenomeDataTypes/reports/standard")

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
  ## Test the result to see that we actually got data.
  if (result[["status_code"]] == "422") {
      warn(sprintf("API request failed for %s (code = 422): ", entry[["Taxon"]]))
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
  cont <- httr::content(result, encoding = "UTF-8", as = "text")
  result <- try(jsonlite::fromJSON(cont, flatten = TRUE))
=======

  ## construct API request URL
  base_url <- glue::glue("https://{webservice}.{tld}/{service_directory}/webservices/")
  query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
  request_url <- glue::glue("{base_url}{query_string}")

  ## retrieve organism metadata from EuPathDB
  metadata_json <- glue::glue("{build_dir}/metadata.json")

  # It turns out that not all eupathdb hosts have moved to https...
  file <- try(download.file(url = request_url, destfile = metadata_json), silent = TRUE)

  if (class(file) == "try-error") {
    ## Try again without https?
    if (isTRUE(verbose)) {
      warn("Downloading the https file failed, not all eupathdb services have ",
           "migrated to https, trying http next.")
    }

    base_url <- glue::glue("http://{webservice}.{tld}/{service_directory}/webservices/")
    query_string <- "OrganismQuestions/GenomeDataTypes.json?o-fields=all"
    request_url <- glue::glue("{base_url}{query_string}")

    ## retrieve organism metadata from EuPathDB
    metadata_json <- glue::glue("{build_dir}/metadata.json")
    file <- download.file(url = request_url, destfile = metadata_json)
  }

  result <- try(jsonlite::fromJSON(metadata_json), silent = TRUE)
<<<<<<< HEAD
>>>>>>> 13b1ccc (updated email; coding style tweaks)
=======

>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  if (class(result)[1] == "try-error") {
      stop("There was a parsing failure when reading the metadata.")
  }
<<<<<<< HEAD

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
=======

  # each record contains:
  # id, fields, tables
  records <- result[["response"]][["recordset"]][["records"]]

  # process "fields" portion of response and extract name
  dat <- data.frame(t(sapply(records[["fields"]], function(x) {
    x[, "value"] })),
    stringsAsFactors = FALSE)

  colnames(dat) <- records[["fields"]][[1]][["name"]]

  ## Once again, this is filling in schisto.org, which is weird.
  dat <- mutate_if(
    dat,
    is.character,
    stringr::str_replace_all, pattern = "SchistoDB.org", replacement = "SchistoDB.net")
<<<<<<< HEAD
>>>>>>> 13b1ccc (updated email; coding style tweaks)

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
  ## CamelCase, changed once which are explicitly numeric/URLs, and added a few which are
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
=======

  SourceUrl <- NULL

  metadata <- dat %>%
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
    dplyr::transmute(
<<<<<<< HEAD
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
  metadata <- metadata %>% dplyr::mutate_if(is.character,
                   stringr::str_replace_all,
                   pattern = "Current_Release",
                   replacement = glue::glue("release-{db_version}"))
  ## 2. In the weeks leading up to a new release, the EuPathDB folks change the SourceURL column
  ##    to reflect the coming database version before it actually exists.  Thus during that time
  ##    downloads will fail unless the database version is substituted back in.
  metadata <- metadata %>% dplyr::mutate("SourceUrl" = gsub(pattern = "DB-(\\d\\d)_",
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
  metadata[["Tags"]] <- sapply(metadata[["DataProvider"]], function(x) {
    tag_strings[[x]]
  })
=======
             "BiocVersion" = as.character(bioc_version),
             "Genome" = sub(".gff", "", basename(.data[["URLgff"]])),
             "NumGenes" = .data[["genecount"]],
             "NumOrthologs" = .data[["orthologcount"]],
             "SourceType" = "GFF",
             "SourceUrl" = .data[["URLgff"]],
             "SourceVersion" = db_version,
             "Species" = .data[["organism"]],
             "TaxonomyId" = .data[["ncbi_tax_id"]],
             "Coordinate_1_based" = TRUE,
             "DataProvider" = .data[["project_id"]],
             "Maintainer" = "Keith Hughitt <keith.hughitt@nih.gov>") %>%
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
>>>>>>> 13b1ccc (updated email; coding style tweaks)

  ## overide missing taxonomy ids for strains where it can be assigned; ideally
  ## OrgDb and GRanges objects should not depend on taxonomy id information since
  ## this precludes the inclusion of a lot of prokaryotic resources.

  ## In addition, exclude remaining species which are missing taxonomy information from
  ## the metadata; cannot construct GRanges/OrgDb instances for them since they are
  ## have no known taxonomy id, and are not in available.species()
<<<<<<< HEAD

  ## This line is not currently used, it probably should be used to subset out the entries
  ## for which the taxonomy information is invalid.  I think I did not use it because
  ## I later added some heuristics to hunt down the missing entries, and actually doing
  ## the subset based on this results in the loss of too many species.
  na_idx <- is.na(metadata[["TaxonomyID"]])
=======
  na_ind <- is.na(metadata[["TaxonomyId"]])

>>>>>>> 13b1ccc (updated email; coding style tweaks)
  ## I think I will try to hack around this problem.
  metadata[["TaxonomyID"]] <- as.numeric(metadata[["TaxonomyID"]])

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
  metadata[["Species"]] <- ""
  metadata[["Strain"]] <- ""

<<<<<<< HEAD
  ## Include the package names for the various data types along with the most likely
  ## useful separations of the taxon name (e.g. The Genus, Species, Strain, etc.)
  for (i in 1:nrow(metadata)) {
    metadatum <- metadata[i, ]
=======
  ## Also double-check the taxon IDs
  db_version <- metadata[1, "SourceVersion"]

  ## A couple changes to try to make the metadata I generate pass
<<<<<<< HEAD
  for (it in 1:nrow(metadata)) {
    metadatum <- metadata[it, ]
<<<<<<< HEAD:R/download_eupath_metadata.R
>>>>>>> 13b1ccc (updated email; coding style tweaks)
    ## In most invocations of make_taxon_names and get_eupath_pkgnames,
=======
=======
  for (i in 1:nrow(metadata)) {
    entry <- metadata[i, ]
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
    ## In most invocations of make_taxon_names and get_eupathdb_pkgnames,
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/download_eupathdb_metadata.R
    ## we use the column 'TaxonUnmodified', because we are modifying Species to
    ## match what is acquired from GenomeInfoDb::loadTaxonomyDb().
    ## But, right now we are in the process of making that match, so use the
    ## Species column here.
<<<<<<< HEAD
<<<<<<< HEAD:R/download_eupath_metadata.R
<<<<<<< HEAD
    pkg_names <- get_eupath_pkgnames(metadatum, column = "TaxonomyName")
    species_info <- make_taxon_names(metadatum, column = "TaxonomyName")
=======
    pkg_names <- get_eupathdb_pkgnames(entry, column = "Species")
    species_info <- make_taxon_names(entry, column = "Species")

>>>>>>> cc20d16 (Continuing clean-up / re-organization)
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
<<<<<<< HEAD
    metadata[i, "GenusSpecies"] <- gsub(x = species_info[["genus_species"]],
                                    pattern = "\\.", replacement = " ")
    metadata[i, "Strain"] <- species_info[["strain"]]
    metadata[i, "Genus"] <- species_info[["genus"]]
    metadata[i, "Species"] <- species_info[["species"]]
    metadata[i, "Taxon"] <- gsub(x = species_info[["taxon"]],
                                  pattern = "\\.", replacement = " ")
    metadata[i, "TaxonUnmodified"] <- species_info[["unmodified"]]
  }

  ## Use the xref_() functions to try to ensure that we find valid taxonomy names
  ## and identifiers for as many species as possible.
  ## There are two things we need to successfully cross reference:
  ##  1.  The taxonomy IDs from GenomeInfoDB
  ##  2.  The species names provided by AnnotationHubData's getSpeciesList().
  taxa_xref <- xref_taxonomy(metadata, verbose = verbose,
                             species_column = "TaxonomyName",
                             taxon_column = "TaxonomyID")
  species_xref <- xref_species(valid = taxa_xref[["matched_metadata"]],
                               invalid = taxa_xref[["unmatched_metadata"]],
                               taxon_column = "TaxonUnmodified",
                               species_column = "GenusSpecies",
                               verbose = verbose)
  ## Hopefully now, the TaxonXref column contains only things which match getSpeciesList()
  ## and the TaxonomyID column contains only things in the GenomeInfoDb.

  ## if enabled, limit metadata table to N entries;
  if (limit_n < Inf & limit_n < nrow(species_xref[["valid"]])) {
    set.seed(1)
    info(sprintf("Limiting metadata results to %d entries", limit_n))
    ind <- sample(nrow(species_xref[["valid"]]), limit_n)
    species_xref[["valid"]] <- species_xref[["valid"]][ind, ]
  }

  ## To satisfy readr which detects the BiocVersion as a double sometimes and character others
  species_xref[["valid"]][["BiocVersion"]] <- as.numeric(species_xref[["valid"]][["BiocVersion"]])
  species_xref[["invalid"]][["BiocVersion"]] <- as.numeric(species_xref[["invalid"]][["BiocVersion"]])

  ## Write out the metadata and finish up.
=======
    pkg_names <- get_eupath_pkgnames(metadatum, column = "Species")
=======
    pkg_names <- get_eupathdb_pkgnames(metadatum, column = "Species")
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/download_eupathdb_metadata.R
    species_info <- make_taxon_names(metadatum, column = "Species")

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
      gsub(x = metadata[it, "OrgdbPkg"], pattern = "db$", replacement = "sqlite"))
    metadata[it, "TxdbPkg"] <- pkg_names[["txdb"]]
    metadata[it, "TxdbFile"] <- file.path(
      dir, "TxDb", metadata[it, "BiocVersion"],
      glue::glue("{metadata[it, 'TxdbPkg']}.sqlite"))
    metadata[it, "Species"] <- gsub(x = species_info[["genus_species"]],
=======
    metadata[i, "Species"] <- gsub(x = species_info[["genus_species"]],
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
                                    pattern = "\\.", replacement = " ")
    metadata[i, "Strain"] <- species_info[["strain"]]
    metadata[i, "Genus"] <- species_info[["genus"]]
    metadata[i, "Sp"] <- species_info[["species"]]
    metadata[i, "Taxon"] <- gsub(x = species_info[["taxon"]],
                                  pattern = "\\.", replacement = " ")
    metadata[i, "TaxonUnmodified"] <- species_info[["unmodified"]]
  }

  taxa_xref <- xref_taxonomy(metadata, verbose = verbose)

  species_xref <- xref_species(valid = taxa_xref[["matched_metadata"]],
                               invalid = taxa_xref[["unmatched_metadata"]],
                               verbose = verbose)
<<<<<<< HEAD
>>>>>>> 13b1ccc (updated email; coding style tweaks)
  if (isTRUE(write_csv)) {
<<<<<<< HEAD
<<<<<<< HEAD:R/download_eupath_metadata.R
    message("Writing EuPathDB metadata csv files.")
    written <- write_eupath_metadata(species_xref[["valid"]], webservice,
<<<<<<< HEAD
                                     file_type = "valid",
                                     build_dir = build_dir)
    invalid_written <- write_eupath_metadata(species_xref[["invalid"]], webservice,
                                             file_type="invalid",
                                             build_dir = build_dir)
=======
=======
    message("Writing csv files.")
    written <- write_eupathdb_metadata(species_xref[["valid"]], webservice,
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/download_eupathdb_metadata.R
                                     bioc_version, db_version, type = "valid")
    invalid_written <- write_eupathdb_metadata(species_xref[["invalid"]], webservice,
                                             bioc_version, db_version, type = "invalid")
>>>>>>> 13b1ccc (updated email; coding style tweaks)
=======
    message("[Info] Writing EuPathDB metadata csv files.")
=======

  # if enabled, limit metadata table to N entries;
  if (limit_n < Inf && limit_n < nrow(species_xref$valid)) {
    set.seed(1)
    info(sprintf("Limiting metadata results to %d entries", limit_n))

    ind <- sample(nrow(species_xref$valid), limit_n)
    species_xref$valid <- species_xref$valid[ind, ]
  }

  if (isTRUE(write_csv)) {
    info("Writing EuPathDB metadata csv files.")
>>>>>>> fc81572 (Some more refactoring / fixes)

    written <- write_eupathdb_metadata(species_xref[["valid"]], webservice,
                                       bioc_version, db_version, file_type = "valid",
                                       build_dir = build_dir)
    invalid_written <- write_eupathdb_metadata(species_xref[["invalid"]], webservice,
                                               bioc_version, db_version, file_type = "invalid",
                                               build_dir = build_dir)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  }
  retlist <- list(
    "valid" = species_xref[["valid"]],
    "invalid" = species_xref[["invalid"]])
  return(retlist)
}
