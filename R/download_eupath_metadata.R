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
                                     bioc_version = NULL, build_dir = "EuPathDB",
                                     eu_version = NULL, limit_n = Inf, verbose = FALSE) {
  versions <- get_versions(bioc_version = bioc_version, eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  db_version <- versions[["db_version"]]
  bioc_version <- versions[["bioc_version"]]

  if (isFALSE(overwrite)) {
    message("Checking for existing metadata csv file.")
    file_lst <- get_metadata_filename(webservice, bioc_version, eu_version, build_dir)
    metadata_df <- readr::read_csv(file = file_lst[["all"]], col_types = readr::cols())
    retlist <- list(
      "valid" = metadata_df,
      "invalid" = data.frame())
    return(retlist)
  }

  ## Choose which service(s) to query, if it is 'eupathdb' do them all.
  webservice <- tolower(webservice)
  valid_metadata <- data.frame()
  invalid_metadata <- data.frame()
  if (webservice == "eupathdb") {
    meta_lst <- get_all_metadata()
    return(meta_lst)
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
      warn("API request failed for %s (code = 422).")
      return(data.frame())
  } else if (result[["status_code"]] == "400") {
      ## likely due to bad formatConfig
      warn("API Request failed for %s (code = 400).")
  } else if (result[["status_code"]] == "404") {
      warn("API Request failed for %s (code = 404).")
  } else if (result[["status_code"]] != "200") {
      warn("API Request failed for (code = %d).", result[["status_code"]])
      return(data.frame())
  } else if (length(result[["content"]]) < 100) {
      warn("Very small amount of content returned.")
  }
  cont <- httr::content(result, encoding = "UTF-8", as = "text")
  result <- try(jsonlite::fromJSON(cont, flatten = TRUE))
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
  metadata <- metadata %>% dplyr::mutate_if(is.character,
                   stringr::str_replace_all,
                   pattern = "Current_Release",
                   replacement = glue::glue("release-{db_version}"))
  ## 2. In the weeks leading up to a new release, the EuPathDB folks change the SourceURL column
  ##    to reflect the coming database version before it actually exists.  Thus during that time
  ##    downloads will fail unless the database version is substituted back in.
  ## Shush, R CMD check
  URLGFF <- URLGenome <- URLProtein <- NULL
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

  ## Include the package names for the various data types along with the most likely
  ## useful separations of the taxon name (e.g. The Genus, Species, Strain, etc.)
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

  ## Write out the metadata and finish up.
  written <- write_eupath_metadata(metadata = species_xref[["valid"]],
                                   webservice = webservice,
                                   file_type = "valid",
                                   build_dir = build_dir,
                                   overwrite = overwrite)
  invalid_written <- write_eupath_metadata(metadata = species_xref[["invalid"]],
                                           webservice = webservice,
                                           file_type="invalid",
                                           build_dir = build_dir,
                                           overwrite = overwrite)

  retlist <- list(
    "valid" = species_xref[["valid"]],
    "invalid" = species_xref[["invalid"]])
  return(retlist)
}
