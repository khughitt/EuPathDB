#' Returns metadata for all eupathdb organisms.
#'
#' @param overwrite Overwrite existing data?
#' @param webservice Optional alternative webservice for hard-to-find species.
#' @param bioc_version Manually set the bioconductor release if desired.
#' @param dir Where to put the json.
#' @param version Choose a specific eupathdb version?
#' @param write_csv Write a csv file in the format expected by AnnotationHubData?
#' @param verbose Print helper message about species matching?
#' @return Dataframe with lots of rows for the various species in eupathdb.
#' @author Keith Hughitt
#' @export
download_eupath_metadata <- function(overwrite=FALSE, webservice="eupathdb",
                                     bioc_version=NULL, dir="EuPathDB",
                                     version=NULL, write_csv=FALSE, verbose=FALSE) {
  ## Get EuPathDB version (same for all databases)
  if (webservice == "eupathdb") {
    projects <- c("amoebadb", "cryptodb", "fungidb", "giardiadb",
                  "microsporidiadb", "piroplasmadb", "plasmodb",
                  "schistodb", "toxodb", "trichdb", "tritrypdb")
    valid_metadata <- data.frame()
    invalid_metadata <- data.frame()
    for (p in projects) {
      project_metadata <- download_eupath_metadata(webservice=p, overwrite=overwrite,
                                                   bioc_version=bioc_version, dir=dir,
                                                   version=version, write_csv=write_csv)
      valid_metadata <- rbind(valid_metadata, project_metadata[["valid"]])
      invalid_metadata <- rbind(invalid_metadata, project_metadata[["invalid"]])
    }
    return(list("valid" = valid_metadata, "invalid" = invalid_metadata))
  }

  if (!file.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }

  ## For when releasing a new bioconductor release which I don't yet have.
  if (is.null(bioc_version)) {
    bioc_version <- BiocInstaller::biocVersion()
  }

  db_version <- NULL
  if (is.null(version)) {
    ## One could just as easily choose any of the other eupathdb hosts.
    db_version <- readLines("http://tritrypdb.org/common/downloads/Current_Release/Build_number")
  } else {
    db_version <- version
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

  service_directory <- prefix_map(webservice)
  ## construct API request URL
  base_url <- glue::glue("https://{webservice}.org/{service_directory}/webservices/")
  if (webservice == "schistodb") {
    ## WTF? There is some weird politics going on here I bet.
    base_url <- glue::glue("https://{webservice}.net/{service_directory}/webservices/")
  }
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
  ##message("Downloaded: ", request_url)

  ## convert to a dataframe
  dat <- data.frame(t(sapply(records[["fields"]], function(x) {
    x[, "value"] })),
    stringsAsFactors=FALSE)
  colnames(dat) <- records[["fields"]][[1]][["name"]]

  ## The current version of the database remains 39; but the sourceUrl returned
  ## by the above json query is 40.  As a result, attempted downloads fail due
  ## to the mismatch in filenames/directories.
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

  ## I am not sure that this is needed anymore, since I cross reference against ncbi myself now.
  known_taxon_ids <- data.frame(
    species=c("Ordospora colligata OC4",
              "Trypanosoma cruzi CL Brener Esmeraldo-like",
              "Trypanosoma cruzi CL Brener Non-Esmeraldo-like"),
    taxonomy_id=c("1354746", "353153", "353153"),
    stringsAsFactors=FALSE)

  taxon_mask <- metadata[["Species"]] %in% known_taxon_ids[["species"]]
  ind <- match(metadata[taxon_mask, "Species"], known_taxon_ids[["species"]])
  metadata[taxon_mask, ][["TaxonomyId"]] <- as.character(
    known_taxon_ids[["taxonomy_id"]][ind])

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
  metadata[["Strain"]] <- ""
  metadata[["Taxon"]] <- ""
  metadata[["Genus"]] <- ""
  metadata[["Sp"]] <- ""
  ## Also double-check the taxon IDs
  all_taxa_ids <- GenomeInfoDb::loadTaxonomyDb()
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
    metadata[it, "GrangesPkg"] <- pkg_names[["granges"]]
    metadata[it, "OrganismdbiPkg"] <- pkg_names[["organismdbi"]]
    metadata[it, "OrgdbPkg"] <- pkg_names[["orgdb"]]
    metadata[it, "TxdbPkg"] <- pkg_names[["txdb"]]
    metadata[it, "Species"] <- gsub(x=species_info[["genus_species"]],
                                    pattern="\\.", replacement=" ")
    metadata[it, "Strain"] <- species_info[["strain"]]
    metadata[it, "Taxon"] <- gsub(x=species_info[["taxon"]],
                                  pattern="\\.", replacement=" ")
    metadata[it, "TaxonUnmodified"] <- species_info[["unmodified"]]
    found_genus_taxa_idx <- which(all_taxa_ids[["genus"]] %in% species_info[["genus"]])
    if (length(found_genus_taxa_idx) > 0) {
      subset_taxa <- all_taxa_ids[found_genus_taxa_idx, ]
      found_species_taxa_idx <- which(subset_taxa[["species"]] %in% species_info[["species"]])
      if (length(found_species_taxa_idx) > 0) {
        taxa_ids <- subset_taxa[found_species_taxa_idx, ]
        taxon_id <- taxa_ids[1, "tax_id"]
        if (is.na(metadata[it, "TaxonomyId"])) {
          if (isTRUE(verbose)) {
            message("Setting the taxonomy id from GenomeInfoDb for ", metadata[it, "Species"], ".")
          }
          metadata[it, "TaxonomyId"] <- taxon_id
        } else if (metadata[it, "TaxonomyId"] != taxon_id) {
          if (isTRUE(verbose)) {
            message("The taxonomy ID from GenomeInfoDb does not match what I have for ",
                    metadata[it, "Species"], ".")
          }
        }
      }
    }
  }
  eupathdb_version <- metadata[1, "SourceVersion"]
  ## A couple changes to try to make the metadata I generate pass
  ## AnnotationHubData::makeAnnotationHubMetadata()
  ## AnnotationHubData expects species suffixes to be 'sp.'
  metadata[["Species"]] <- gsub(x=metadata[["Species"]],
                                pattern=" sp $",
                                replacement=" sp",
                                perl=TRUE)
  metadata[["Species"]] <- gsub(x=metadata[["Species"]],
                                pattern=" sp$",
                                replacement=" sp\\.",
                                perl=TRUE)
  ## Having a 'Assemblage' is verboten.
  metadata[["Species"]] <- gsub(x=metadata[["Species"]],
                                pattern=" Assemblage",
                                replacement="",
                                perl=TRUE)
  ## Having some species 'like x' is also verboten.
  metadata[["Species"]] <- gsub(x=metadata[["Species"]],
                                pattern=" like",
                                replacement="",
                                perl=TRUE)

  ## An attempt to get as many species from AnnotationHub as possible.
  testing_metadata <- metadata
  valid_metadata <- data.frame()
  invalid_metadata <- data.frame()
  all_valid_species <- AnnotationHubData::getSpeciesList()
  valid_idx <- testing_metadata[["TaxonUnmodified"]] %in% all_valid_species
  if (isTRUE(verbose)) {
    message("Added ", sum(valid_idx), " species without changing anything out of ",
            nrow(testing_metadata), ".")
  }
  if (sum(valid_idx) > 0) {
    ## Add the stuff which was found to the set of valid entries.
    valid_metadata <- testing_metadata[which(valid_idx), ]
    ## Then remove them from the set to be tested.
    testing_metadata <- testing_metadata[which(!valid_idx), ]
    ## Set the 'Species' column to taxonunmodified
    valid_metadata[["Species"]] <- valid_metadata[["TaxonUnmodified"]]
  }
  if (isTRUE(verbose)) {
    message("Now there are: ", nrow(testing_metadata), " rows left.")
  }
  valid_idx <- testing_metadata[["Species"]] %in% all_valid_species
  message("Added ", sum(valid_idx), " species after using only the genus species.")
  if (sum(valid_idx) > 0) {
    new_metadata <- testing_metadata[which(valid_idx), ]
    ## Pull out the new valid entries
    valid_metadata <- rbind(valid_metadata, new_metadata)
    ## Add those to the valid metadata.
    invalid_metadata <- testing_metadata[which(!valid_idx), ]
    ## Add whatever is left to the set of invalid metadata.
  }
  if (nrow(invalid_metadata) > 0) {
    message("Unable to find species names for ", nrow(invalid_metadata), " species.")
    message(toString(invalid_metadata[["Species"]]))
  }
  metadata <- valid_metadata

  if (isTRUE(write_csv)) {
    granges_metadata <- metadata %>%
      dplyr::mutate(
               Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
               Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
transcript information for {.data[['Taxon']]}"),
               RDataClass="GRanges",
               DispatchClass="GRanges",
               ResourceName=.data[["GrangesPkg"]],
               RDataPath=file.path(
                 dir, "GRanges", .data[["BiocVersion"]],
                 glue::glue("{.data[['GrangesPkg']]}")))
    csv_file <- file.path(
      path.package("EuPathDB"),
      "inst", "extdata",
      glue::glue("GRanges_bioc_v{bioc_version}_eupathdb_v{eupathdb_version}_metadata.csv"))
    if (file.exists(csv_file)) {
      readr::write_csv(x=granges_metadata, path=csv_file, append=TRUE)
    } else {
      readr::write_csv(x=granges_metadata, path=csv_file, append=FALSE, col_names=TRUE)
    }

    orgdb_metadata <- metadata %>%
      dplyr::mutate(
               Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
               Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
annotations for {.data[['Taxon']]}"),
               RDataClass="OrgDb",
               DispatchClass="SQLiteFile",
               ResourceName=.data[["OrgdbPkg"]],
               RDataPath=file.path(
                 dir, "OrgDb", .data[["BiocVersion"]],
                 glue::glue("{.data[['OrgdbPkg']]}.eg.sqlite")))
    csv_file <- file.path(
      path.package("EuPathDB"),
      "inst", "extdata",
      glue::glue("OrgDb_bioc_v{bioc_version}_eupathdb_v{eupathdb_version}_metadata.csv"))
    if (file.exists(csv_file)) {
      readr::write_csv(x=orgdb_metadata, path=csv_file, append=TRUE)
    } else {
      readr::write_csv(x=orgdb_metadata, path=csv_file, append=FALSE, col_names=TRUE)
    }

    txdb_metadata <- metadata %>%
      dplyr::mutate(
               Title=glue::glue("Transcript information for {.data[['Taxon']]}"),
               Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Transcript information for {.data[['Taxon']]}"),
               RDataClass="TxDb",
               DispatchClass="SQLiteFile",
               ResourceName=.data[["TxdbPkg"]],
               RDataPath=file.path(
                 dir, "TxDb", .data[["BiocVersion"]],
                 glue::glue("{.data[['TxdbPkg']]}.sqlite")))
    csv_file <- file.path(
      path.package("EuPathDB"),
      "inst", "extdata",
      glue::glue("TxDb_bioc_v{bioc_version}_eupathdb_v{eupathdb_version}_metadata.csv"))
    if (file.exists(csv_file)) {
      readr::write_csv(x=txdb_metadata, path=csv_file, append=TRUE)
    } else {
      readr::write_csv(x=txdb_metadata, path=csv_file, append=FALSE, col_names=TRUE)
    }

    organismdbi_metadata <- metadata %>%
      dplyr::mutate(
               Title=glue::glue("Combined information for {.data[['Taxon']]}"),
               Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Combined information for {.data[['Taxon']]}"),
               RDataClass="OrganismDBI",
               DispatchClass="SQLiteFile",
               ResourceName=.data[["OrganismdbiPkg"]],
               RDataPath=file.path(
                 dir, "OrganismDbi", .data[["BiocVersion"]],
                 glue::glue("{.data[['OrganismdbiPkg']]}"),
                 "graphInfo.rda"))
    csv_file <- file.path(
      path.package("EuPathDB"),
      "inst", "extdata",
      glue::glue("OrganismDbi_bioc_v{bioc_version}_eupathdb_v{eupathdb_version}_metadata.csv"))
    if (file.exists(csv_file)) {
      readr::write_csv(x=organismdbi_metadata, path=csv_file, append=TRUE)
    } else {
      readr::write_csv(x=organismdbi_metadata, path=csv_file, append=FALSE, col_names=TRUE)
    }

    bsgenome_metadata <- metadata %>%
      dplyr::mutate(
               Title=glue::glue("Genome for {.data[['Taxon']]}"),
               Description=glue::glue("{.data[['DataProvider']]} {.data[['SourceVersion']]} \\
Genome for {.data[['Taxon']]}"),
               RDataClass="BSGenome",
               DispatchClass="2bit",
               ResourceName=.data[["BsgenomePkg"]],
               RDataPath=file.path(
                 dir, "BSgenome", .data[["BiocVersion"]],
                 glue::glue("{.data[['BsgenomePkg']]}"),
                 "single_sequences.2bit"))
    csv_file <- file.path(
      path.package("EuPathDB"),
      "inst", "extdata",
      glue::glue("BSgenome_bioc_v{bioc_version}_eupathdb_v{eupathdb_version}_metadata.csv"))
    if (file.exists(csv_file)) {
      readr::write_csv(x=bsgenome_metadata, path=csv_file, append=TRUE)
    } else {
      readr::write_csv(x=bsgenome_metadata, path=csv_file, append=FALSE, col_names=TRUE)
    }
  }

  retlist <- list(
    "valid" = metadata,
    "invalid" = invalid_metadata)
  return(retlist)

}
