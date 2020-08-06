#' Use the post interface to get SNP data.
#'
#' @param entry The full annotation entry.
#' @param dir Location to which to save intermediate savefile.
#' @param overwrite Overwrite the savefile when attempting a redo?
<<<<<<< HEAD:R/post_eupath_snps.R
#' @return A big honking table.
post_eupath_snp_table <- function(entry = NULL, dir = "EuPathDB", overwrite = FALSE) {
=======
#' @return  A big honking table.
post_eupathdb_snp_table <- function(entry=NULL, dir="EuPathDB", overwrite=FALSE) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_snps.R
  if (is.null(entry)) {
    stop("  Need an entry from the eupathdb.")
  }
  rdadir <- file.path(dir, "rda")
  if (!file.exists(rdadir)) {
    created <- dir.create(rdadir, recursive = TRUE)
  }
  savefile <- file.path(rdadir, glue::glue("{entry[['Genome']]}_snp_table.rda"))
  if (file.exists(savefile)) {
    if (isTRUE(overwrite)) {
      removed <- file.remove(savefile)
    } else {
      message("  Delete the file ", savefile, " to regenerate.")
      result <- new.env()
      load(savefile, envir = result)
      result <- result[["result"]]
      return(result)
    }
  }

  get_chr <- function(entry, excludes = "Choose chromosome") {
    webservice <- tolower(entry[["DataProvider"]])
    tld <- "org"
    if (webservice == "schistodb") {
      tld <- "net"
    }
    request_url <- glue::glue(
                           "http://{webservice}.{tld}/webservices/SnpQuestions/NgsSnpsByLocation.wadl")
    request <- curl::curl(request_url)
    res <- xml2::read_xml(request)
    ##close(request)
    fields <- rvest::xml_nodes(res, xpath = '//*[@name="chromosomeOptionalForNgsSnps"]')[[1]] %>%
      xml2::xml_children() %>%
      xml2::xml_attr("value")
    drop_idx <- is.na(fields)
    fields <- fields[!drop_idx]
    drop_idx <- fields == "none"
    fields <- fields[!drop_idx]
    drop_idx <- grepl(pattern = "^pan_", x = fields)
    fields <- fields[!drop_idx]
    excludes_idx <- fields %in% excludes
    fields <- fields[!excludes_idx]
    return(fields)
  }

  species <- entry[["TaxonUnmodified"]]

  all_chrs <- get_chr(entry)
  all_snps <- data.table()
  for (chr in all_chrs) {
    parameters <- list("organismSinglePick" = jsonlite::unbox(species),
                       "chromosomeOptionalForNgsSnps" = jsonlite::unbox(chr),
                       "start_point" = jsonlite::unbox("1"),
                       "end_point" = jsonlite::unbox("0"),
                       "ngsSnp_strain_meta" = jsonlite::unbox('{"filters":[]}'),
                       "WebServicesPath" = jsonlite::unbox("dflt"),
                       "ReadFrequencyPercent" = jsonlite::unbox("80%"),
                       "MinPercentMinorAlleles" = jsonlite::unbox("0"),
                       "MinPercentIsolateCalls" = jsonlite::unbox("20")
                       )
    question <- "SnpQuestions.NgsSnpsByLocation"
    columns <- c("location_text", "snp_location", "is_coding", "position_in_CDS",
                 "ref_aa_with_position", "source_id", "project_id", "PercentMinorAlleles")
<<<<<<< HEAD:R/post_eupath_snps.R
    result <- post_eupath_raw(entry, question = question, columns = columns,
                              parameters = parameters, table_name = "snps")
=======
    result <- post_eupathdb_raw(entry, question=question, columns=columns,
                              parameters=parameters, table_name="snps")
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/post_eupathdb_snps.R
    all_snps <- rbind(all_snps, data.table::as.data.table(result))
  }
  message("  Saving ", savefile)
  save(result, file = savefile)
  return(result)
}
