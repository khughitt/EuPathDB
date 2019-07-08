#' Load organism annotation data from an orgdb sqlite package.
#'
#' Creates a dataframe gene and transcript information for a given set of gene
#' ids using the AnnotationDbi interface.
#'
#' Tested in test_45ann_organdb.R
#' This defaults to a few fields which I have found most useful, but the brave
#' or pathological can pass it 'all'.
#'
#' @param orgdb OrganismDb instance.
#' @param gene_ids  Search for a specific set of genes?
#' @param include_go  Ask the Dbi for gene ontology information?
#' @param keytype mmm the key type used?
#' @param strand_column  There are a few fields I want to gather by default:
#'   start, end, strand, chromosome, type, and name; but these do not
#'   necessarily have consistent names, use this column for the chromosome
#'   strand.
#' @param start_column  Use this column for the gene start.
#' @param end_column  Use this column for the gene end.
#' @param chromosome_column  Use this column to identify the chromosome.
#' @param type_column  Use this column to identify the gene type.
#' @param name_column Use this column to identify the gene name.
#' @param fields Columns included in the output.
#' @param sum_exon_widths Perform a sum of the exons in the data set?
#' @return Table of geneids, chromosomes, descriptions, strands, types, and lengths.
#' @seealso \pkg{AnnotationDbi} \pkg{GenomicFeatures} \pkg{BiocGenerics}
#'  \code{\link[AnnotationDbi]{columns}} \code{\link[AnnotationDbi]{keytypes}}
#'  \code{\link[AnnotationDbi]{select}} \code{\link[GenomicFeatures]{exonsBy}}
#' @examples
#' \dontrun{
#'  one_gene <- load_orgdb_annotations(org, c("LmJF.01.0010"))
#' }
#' @author atb
#' @export
load_orgdb_annotations <- function(orgdb=NULL, gene_ids=NULL, include_go=FALSE,
                                   keytype="gid", strand_column="cdsstrand",
                                   start_column="cdsstart", end_column="cdsend",
                                   chromosome_column="cdschrom",
                                   type_column="gene_type", name_column="cdsname",
                                   fields=NULL, sum_exon_widths=FALSE) {
  if (is.null(orgdb)) {
    message("Assuming Homo.sapiens.")
    org_pkgstring <- "library(Homo.sapiens); orgdb <- Homo.sapiens"
    eval(parse(text=org_pkgstring))
  } else if ("character" %in% class(orgdb)) {
    org_pkgstring <- glue::glue("library({orgdb}); orgdb <- {orgdb}")
    eval(parse(text=org_pkgstring))
  }
  keytype <- toupper(keytype)
  strand_column <- toupper(strand_column)
  start_column <- toupper(start_column)
  end_column <- toupper(end_column)
  chromosome_column <- toupper(chromosome_column)
  type_column <- toupper(type_column)
  name_column <- toupper(name_column)
  ## Caveat: if fields was NULL, now it is character(0)
  fields <- toupper(fields)
  all_fields <- AnnotationDbi::columns(orgdb)
  chosen_fields <- c()

  if (! name_column %in% all_fields) {
    a_name <- grepl(pattern="NAME", x=all_fields)
    new_name_column <- all_fields[a_name][1]
    message("Unable to find ", name_column, ", setting it to ", new_name_column, ".")
    name_column <- new_name_column
  }
  if (! type_column %in% all_fields) {
    message("Unable to find ", type_column, " in the db, removing it.")
    type_column <- NULL
  }
  if (! chromosome_column %in% all_fields) {
    message("Unable to find ", chromosome_column, " in the db, removing it.")
    chromosome_column <- NULL
  }
  if (! strand_column %in% all_fields) {
    message("Unable to find ", strand_column, " in the db, removing it.")
    strand_column <- NULL
  }
  if (! start_column %in% all_fields) {
    message("Unable to find ", start_column, " in the db, removing it.")
    start_column <- NULL
  }
  if (! end_column %in% all_fields) {
    message("Unable to find ", end_column, " in the db, removing it.")
    end_column <- NULL
  }

  if (length(fields) == 0) {
    chosen_fields <- c(name_column, type_column, chromosome_column, strand_column,
                       start_column, end_column)
  } else {
    chosen_fields <- c(name_column, type_column, chromosome_column, strand_column,
                       start_column, end_column, fields)
  }

  if (sum(chosen_fields %in% all_fields) != length(chosen_fields)) {
    missing_idx <- ! chosen_fields %in% all_fields
    missing_fields <- chosen_fields[missing_idx]
    found_fields <- chosen_fields %in% all_fields
    chosen_fields <- chosen_fields[found_fields]
    message("Some requested columns are not available: ", toString(missing_fields), ".")
    message("The following are available: ", toString(all_fields))
  }

  if (chosen_fields[1] == "all") {
    message("Selecting the following fields, this might be too many: \n",
            toString(all_fields))
    chosen_fields <- all_fields
  }

  ## Gene IDs
  if (is.null(gene_ids)) {
    gene_ids <- try(AnnotationDbi::keys(orgdb, keytype=keytype))
    if (class(gene_ids) == "try-error") {
      if (grepl(x=gene_ids[[1]], pattern="Invalid keytype")) {
        valid_keytypes <- AnnotationDbi::keytypes(orgdb)
        stop("Try using valid keytypes: ", toString(valid_keytypes))
      } else {
        stop("There was an error getting the gene ids.")
      }
    } else {
      message("Extracted all gene ids.")
    }
  }
  ## Note querying by "GENEID" will exclude noncoding RNAs
  message("Attempting to select: ", toString(chosen_fields))
  gene_info <- try(AnnotationDbi::select(
                                    x=orgdb,
                                    keys=gene_ids,
                                    keytype=keytype,
                                    columns=chosen_fields))
  if (class(gene_info) == "try-error") {
    message("Select statement failed, this is commonly because there is no join",
            " between the transcript table and others.")
    message("Thus it says some stupid crap about 'please add gtc to the interpolator'",
            " which I think references select-method.R in GenomicFeatures.")
    message("So, try replacing columns with stuff like 'tx*' with 'cds*'?")
    stop()
  }

  ## Compute total transcript lengths (for all exons)
  ## https://www.biostars.org/p/83901/
  gene_exons <- try(GenomicFeatures::exonsBy(orgdb, by="gene"), silent=TRUE)
  if (class(gene_exons) == "try-error") {
    gene_exons <- NULL
  }
  transcripts <- try(GenomicFeatures::transcripts(orgdb), silent=TRUE)
  if (class(transcripts) == "try-error") {
    transcripts <- NULL
  }
  fivep_utr <- try(GenomicFeatures::fiveUTRsByTranscript(orgdb, use.names=TRUE), silent=TRUE)
  if (class(fivep_utr) == "try-error") {
    fivep_utr <- NULL
  }
  threep_utr <- try(GenomicFeatures::threeUTRsByTranscript(orgdb, use.names=TRUE), silent=TRUE)
  if (class(threep_utr) == "try-error") {
    threep_utr <- NULL
  }
  colnames(gene_info) <- tolower(colnames(gene_info))
  if (isTRUE(sum_exon_widths)) {
    message("Summing exon lengths, this takes a while.")
    lengths <- lapply(gene_exons, function(x) {
      sum(BiocGenerics::width(GenomicRanges::reduce(x)))
    })
    message("Adding exon lengths to the gene_exons.")
    lengths <- as.data.frame(unlist(lengths), stringsAsFactors=FALSE)
    colnames(lengths) <- "transcript_length"
    gene_info <- merge(gene_info, lengths, by.x=keytype, by.y="row.names")
  }
  rownames(gene_info) <- make.names(gene_info[[1]], unique=TRUE)

  retlist <- list(
    "genes" = gene_info,
    "gene_exons" = gene_exons,
    "transcripts" = transcripts,
    "fivep_utr" = fivep_utr,
    "threep_utr" = threep_utr)
  return(retlist)
}