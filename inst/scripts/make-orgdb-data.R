###############################################################################
#
# Functions for creating GRanges objects from EuPathDB resources
# 
# Author: Keith Hughitt (khughitt@umd.edu)
# Last Update: Sept 01, 2016
#
###############################################################################
library('AnnotationForge')
library('jsonlite')
library('rtracklayer')
library('GenomicFeatures')
source('shared.R')

options(stringsAsFactors=FALSE)

#'
#' Generate OrgDb for EuPathDB organism
#'
#' @param ahm AnnotationHubMetadata instance
#' @return OrgDb instance
#' 
EuPathDBGFFtoOrgDb <- function(ahm) {
    # Get genus and species from organism name
    species_parts <- unlist(strsplit(ahm@Species, ' '))
    genus <- species_parts[1]
    species <- species_parts[2]

    message(sprintf("- Generating OrgDb for %s", ahm@Species))

    # save gff as tempfile
    input_gff <- tempfile(fileext='.gff')
    download.file(ahm@SourceUrl, input_gff)

    # get chromosome information from GFF file
    gff <- import.gff3(input_gff)

    # gene/chr mapping
    genes <- gff[gff$type == 'gene']

    chr_mapping <- data.frame(
        'GID' = genes$ID,
        'CHR' = as.character(seqnames(genes)),
        stringsAsFactors=FALSE
    )

    # get basic gene-related fields
    gene_info <- .extract_gene_info(gff)

    # gene types
    gene_types <- .get_gene_types(ahm@DataProvider, ahm@Species)

    # go terms
    go_table <- .get_go_term_table(ahm@DataProvider, ahm@Species)

    # interpro domains
    interpro_table <- .get_interpro_table(ahm@DataProvider, ahm@Species)

    # ortholog table
    ortholog_table <- .get_ortholog_table(ahm@DataProvider, ahm@Species)

    # create a random directory to use for package build
    sub_dir <- paste0(sample(c(0:9, letters), 10, replace=TRUE), collapse='')
    temp_dir <- file.path(tempdir(), sub_dir)
    dir.create(temp_dir, recursive=TRUE)

    # Compile list of arguments for makeOrgPackage call
    orgdb_args <- list(
        'gene_info'  = gene_info,
        'chromosome' = chr_mapping,
        'go'         = go_table,
        'interpro'   = interpro_table,
        'orthologs'  = ortholog_table,
        'type'       = gene_types,
        'version'    = ahm@SourceVersion,
        'author'     = ahm@Maintainer,
        'maintainer' = ahm@Maintainer,
        'tax_id'     = as.character(ahm@TaxonomyId),
        'genus'      = genus,
        'species'    = species,
        'outputDir'  = temp_dir,
        'goTable'    = "go"
    )

    # NOTE: Aug 20, 2016 - skipping KEGG table for now; will add in once
    # everything else is working...
    # Add KEGG mapping if one exists;
    #if (nrow(kegg_table) > 0) {
    #    orgdb_args[['kegg']] <- kegg_table
    #}

    org_result <- do.call('makeOrgPackage', orgdb_args)
}

#'
#' Extract gene information from a GFF file
#'
#' @param gff GenomicRanges instance as returned by `import.gff3`
#' @param return data.frame containing basic gene information (ID, description,
#' etc.)
#'
.extract_gene_info <- function(gff) {
    # get gene features and convert to a dataframe
    genes <- gff[gff$type == 'gene']
    gene_info <- as.data.frame(elementMetadata(genes))

    # drop any empty and NA columns
    na_mask <- apply(gene_info, 2, function(x) { sum(!is.na(x)) > 0 })
    empty_mask <- apply(gene_info, 2, function(x) { length(unlist(x)) > 0 })
    gene_info <- gene_info[,na_mask & empty_mask]

    # Convert form-encoded description string to human-readable
    gene_info$description <- gsub("\\+", " ", gene_info$description)

    # Normalize columns names
    colnames(gene_info) <- toupper(colnames(gene_info))
    colnames(gene_info)[colnames(gene_info) == "ID"] <- "GID"

    ## Move gid to the front of the line.
    gid_index <- grep("GID", colnames(gene_info))
    gene_info <- gene_info[, c(gid_index, (1:ncol(gene_info))[-gid_index])]
    colnames(gene_info) <- paste0("GENE", colnames(gene_info))
    colnames(gene_info)[1] <- "GID"

    num_rows <- nrow(gene_info)
    gene_info[["GENEALIAS"]] <- as.character(gene_info[["GENEALIAS"]])

    # Remove any newlines present in GENEALIAS field;
    # as.character inserts newlines for objects with >500 characters.
	# https://stat.ethz.ch/R-manual/R-devel/library/base/html/character.html
    gene_info[["GENEALIAS"]] <- gsub('\n', '', gene_info[["GENEALIAS"]])

    return(gene_info)
}

#'
#' Returns a mapping of gene ID to gene type for a specified organism
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#'
#' @return Dataframe with 'GID' and 'TYPE' columns.
#'
.get_gene_types <- function(data_provider, organism) {
    # query EuPathDB API
    res <- .query_eupathdb(data_provider, organism, 'o-fields=gene_type')
    dat <- res$response$recordset$records

    # get vector of types
    types <- sapply(dat$fields, function (x) x[,'value'])

    # return as dataframe
    data.frame(GID=dat$id, TYPE=types, stringsAsFactors=FALSE)
}

#'
#' Returns a mapping of gene ID to GO terms for a specified organism
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#'
#' @return Dataframe with 'GID', 'GO', and 'EVIDENCE' fields
#'
.get_go_term_table <- function(data_provider, organism) {
    # retrieve GoTerms table
    result <- .retrieve_eupathdb_table(data_provider, organism, 'GoTerms')

    # fix column names and return result
    colnames(result) <- c("GID", "GO", "ONTOLOGY", "GO_TERM_NAME", "SOURCE",
                          "EVIDENCE", "IS_NOT")

    # drop everything except for GID, GO, and EVIDENCE columns
    result <- result[,colnames(result) %in% c('GID', 'GO', 'EVIDENCE')]

    # remove duplicated entries resulting from alternative sources / envidence
    # codes
    result <- result[!duplicated(result),]

    return(result)
}

#'
#' Returns a mapping of gene ID to InterPro domains for a specified organism
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#'
#' @return Dataframe with ....
#'
.get_interpro_table <- function(data_provider, organism) {
    # retrieve InterPro domain table
    result <- .retrieve_eupathdb_table(data_provider, organism, 'InterPro')

    # fix numeric types
    result$interpro_e_value <- as.numeric(result$interpro_e_value)
    result$interpro_start_min <- as.numeric(result$interpro_start_min)
    result$interpro_end_min <- as.numeric(result$interpro_end_min)

    # replace NA's with empty strings (occur in INTERPRO_FAMILY_ID and
    # INTERPRO_SECONDARY_ID fields)
    result[is.na(result)] <- ''

    # fix column names and return result
    colnames(result) <- toupper(colnames(result)) 

    return(result)
}

#'
#' Returns a mapping of gene ID to ortholog genes
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#'
#' @return Dataframe with ....
#'
.get_ortholog_table <- function(data_provider, organism) {
    # retrieve ortholog domain table
    result <- .retrieve_eupathdb_table(data_provider, organism, 'Orthologs')

    # fix column names and return result
    colnames(result) <- c("GID", "GO", "ONTOLOGY", "GO_TERM_NAME", "SOURCE",
                          "EVIDENCE", "IS_NOT")

    # drop everything except for GID, GO, and EVIDENCE columns
    result <- result[,colnames(result) %in% c('GID', 'GO', 'EVIDENCE')]

    # remove duplicated entries resulting from alternative sources / envidence
    # codes
    result <- result[!duplicated(result),]

    return(result)
}
