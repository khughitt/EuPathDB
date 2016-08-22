###############################################################################
#
# Functions for parsing EuPathDb resources
# 
# Author: Keith Hughitt (khughitt@umd.edu)
# Last Update: Aug 20, 2016
#
# Questions:
#
###############################################################################
options(stringsAsFactors=FALSE)

#'
#' Generate TxDb for EuPathDB organism
#'
#' @param ahm AnnotationHubMetadata instance
#' @return TxDb instance
#' 
EuPathDBGFFtoTxDb <- function(ahm) {
    # save gff as tempfile
    input_gff <- tempfile()
    download.file(ahm@SourceUrl, input_gff)

    message(sprintf("- Generating TxDb for %s", ahm@Species))

    # get chromosome information from GFF file
    gff <- rtracklayer::import.gff3(input_gff)
    ch <- gff[gff$type == 'chromosome']

    chr_info <- data.frame(
        'chrom'=ch$ID,
        'length'=width(ch),
        'is_circular'=rep(FALSE, length(ch))
    )

    # NOTE: warning to look into when creating txdb for LmjF
    # Warning message:
    # In makeTxDbFromGRanges(gr, metadata = metadata) :
    #  The following transcripts were dropped because their exon ranks could not be
    #  inferred (either because the exons are not on the same chromosome/strand or
    #  because they are not separated by introns): rna_LmjF.28.2965-1
    makeTxDbFromGFF(input_gff,
                    dataSource=dirname(ahm@SourceUrl),
                    organism=ahm@Species,
                    taxonomyId=ahm@TaxonomyId,
                    chrominfo=chr_info)
}


#'
#' Generate OrgDb for EuPathDB organism
#'
#' @param ahm AnnotationHubMetadata instance
#' @return TxDb instance
#' 
EuPathDBGFFtoOrgDb <- function(ahm) {
    # Get genus and species from organism name
    species_parts <- unlist(strsplit(ahm@Species, ' '))
    genus <- species_parts[1]
    species <- species_parts[2]

    message(sprintf("- Generating OrgDb for %s", ahm@Species))

    # save gff as tempfile
    input_gff <- tempfile()
    download.file(ahm@SourceUrl, input_gff)

    # get chromosome information from GFF file
    gff <- rtracklayer::import.gff3(input_gff)

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
    gene_types <- .get_gene_types(ahm@DataProvider, organism)

    # go terms
    go_table <- .get_go_terms(ahm@DataProvider, organism)

    # Compile list of arguments for makeOrgPackage call
    orgdb_args <- list(
        'gene_info'  = gene_info,
        'chromosome' = chr_mapping,
        'go'         = go_table,
        'type'       = gene_types,
        'version'    = ahm@SourceVersion,
        'author'     = ahm@Maintainer,
        'maintainer' = ahm@Maintainer,
        'tax_id'     = as.character(ahm@TaxonomyId),
        'genus'      = genus,
        'species'    = species,
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
    gene_info <- as.data.frame(elementMetadata(genes), stringsAsFactors=FALSE)

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
#'
#'
.get_go_terms <- function(data_provider, organism) {
    # query EuPathDB API
    res <- .query_eupathdb(data_provider, organism, 'o-tables=GoTerms')
    dat <- res$response$recordset$records

    # drop genes with no associated GO terms
    gene_mask <- sapply(dat[,'tables'], function(x) { length(x$rows[[1]]) > 0})
    dat <- dat[gene_mask,]

    # create empty data frame to store result in
    result <- data.frame(stringsAsFactors=FALSE)

    # iterate over remaining genes and extract GO annotations for them
    for (i in 1:nrow(dat)) {
        # example entry:
        # 
        # > dat$tables[[1]]$rows[[1]]$fields[[1]]
        #         name                      value
        # 1         go_id                 GO:0007018
        # 2      ontology         Biological Process
        # 3  go_term_name microtubule-based movement
        # 4        source                   Interpro
        # 5 evidence_code                        IEA
        # 6        is_not                       <NA>
        gene_go_terms <- dat$tables[[i]]
        rows <- t(sapply(gene_go_terms$rows[[1]]$fields, function(x) { as.vector(x$value) }))
        result <- rbind(result, cbind(dat$id[i], rows))
    }

    # fix column names and return result
    colnames(result) <- c("GID", "GO", "ONTOLOGY", "GO_TERM_NAME", "SOURCE",
                          "EVIDENCE", "IS_NOT")

    # drop everything except for GID, GO, and EVIDENCE columns
    result <- result[,colnames(result) %in% c('GID', 'GO', 'EVIDENCE')]

    return(result)
}

#'
#' Queries one of the EuPathDB APIs and returns a dataframe representation
#' of the result.
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#' @param query_args String of additional query arguments
#' @param wadl String specifying API service to be queried
#' @param format String specifying API response type (currently only 'json'
#'        is supported)
#' @return list containing response from API request.
#'
#' More information
#' ----------------
#' 1. http://tritrypdb.org/tritrypdb/serviceList.jsp
#'
.query_eupathdb <- function(data_provider, organism, query_args, wadl='GeneQuestions/GenesByTaxon', format='json') {
    # construct API query
    base_url <- sprintf('http://%s.org/webservices/%s.%s?', 
                        tolower(data_provider), wadl, format)
    query_string <- sprintf('organism=%s&%s', URLencode(organism), query_args)
    request_url <- paste0(base_url, query_string)

    message(sprintf("- Querying %s", request_url))

    # query API for gene types
    if (format == 'json') {
        jsonlite::fromJSON(request_url)
    } else {
        stop("Invalid response type specified.")
    }
}
