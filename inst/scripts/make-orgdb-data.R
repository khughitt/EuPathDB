#!/usr/bin/env Rscript
###############################################################################
#
# Functions for creating OrgDb objects from EuPathDB resources
#
# Note: Trichomonas vaginalis G3 genome contains significantly
# more (about 4x) genes and orthologs compared to most other
# EuPathDb organisms, and may require much more memory and time
# to build.
#
# Most organisms should finish within several hours and use 10GB memory or
# less. On the larger end, T. vaginalis G3 required 42GB memory and took over
# 12 hours to complete on a PC workstation.
# 
# Author: Keith Hughitt (khughitt@umd.edu)
# Last Update: July 13, 2017
#
# Usage: ./make-orgdb-data.R /path/to/eupathdb/orgdb/33
#
# Note: If the list of available organisms changes, the `availableEuPathDB`
# vector found in R/EuPathDB.R will also need to be updated.
#
###############################################################################
library('AnnotationForge')
library('RSQLite')
library('jsonlite')
library('rtracklayer')
library('GenomicFeatures')
library('doParallel')
library('foreach')
library('httr')

source('shared.R')
options(stringsAsFactors=FALSE)

#'
#' Generate OrgDb for EuPathDB organism
#'
#' @param entry One dimensional dataframe with organism metadata
#' @return OrgDb instance
#' 
EuPathDBGFFtoOrgDb <- function(entry, output_dir) {
    # Get genus and species from organism name
    species_parts <- unlist(strsplit(entry$Species, ' '))
    genus <- species_parts[1]
    species <- species_parts[2]

    # save gff as tempfile
    input_gff <- tempfile(fileext='.gff')
    download.file(entry$SourceUrl, input_gff)

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
    gene_types <- .get_gene_types(entry$DataProvider, entry$Species)

    # go terms
    go_table <- .get_go_term_table(entry$DataProvider, entry$Species)

    # pathways
    pathway_table <- .get_pathway_table(entry$DataProvider, entry$Species)

    # interpro domains
    interpro_table <- .get_interpro_table(entry$DataProvider, entry$Species)

    # ortholog table
    #
    # Note: skipping for organisms with large number of orthologs for now since
    # the requests can be very large (~500Mb) and often fail jsonlite which
    # uses cURL under the hood.
    #
    # A possible work-around would be to manually download the results using
    # wget, e.g.: download.file(url, 'out.json', method='wget')
    #
    #if (entry$NumOrthologs < 20000) {
    if (entry$NumOrthologs < 3000) {
        message(sprintf('- Retrieving %d orthologs for %s', 
                        entry$NumOrthologs, entry$Species))
        ortholog_table <- .get_ortholog_table(entry$DataProvider, entry$Species)
    } else {
        message(sprintf('- Skipping ortholog table for %s', entry$Species))
        ortholog_table <- data.frame()
    }

    # create a randomly-named sub-directory to store orgdb output in; since
    # makeOrganismPackage doesn't incorporate strain information in the 
    # package name, this is necessary to avoid directory name collisions
    build_dir <- file.path(output_dir, paste0(sample(c(0:9, letters), 10, replace=TRUE), collapse=''))
    dir.create(build_dir, recursive=TRUE)

    # Compile list of arguments for makeOrgPackage call
    orgdb_args <- list(
        'gene_info'  = gene_info,
        'chromosome' = chr_mapping,
        'type'       = gene_types,
        'version'    = as.character(entry$SourceVersion),
        'author'     = entry$Maintainer,
        'maintainer' = entry$Maintainer,
        'tax_id'     = as.character(entry$TaxonomyId),
        'genus'      = genus,
        'species'    = species,
        'outputDir'  = build_dir
    )

    # add non-empty tables
    if (nrow(go_table) > 0) {
        orgdb_args[['go']] <- go_table
        'goTable' <- "go"
    }
    if (nrow(pathway_table) > 0) {
        orgdb_args[['pathways']] <- pathway_table
    }
    if (nrow(interpro_table) > 0) {
        orgdb_args[['interpro']] <- interpro_table
    }
    if (nrow(ortholog_table) > 0) {
        orgdb_args[['orthologs']] <- ortholog_table
    }

    # NOTE: Aug 20, 2016 - skipping KEGG table for now; will add in once
    # everything else is working...
    # Add KEGG mapping if one exists;
    #if (nrow(kegg_table) > 0) {
    #    orgdb_args[['kegg']] <- kegg_table
    #}

    message(sprintf("- Calling makeOrgPackage for %s", entry$Species))
    orgdb_path <- do.call('makeOrgPackage', orgdb_args)

    # Fix name in sqlite metadata table
    dbpath <- file.path(orgdb_path, 'inst/extdata', 
                        sub('.db', '.sqlite', basename(orgdb_path)))

    message(sprintf("- Fixing sqlite Orgdb sqlite database %s", dbpath))

    # make sqlite database editable
    Sys.chmod(dbpath, mode='0644')

    db = dbConnect(SQLite(), dbname=dbpath)

    # update SPECIES field
    query <- sprintf('UPDATE metadata SET value="%s" WHERE name="SPECIES";',
                     entry$Species)
    dbSendQuery(conn=db, query)

    # update ORGANISM field
    query <- sprintf('UPDATE metadata SET value="%s" WHERE name="ORGANISM";',
                     entry$Species)
    dbSendQuery(conn=db, query)

    # lock it back down
    Sys.chmod(dbpath, mode='0444')

    # return the path to the sqlite database
    dbpath
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

    # remove problematic GENECOLOUR field if it exists.
    # Found for "Leishmania braziliensis MHOM/BR/75/M2904" -- only one row
    # has a non-empty value and it does not appear to be correct:
    # > gene_info$GENECOLOUR[1859]
    # [[1]]
    # [1] "10"                  "LbrM15.0470"         "LbrM.15.0630"
    # "LbrM15_V2.0630"      "LbrM15_V2.0630:pep"  "LbrM15_V2.0630:mRNA"
    gene_info <- gene_info[,colnames(gene_info) != 'GENECOLOUR']

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

    # fix type for GENEALIAS column if present (found in version 28 and
    # earlier)
    if ("GENEALIAS" %in% colnames(gene_info)) {
        gene_info[["GENEALIAS"]] <- as.character(gene_info[["GENEALIAS"]])

        # Remove any newlines present in GENEALIAS field;
        # as.character inserts newlines for objects with >500 characters.
        # https://stat.ethz.ch/R-manual/R-devel/library/base/html/character.html
        gene_info[["GENEALIAS"]] <- gsub('\n', '', gene_info[["GENEALIAS"]])
    }

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
    res <- .query_eupathdb(data_provider, organism, 
                           list(`o-fields`='primary_key,gene_type'))
    dat <- res$response$recordset$records

    # get vector of types
    ids <- unlist(sapply(dat$fields, function(x) { strsplit(x[,'value'], ',')[1] }))
    types <- unlist(sapply(dat$fields, function(x) { strsplit(x[,'value'], ',')[2] }))

    df <- data.frame(GID=ids, TYPE=types, stringsAsFactors=FALSE)

    # remove duplicated rows and return
    df[!duplicated(df),]
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
    #result <- .retrieve_eupathdb_table(data_provider, organism, 'GoTerms')
    result <- .retrieve_eupathdb_attributes(data_provider, organism, 'GOTerms')

    if (nrow(result) == 0) {
        return(result)
    }

    # convert column names to uppercase for consistency
    colnames(result) <- toupper(colnames(result))

    # drop uneeded columns
    result <- result[,colnames(result) %in% c('GID', 'ONTOLOGY', 'GO_ID',
                                              'GO_TERM_NAME', 'SOURCE',
                                              'EVIDENCE_CODE')]

    # remove duplicated entries resulting from alternative sources / envidence
    # codes
    result <- result[!duplicated(result),]

    # remove rows missing the ontology field (Bug in EuPathDB 33; affects only
    # a small number of entries)
    result <- result[!is.na(result$ONTOLOGY),]

    return(result)
}

#'
#' 2017/07/08: IN DEVELOPMENT
#'
#' Returns a mapping of gene ID to metabolic pathways (KEGG, LeishCyc, etc.)
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#'
#' @return Dataframe with gene/pathway mapping
#'
.get_pathway_table <- function(data_provider, organism) {
    # query body as a structured list
    query_body <- list(
        answerSpec=list(
            'questionName'=unbox("GeneQuestions.GenesByTaxonGene"),
            parameters=list(organism=unbox(organism)),
            viewFilters=list(),
            filters=list()
        ),
        formatting=list(
            formatConfig=list(
                tables="MetabolicPathways",
                includeHeader=unbox("true"),
                attachmentType=unbox("plain")
            ),
            format=unbox("tableTabular")
        )
    )

    # query EuPathDB
    res <- .post_eupathdb(data_provider, query_body)

    # parse response
    dat <- read.delim(textConnection(res), sep='\t')

    # if no pathway information is available, return an empty dataframe
    if (nrow(dat) == 0) {
        return(data.frame())
    }

    # drop empty column
    dat <- dat[,1:7]

    # simplify column names
    # > colnames(dat)                                                                                                                                                                                                     
    # [1] "X.Gene.ID."                        "X.pathway_source_id."                                                                                                                                                      
    # [3] "X.Pathway."                        "X.Pathway.Source."                                                                                                                                                         
    # [5] "X.EC.Number.Matched.in.Pathway."   "X.expasy_url."                                                                                                                                                             
    # [7] "X...Reactions.Matching.EC.Number."     
    colnames(dat) <- toupper(sub('_+$', '', sub('^X_+', '', gsub('\\.', '_', colnames(dat)))))
    colnames(dat)[1] <- 'GID'

    # drop unneeded columns
    dat <- dat[,c('GID', 'PATHWAY', 'PATHWAY_SOURCE_ID', 'PATHWAY_SOURCE')]

    # remove duplicated rows
    dat <- dat[!duplicated(dat),]

    dat
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
    result <- .retrieve_eupathdb_attributes(data_provider, organism, 'InterPro')

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
    result <- .retrieve_eupathdb_attributes(data_provider, organism, 'Orthologs')

    # fix column names and return result
    colnames(result) <- toupper(colnames(result)) 

    return(result)
}

###############################################################################
# MAIN
###############################################################################

# parse command-line arguments
args <- commandArgs(trailingOnly=TRUE)

# Create output directory if it doesn't already exist
output_dir <- args[1]

if (is.na(output_dir)) {
    stop("Missing argument specifying output directory to use...")
}

if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive=TRUE)
}

# load metadata
dat <- read.csv('../extdata/orgdb_metadata.csv')

# randomize order of entries to spread out the requests to multiple databases
dat <- dat[sample(1:nrow(dat)),]

# iterate over metadata entries and create OrgDb objects for each item
cl <- makeCluster(max(1, min(12, detectCores() - 4)), outfile="")

registerDoParallel(cl)

# packages needed during OrgDb construction
dependencies <- c('rtracklayer', 'AnnotationForge', 'GenomicFeatures',
                  'jsonlite', 'RSQLite', 'httr')

foreach(i=1:nrow(dat), .packages=dependencies, .verbose=TRUE) %dopar% {
    # re-initialize options
    options(stringsAsFactors=FALSE)

    # get metadata entry for a single organism
    entry <- dat[i,]

    # location to save orgdb to
    outfile <- file.path(output_dir, sub('.rda', '.sqlite', entry$ResourceName))

    # if sqlite database already exists, skip entry
    if (file.exists(outfile)) {
        message(sprintf("- Skipping %s... (EXISTS)", entry$Species))
        return
    } else {
        # create OrgDb object from metadata entry
        message(sprintf("- Building OrgDb for %s.", entry$Species))
        dbpath <- EuPathDBGFFtoOrgDb(entry, output_dir)

        # copy sqlite database to main output directory
        message(sprintf("- Finished building OrgDb for %s", entry$Species))
        file.copy(dbpath, outfile)
    }
    gc()
}

# unregister cpus
stopCluster(cl)

