###############################################################################
# Shared functions for parsing EuPathDB resources
# Keith Hughitt (khughitt@umd.edu)
# Sept 01, 2016
###############################################################################

#'
#' Queries one of the EuPathDB APIs for gene data
#'
#' Note that as of version 30, EuPathDB no longer supports table queries for
#' genes, and as such this method must be used instead. Support for tables
#' queries is likely to be returned in future versions of EuPathDB.
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#' @param table_name Name of the particular table to be retrieved (e.g.
#' 'GoTerms')
#' @param wadl String specifying API service to be queried
#' @param format String specifying API response type (currently only 'json'
#'        is supported)
#' @return list containing response from API request.
.retrieve_eupathdb_attributes <- function(data_provider, organism, table_name,
                                          wadl='GeneQuestions/GenesByTaxonGene',
                                          format='json') {
    # query EuPathDB API
    res <- .query_eupathdb(data_provider, organism, sprintf('o-tables=%s', table_name), wadl)
    dat <- res$response$recordset$records

    message(sprintf("- Parsing %s table for %s.", table_name, organism))

    # drop genes with no associated table entries
    gene_mask <- sapply(dat[,'tables'], function(x) { length(x$rows[[1]]) > 0})
    dat <- dat[gene_mask,]

    # create empty data frame to store result in
    result <- data.frame(stringsAsFactors=FALSE)

    # if no rows found, return empty data.frame
    if (nrow(dat) == 0) {
        return(result)
    }

    message(sprintf("- Parsing %d rows in %s table for %s.", nrow(dat), table_name, organism))

    # iterate over remaining genes and extract table entries for them
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
        table_entries <- dat$tables[[i]]
        rows <- t(sapply(table_entries$rows[[1]]$fields, function(x) { x$value }))
        result <- rbind(result, cbind(dat$id[i], rows))

        if (i %% 1000 == 0) {
            message(sprintf(" - Parsing row %d/%d in %s table for %s.", 
                            i, nrow(dat), table_name, organism))
        }
    }

    # set column names for result
    colnames(result) <- c('GID', dat$tables[[1]]$rows[[1]]$fields[[1]]$name)
    return(result)
}


#'
#' Queries one of the EuPathDB APIs for table data.
#'
#' @param data_provider Name of data provider to query (e.g. 'TriTrypDB')
#' @param organism Full name of organism, as used by EuPathDB APIs
#' @param table_name Name of the particular table to be retrieved (e.g.
#' 'GoTerms')
#' @param wadl String specifying API service to be queried
#' @param format String specifying API response type (currently only 'json'
#'        is supported)
#' @return list containing response from API request.
.retrieve_eupathdb_table <- function(data_provider, organism, table_name,
                                     wadl='GeneQuestions/GenesByTaxon',
                                     format='json') {
    # query EuPathDB API
    res <- .query_eupathdb(data_provider, organism, sprintf('o-tables=%s', table_name), wadl)
    dat <- res$response$recordset$records

    message(sprintf("- Parsing %s table for %s.", table_name, organism))

    # drop genes with no associated table entries
    gene_mask <- sapply(dat[,'tables'], function(x) { length(x$rows[[1]]) > 0})
    dat <- dat[gene_mask,]

    # create empty data frame to store result in
    result <- data.frame(stringsAsFactors=FALSE)

    # if no GO terms found, return empty data.frame
    if (nrow(dat) == 0) {
        return(result)
    }

    # iterate over remaining genes and extract table entries for them
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
        table_entries <- dat$tables[[i]]
        rows <- t(sapply(table_entries$rows[[1]]$fields, function(x) { x$value }))
        result <- rbind(result, cbind(dat$id[i], rows))
    }

    # set column names for result
    colnames(result) <- c('GID', dat$tables[[1]]$rows[[1]]$fields[[1]]$name)

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
.query_eupathdb <- function(data_provider, organism, query_args,
                            wadl='GeneQuestions/GenesByTaxon', format='json') {
    # construct API query
    base_url <- sprintf('http://%s.org/webservices/%s.%s?', 
                        tolower(data_provider), wadl, format)
    query_string <- sprintf('organism=%s&%s', 
                            URLencode(organism, reserved=TRUE), query_args)
    request_url <- paste0(base_url, query_string)

    if (nchar(request_url) > 200) {
        log_url <- paste0(strtrim(request_url, 160), '...')
    } else {
        log_url <- request_url
    }
    message(sprintf("- Querying %s", log_url))

    # query API for gene types
    if (format == 'json') {
        fromJSON(request_url)
    } else {
        stop("Invalid response type specified.")
    }
}

#'
#' Parses a EuPathDB organism txt file and retrieve table information for
#' all available genes.
#'
#' Note: currently this method is not being used since it still performs very
#' slowly compared with using API queries.
#'
#' @param table_name Name of the particular table to be retrieved (e.g.
#' 'GoTerms')
#' @return data frame containing table results
.parse_eupathdb_txt_table <- function(filepath, table_name) {
    # open txt file
    if (endsWith(filepath, '.gz')) {
        fp <- gzfile(filepath)
        open(fp)
    } else {
        fp <- file(filepath, open='r')
    }

    # create empty data frame to store result in
    result <- data.frame(stringsAsFactors=FALSE)

    # iterate over lines of file
    while (length(aline <- readLines(fp, n=1, warn=FALSE)) > 0) {
        # Gene ID
        if(grepl("^Gene ID", aline)) {
            gene_id <- .get_value(aline)
        }

        # Parse table entries
        else if (grepl(sprintf("^TABLE: %s", table_name), aline)) {
            aline <- readLines(fp, n=1, warn=FALSE)
            table_text <- aline

            # read in table, one line at a time
            while (length(aline) != 0) {
                aline <- readLines(fp, n=1, warn=FALSE)
                table_text <- paste(c(table_text, aline), sep='\n')
            }

            # read into a data frame and fix column names
            dat <- read.delim(text=table_text)
            colnames(dat) <- substr(colnames(dat), 3, nchar(colnames(dat)) - 1)

            # append to multigene result dataframe
            if (nrow(dat) > 0) {
                result <- rbind(result, cbind(GID=gene_id, dat))
            }
        }
    }
    return(result)
}

#
# Parses a key: value string and returns the value
#
.get_value = function(x) {
    return(gsub(" ","", tail(unlist(strsplit(x, ': ')), n=1), fixed=TRUE))
}
