#!/usr/bin/env Rscript
###############################################################################
#
# Functions for creating OrgDb objects from EuPathDB resources
# 
# Author: Keith Hughitt (keith.hughitt@nih.gov)
#
# Usage: ./make-granges-data.R /path/to/eupathdb/granges/32
#
###############################################################################
options(stringsAsFactors = FALSE)
library('rtracklayer')

#'
#' Generate GRanges for EuPathDB organism
#'
#' @param entry One dimensional dataframe with organism metadata
#' @return GRanges instance
#' 
EuPathDBGFFtoGRanges <- function(entry) {
    # save gff as tempfile
    input_gff <- tempfile(fileext = '.gff')

    message(sprintf("- Generating GRanges object for %s", entry$Organism))

    # attempt to download file
    res <- tryCatch({
        download.file(entry$SourceUrl, input_gff)
    }, error = function(e) {
        return(404)
    })

    # stop here if file not successfully downloaded
    if (res != 0) {
        warning("Unable to download annotations for %s; skipping...",
                entry$Organism)
        return(NA)
    }

    # convert to GRanges with rtracklayer and return 
    import.gff3(input_gff)
}

###############################################################################
# MAIN
###############################################################################

# parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Create output directory if it doesn't already exist
output_dir <- args[1]

if (is.na(output_dir)) {
    stop("Missing argument specifying output directory to use...")
}

if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}

# load metadata
dat <- read.csv('../extdata/granges_metadata.csv', stringsAsFactors = FALSE)

# iterate over metadata entries and create GRanges objects for each item
for (i in 1:nrow(dat)) {
    # check to see if output already exists
    entry <- dat[i, ]
    outfile <- file.path(output_dir, entry$ResourceName)

    if (file.exists(outfile)) {
        message(sprintf("- Skipping %s (EXISTS)", entry$Organism))
        return
    } else {
        # create GRanges object from metadata entry
        gr <- EuPathDBGFFtoGRanges(entry)

        # save to file
        message(sprintf("Saving GRanges object to %s", outfile))
        save(gr, file = outfile)
    }
}

