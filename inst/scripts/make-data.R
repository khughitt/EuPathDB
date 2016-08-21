#'
#' Functions for parsing EuPathDb resources
#' 
#' Author: Keith Hughitt (khughitt@umd.edu)
#' Last Update: Aug 20, 2016
#'
#' Questions:
#' 
library('rtracklayer')

#'
#' Generate TxDb for EuPathDB organism
#'
EuPathDBGFFtoTxDb <- function(ahm) {
    # save gff as tempfile
    input_gff <- tempfile()
    download.file(ahm@SourceUrl, input_gff)

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
