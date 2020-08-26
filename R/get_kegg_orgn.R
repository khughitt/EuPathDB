#' Search KEGG identifiers for a given species name.
#'
#' KEGG identifiers do not always make sense.  For example, how am I supposed to
#' remember that Leishmania major is lmj?  This takes in a human readable string
#' and finds the KEGG identifiers that match it.
#'
#' @param species Search string (Something like 'Homo sapiens').
#' @param short Only pull the orgid?
#' @return Data frame of possible KEGG identifier codes, genome ID numbers,
#'  species, and phylogenetic classifications.
#' @seealso \pkg{RCurl}
#' @examples
#' \dontrun{
#'  fun = get_kegg_orgn('Canis')
#'  ## >     Tid     orgid      species                   phylogeny
#'  ## >  17 T01007   cfa Canis familiaris (dog) Eukaryotes;Animals;Vertebrates;Mammals
#' }
#' @export
get_kegg_orgn <- function(species = "Leishmania", short = TRUE) {
  all_organisms <- RCurl::getURL("http://rest.kegg.jp/list/organism")
  org_tsv <- textConnection(all_organisms)
  all <- read.table(org_tsv, sep = "\t", quote = "", fill = TRUE)
  colnames(all) <- c("Tid", "orgid", "species", "phylogeny")
  ## Look for the search string in the species column
  candidates <- all[grepl(species, all[["species"]]), ]
  if (isTRUE(short)) {
    candidates <- as.character(candidates[["orgid"]])
  }
  closed <- try(close(org_tsv), silent=TRUE)
  return(candidates)
}
