#' Get an orgdb from an AnnotationHub taxonID.
#'
#' Ideally, annotationhub will one day provide a one-stop shopping source for a
#' tremendous wealth of curated annotation databases, sort of like a
#' non-obnoxious biomart.  But for the moment, this function is more
#' fragile than I would like.
#'
#' @param ahid TaxonID from AnnotationHub
#' @param title Title for the annotation hub instance
#' @param species Species to download
#' @param type Datatype to download
#' @return An Orgdb instance
#' @seealso \pkg{AnnotationHub} \pkg{S4Vectors}
#' @examples
#' \dontrun{
#'  orgdbi <- mytaxIdToOrgDb(taxid)
#' }
#' @author atb
#' @export
orgdb_from_ah <- function(ahid = NULL, title = NULL, species = NULL, type = "OrgDb") {
  ## Other available types:
  tt <- loadNamespace("AnnotationHub")
  ah <- AnnotationHub::AnnotationHub()
  message("Available types: \n", toString(levels(as.factor(ah$rdataclass))))

  if (!is.null(type)) {
    ah <- AnnotationHub::query(x = ah, pattern = type)
  }
  if (is.null(title) & is.null(species) & is.null(ahid)) {
    message("Going to attempt to find a human database.  I hope this is what you want!")
    hits <- grepl(pattern = "Hs\\.eg\\.db", x = ah$title)
    ahid <- names(ah)[hits]
  } else if (is.null(ahid) & is.null(title) & is.null(species)) {
    ## Then we got a species
    possible <- ah$species
    titles <- ah$title
    hits_idx <- grepl(pattern = species, x = possible)
    first_true <- which.max(hits_idx)
    first_true_name <- titles[first_true]
    hits <- names(ah)[hits_idx]
    message("The possible hits are: \n",
            toString(hits), "\nchoosing: ", hits[1],
            "\nwhich is ", first_true_name)
    ahid <- hits[1]
  } else if (is.null(ahid) & is.null(species)) {
    ## We got a title
    possible <- ah$title
    hits_idx <- grepl(pattern = title, x = possible)
    first_true <- which.max(hits_idx)
    first_true_name <- possible[first_true]
    hits <- names(ah)[hits_idx]
    message("The possible hits are: \n",
            toString(hits), "\nchoosing: ", hits[1],
            "\nwhich is ", first_true_name)
    ahid <- hits[1]
  }

  ah_names <- names(ah)
  ah_titles <- ah$title
  hit_idx <- ah_names == ahid
  hit_num <- which.max(hit_idx)
  hit_title <- ah_titles[hit_num]
  message("Chose ", ahid, " which is ", hit_title, ".")
  res <- ah[[ahid]]
  return(res)
}
