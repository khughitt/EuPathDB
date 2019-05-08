#' Iterate through the various ways of representing taxon names
#'
#' Spend some time making sure they are valid, too.  Thus we want to get rid of
#' weird characters like hash marks, pipes, etc.
#'
#' @param entry  An entry of the eupathdb metadata.
#' @return  A list of hopefully valid nomenclature names to be used elsewhere in
#'   this family.
#' @author atb
make_taxon_names <- function(entry, column="TaxonUnmodified") {
  taxon <- entry[[column]]
  unmodified <- taxon
  species_parts <- unlist(strsplit(taxon, " "))
  genus <- species_parts[1]
  first <- toupper(substring(genus, 1, 1))
  species <- species_parts[2]
  strain <- ""
  if (length(species_parts) >= 3) {
    for (part in 3:length(species_parts)) {
      strain <- glue::glue("{strain}.{species_parts[part]}")
    }
  }
  strain <- gsub(pattern="^\\.", replacement="", x=strain)

  ## The following decisions, whether to remove a character or replace it with a '.' are
  ## mostly arbitrary ones depending pretty much entirely on how annoyed I get when I see
  ## a given special character in a species/strain.

  ## Start by getting rid of annoying text
  strain_pattern <- "(_| |\\.)strain(_| |\\.)"
  taxon <- gsub(pattern=strain_pattern, replacement="\\1", x=taxon)
  genus <- gsub(pattern=strain_pattern, replacement="\\1", x=genus)
  species <- gsub(pattern=strain_pattern, replacement="\\1", x=species)
  strain <- gsub(pattern=strain_pattern, replacement="\\1", x=strain)

  ## Replace some annoying characters with a '.'
  annoying_pattern <- "(_|-|#| )"
  taxon <- gsub(pattern=annoying_pattern, replacement="\\.", x=taxon)
  genus <- gsub(pattern=annoying_pattern, replacement="\\.", x=genus)
  species <- gsub(pattern=annoying_pattern, replacement="\\.", x=species)
  strain <- gsub(pattern=annoying_pattern, replacement="\\.", x=strain)

  ## Completely remove the truly stupid characters.
  stupid_pattern <- "(\\,|\\/|\\?|\\|\\[|\\]|\\(|\\)|\\:)"
  taxon <- gsub(pattern=stupid_pattern, replacement="", x=taxon)
  genus <- gsub(pattern=stupid_pattern, replacement="", x=genus)
  species <- gsub(pattern=stupid_pattern, replacement="", x=species)
  strain <- gsub(pattern=stupid_pattern, replacement="", x=strain)

  ## There are a few extra-weirdos with double-.s
  silly_pattern <- "\\.\\."
  taxon <- gsub(pattern=silly_pattern, replacement="\\.", x=taxon)
  genus <- gsub(pattern=silly_pattern, replacement="\\.", x=genus)
  species <- gsub(pattern=silly_pattern, replacement="\\.", x=species)
  strain <- gsub(pattern=silly_pattern, replacement="\\.", x=strain)

  species_strain <- glue::glue("{species}.{strain}")
  genus_species <- glue::glue("{genus}.{species}")

  species_strain <- paste(unlist(strsplit(taxon, split="\\."))[-1], collapse=".")
  genus_species <- glue::glue("{genus}.{species}")

  gspecies <- glue::glue("{first}{species}")
  gsstrain <- glue::glue("{gspecies}{strain}")

  ## I need a final removal of .. in case there is no strain or somesuch.
  species_strain <- gsub(pattern=silly_pattern, replacement="\\.", x=species_strain)
  genus_species <- gsub(pattern=silly_pattern, replacement="\\.", x=genus_species)
  gspecies <- gsub(pattern=silly_pattern, replacement="\\.", x=gspecies)
  gsstrain <- gsub(pattern=silly_pattern, replacement="\\.", x=gsstrain)

  taxa <- list(
    "unmodified" = unmodified,
    "taxon" = taxon,
    "genus" = genus,
    "species" = species,
    "strain" = strain,
    "species_strain" = species_strain,
    "genus_species" = genus_species,
    "gspecies" = gspecies,
    "gsstrain" = gsstrain)
  return(taxa)
}
