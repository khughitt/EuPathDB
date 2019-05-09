prefix_map <- function(prefix) {
  prefix_mapping <- list(
    "amoebadb" = "amoeba",
    "microbiomedb" = "mbio",
    "microsporidiadb" = "micro",
    "piroplasmadb" = "piro",
    "plasmodb" = "plasmo",
    "schistodb" = "schisto",
    "toxodb" = "toxo"
  )
  new_prefix <- prefix
  if (prefix %in% names(prefix_mapping)) {
    new_prefix <- prefix_mapping[[prefix]]
  }
  return(new_prefix)
}
