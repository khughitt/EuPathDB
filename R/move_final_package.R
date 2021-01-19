#' Move a package file to its final location for collation by AnnotationHubData.
#'
#' @param pkglist Information list from a make_pkg function.
#' @param type Which type of package is this?
#' @param build_dir base working directory.
move_final_package <- function(pkglist, type = "orgdb", build_dir = "EuPathDB") {
  final_dir <- file.path(build_dir, "tar")
  if (!file.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE)
  }
  pkgname <- basename(as.character(pkgname))
  version_string <- format(Sys.time(), "%Y.%m")
  final_filename <- glue::glue("{pkgname}_{version_string}.tar.gz")
  current_path <- file.path(build_dir, final_filename)
  final_path <- file.path(final_dir, final_filename)
  if (type == "granges") {
    current_path <- file.path(build_dir, pkgname)
    final_path <- file.path(final_dir, pkgname)
  } else if (type == "txdb") {
    gff_file <- file.path(build_dir, glue::glue("{pkgname}.gff"))
    gff_dir <- file.path(build_dir, "gff")
    if (!file.exists(gff_dir)) {
      dir.create(gff_dir, recursive = TRUE)
    }
    gff_moved <- file.rename(from = gff_file, file.path(gff_dir, basename(gff_file)))
  } else if (type == "bsgenome") {
    current_path <- basename(final_path)
    fasta_file <- file.path(build_dir, glue::glue("{pkgname}.fasta"))
    fasta_dir <- file.path(build_dir, "fasta")
    if (!file.exists(fasta_dir)) {
      dir.create(fasta_dir, recursive = TRUE)
    }
    fasta_moved <- file.rename(from = fasta_file, file.path(fasta_dir, basename(fasta_file)))
  } else if (type == "organismdbi") {
    current_path <- basename(final_path)
    fasta_file <- file.path(build_dir, glue::glue("{pkgname}.fasta"))
    fasta_dir <- file.path(build_dir, "fasta")
    if (!file.exists(fasta_dir)) {
      dir.create(fasta_dir, recursive = TRUE)
    }
    fasta_moved <- try(
        file.rename(from = fasta_file, file.path(fasta_dir, basename(fasta_file))), silent = TRUE)
  }
  moved <- file.rename(from = current_path, to = final_path)
  return(final_path)
}
