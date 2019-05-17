#' Move a package file to its final location for collation by AnnotationHubData.
#'
#' @param pkgname Name of package to move to its final home.
#' @param type Which type of package is this?
#' @param dir base working directory.
move_final_package <- function(pkgname, type="orgdb", dir="EuPathDB") {
  final_dir <- file.path(dir, "tar")
  if (!file.exists(final_dir)) {
    dir.create(final_dir, recursive=TRUE)
  }
  pkgname <- basename(as.character(pkgname))
  version_string <- format(Sys.time(), "%Y.%m")
  final_filename <- glue::glue("{pkgname}_{version_string}.tar.gz")
  current_path <- file.path(dir, final_filename)
  final_path <- file.path(final_dir, final_filename)
  if (type == "granges") {
    current_path <- file.path(dir, pkgname)
    final_path <- file.path(final_dir, pkgname)
  } else if (type == "txdb") {
    gff_file <- file.path(dir, glue::glue("{pkgname}.gff"))
    gff_dir <- file.path(dir, "gff")
    if (!file.exists(gff_dir)) {
      dir.create(gff_dir, recursive=TRUE)
    }
    gff_moved <- file.rename(from=gff_file, file.path(gff_dir, basename(gff_file)))
  } else if (type == "bsgenome") {
    current_path <- basename(final_path)
    fasta_file <- file.path(dir, glue::glue("{pkgname}.fasta"))
    fasta_dir <- file.path(dir, "fasta")
    if (!file.exists(fasta_dir)) {
      dir.create(fasta_dir, recursive=TRUE)
    }
    fasta_moved <- file.rename(from=fasta_file, file.path(fasta_dir, basename(fasta_file)))
  } else if (type == "organismdbi") {
    current_path <- basename(final_path)
    fasta_file <- file.path(dir, glue::glue("{pkgname}.fasta"))
    fasta_dir <- file.path(dir, "fasta")
    if (!file.exists(fasta_dir)) {
      dir.create(fasta_dir, recursive=TRUE)
    }
    fasta_moved <- try(
      file.rename(from=fasta_file, file.path(fasta_dir, basename(fasta_file))), silent=TRUE)
  }
  moved <- file.rename(from=current_path, to=final_path)
  return(final_path)
}
