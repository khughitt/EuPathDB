#' Move a package file to its final location for collation by AnnotationHubData.
move_final_package <- function(pkgname, type="orgdb", dir="EuPathDB") {
  final_dir <- file.path(dir, type)
  if (!file.exists(final_dir)) {
    dir.create(final_dir, recursive=TRUE)
  }
  pkgname <- basename(pkgname)
  version_string <- format(Sys.time(), "%Y.%m")
  final_filename <- glue::glue("{pkgname}_{version_string}.tar.gz")
  current_path <- file.path(dir, final_filename)
  final_path <- file.path(final_dir, final_filename)
  if (type == "granges") {
    current_path <- file.path(dir, pkgname)
    final_path <- file.path(final_dir, pkgname)
  }
  moved <- file.rename(from=current_path, to=final_path)
  return(final_path)
}
