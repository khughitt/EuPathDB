#' Generate a BSgenome package from the eupathdb.
#'
#' Since we go to the trouble to try and generate nice orgdb/txdb/organismdbi
#' packages, it seems to me that we ought to also be able to make a readable
#' genome package.  I should probably use some of the logic from this to make
#' the organismdbi generator smarter.
#'
#' @param entry Single eupathdb metadata entry.
<<<<<<< HEAD
#' @param eu_version Which version of the eupathdb to use for creating the BSGenome?
#' @param build_dir Working directory.
=======
#' @param eupathdb_version Which version of the eupathdb to use for creating the BSGenome?
#' @param workdir Working directory.
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @param installp Install the resulting package?
#' @param reinstall Rewrite an existing package directory.
#' @param ... Extra arguments for downloading metadata when not provided.
#' @return List of package names generated (only 1).
#' @author atb
#' @export
<<<<<<< HEAD
<<<<<<< HEAD:R/make_eupath_bsgenome.R
make_eupath_bsgenome <- function(entry, eu_version = NULL, build_dir = "EuPathDB", copy_s3 = FALSE,
                                 installp = TRUE, reinstall = FALSE, ...) {
=======
make_eupathdb_bsgenome <- function(entry, eu_version=NULL, workdir="EuPathDB", copy_s3=FALSE,
                                 installp=TRUE, reinstall=FALSE, ...) {
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/make_eupathdb_bsgenome.R
=======
make_eupathdb_bsgenome <- function(entry, eupathdb_version = NULL, workdir = "EuPathDB", copy_s3 = FALSE,
                                   installp = TRUE, reinstall = FALSE, ...) {
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  arglist <- list(...)
  author <- "Ashton Trey Belew <abelew@umd.edu>"
  if (!is.null(arglist[["author"]])) {
    author <- arglist[["author"]]
  }
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  versions <- get_versions(eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  taxa <- make_taxon_names(entry)
<<<<<<< HEAD
<<<<<<< HEAD:R/make_eupath_bsgenome.R
  pkgnames <- get_eupath_pkgnames(entry, eu_version = eu_version)
=======
  pkgnames <- get_eupathdb_pkgnames(entry, eu_version=eu_version)
>>>>>>> fd9c661 (Doing a bit of re-organizing):R/make_eupathdb_bsgenome.R
=======
  pkgnames <- get_eupathdb_pkgnames(entry, eupathdb_version = eupathdb_version)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  pkgname <- pkgnames[["bsgenome"]]
  if (pkgname %in% installed.packages() & !isTRUE(reinstall)) {
    message(" ", pkgname, " is already installed.")
    retlist <- list(
      "bsgenome_name" = pkgname
    )
    return(retlist)
  }

  ## Check that a directory exists to leave the final package
<<<<<<< HEAD
  build_dir <- file.path(build_dir)
  if (!file.exists(build_dir)) {
    tt <- dir.create(build_dir, recursive = TRUE)
=======
  workdir <- file.path(workdir)
  if (!file.exists(workdir)) {
    tt <- dir.create(workdir, recursive = TRUE)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  }
  ## Check for an incomplete installation directory and clear it out.
  if (file.exists(pkgname)) {
    final_deleted <- unlink(x = pkgname, recursive = TRUE, force = TRUE)
  }

  ## Figure out the version numbers and download urls.
  db_version <- entry[["SourceVersion"]]
<<<<<<< HEAD
  if (!is.null(eu_version)) {
    db_version <- gsub(x = eu_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")
=======
  if (!is.null(eupathdb_version)) {
    db_version <- gsub(x = eupathdb_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  }
  fasta_start <- entry[["SourceUrl"]]
  fasta_starturl <- sub(pattern = "gff",
                        replacement = "fasta",
                        x = fasta_start,
                        perl = TRUE)
  fasta_url <- sub(pattern = "\\.gff", replacement = "_Genome\\.fasta",
                   x = fasta_starturl)
  fasta_hostname <- sub(pattern = "https://(.*)\\.(org|net).*$",
                        replacement = "\\1",
                        x = fasta_start)
<<<<<<< HEAD
  ## genome_filename <- file.path(build_dir, paste0(pkgname, ".fasta"))
  genome_filename <- file.path(build_dir, glue::glue("{pkgname}.fasta"))
=======
  ## genome_filename <- file.path(workdir, paste0(pkgname, ".fasta"))
  genome_filename <- file.path(workdir, glue::glue("{pkgname}.fasta"))
>>>>>>> cc20d16 (Continuing clean-up / re-organization)

  ## Find a spot to dump the fasta files
  bsgenome_dir <- file.path(build_dir, pkgname)
  if (!file.exists(bsgenome_dir)) {
    created <- dir.create(bsgenome_dir, recursive = TRUE)
  }
  ## Download them to this directory.
<<<<<<< HEAD
  downloaded <- download.file(url=fasta_url, destfile=genome_filename, quiet=FALSE)
  ## Extract all the individual chromosomes into this directory.
=======
  downloaded <- download.file(url = fasta_url, destfile = genome_filename, quiet = FALSE)
  ## And extract all the individual chromosomes into this directory.
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
  input <- Biostrings::readDNAStringSet(genome_filename)
  output_list <- list()
  sequence_names <- "c("
  message(" Writing chromosome files, this is slow for fragmented scaffolds.")
  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style = 3)
  }
  for (index in 1:length(input)) {
    if (isTRUE(show_progress)) {
      pct_done <- index / length(input)
      setTxtProgressBar(bar, pct_done)
    }
    chr <- names(input)[index]
    chr_name <- strsplit(chr, split = " ")[[1]][1]
    ## chr_file <- file.path(bsgenome_dir, paste0(chr_name, ".fa"))
    chr_file <- file.path(bsgenome_dir, glue::glue("{chr_name}.fa"))
    output <- Biostrings::writeXStringSet(input[index], chr_file, append = FALSE,
                                          compress = FALSE, format = "fasta")
    output_list[[chr_name]] <- chr_file
    sequence_names <- paste0(sequence_names, '"', chr_name, '", ')
  }
  if (isTRUE(show_progress)) {
    close(bar)
  }
  sequence_names <- gsub(pattern = ", $", replacement = ")", x = sequence_names)

  ## Now start creating the DESCRIPTION file
  desc_file <- file.path(bsgenome_dir, "DESCRIPTION")
  descript <- desc::description$new("!new")
  descript$set(Package = pkgname)
  title <- glue::glue("{taxa[['genus']]} {taxa[['species']]} strain {taxa[['strain']]} \\
                version {db_version}")

  descript$set(Title = title)
  descript$set(Author = author)
  version_string <- format(Sys.time(), "%Y.%m")
  descript$set(Version = version_string)
  descript$set(Maintainer = author)
  descript$set(Description = glue::glue("A full genome from the eupathdb for {title}."))
  descript$set(License = "Artistic-2.0")
  descript$set(URL = "https://eupathdb.org")
  descript$set(BugReports = "https://github.com/elsayed-lab")
  descript$set(seqs_srcdir = bsgenome_dir)
  descript$set(seqnames = sequence_names)
  descript$set(organism = taxa[["taxon"]])
  descript$set(common_name = taxa[["genus_species"]])
  descript$set(provider = fasta_hostname)
  descript$set(provider_version = glue::glue("{fasta_hostname} {db_version}"))
  descript$set(release_date = format(Sys.time(), "%Y%m%d"))
  descript$set(BSgenomeObjname = glue::glue("{taxa[['genus_species']]}_{taxa[['strain']]}"))
  descript$set(release_name = as.character(db_version))
  descript$set(organism_biocview = glue::glue("{taxa[['genus_species']]}_{taxa[['strain']]}"))
  descript$del("LazyData")
  descript$del("Authors@R")
  descript$del("URL")
  descript$del("BugReports")
  descript$del("Encoding")
  description_file <- file.path(bsgenome_dir, "DESCRIPTION")
  descript$write(description_file)

  ## Generate the package, this puts it into the cwd.
  message(" Calling forgeBSgenomeDataPkg().")
  ## Otherwise I get error in cannot find uniqueLetters (this seems to be a new development)
  ## Invoking library(Biostrings") annoys R CMD check, but I am not sure there is a good
  ## way around that due to limitations of Biostrings, lets see.
  uniqueLetters <- Biostrings::uniqueLetters
  tt <- try(do.call("library", as.list("Biostrings")), silent = TRUE)
  annoying <- try(BSgenome::forgeBSgenomeDataPkg(description_file, verbose = FALSE))

  inst <- NULL
  if (isTRUE(installp)) {
    if (class(annoying) != "try-error") {
      inst <- try(devtools::install(pkgname, quiet = TRUE))
    }
  }

  if (isTRUE(copy_s3)) {
    source_dir <- basename(bsgenome_dir)
    s3_file <- entry[["BsgenomeFile"]]
    copied <- copy_s3_file(src_dir = source_dir, type = "bsgenome", s3_file = s3_file)
    if (isTRUE(copied)) {
      message(" Successfully copied the genome 2bit file to the s3 staging directory.")
    }
  }

  retlist <- list()
  if (class(inst) != "try-error" & !is.null(inst)) {
    retlist[["bsgenome_name"]] <- pkgname
    ## Clean up a little.
    deleted <- unlink(x = bsgenome_dir, recursive = TRUE, force = TRUE)
    built <- try(devtools::build(pkgname, quiet = TRUE))
    if (class(built) != "try-error") {
<<<<<<< HEAD
      final_path <- move_final_package(pkgname, type = "bsgenome", build_dir = build_dir)
=======
      final_path <- move_final_package(pkgname, type = "bsgenome", workdir = workdir)
>>>>>>> cc20d16 (Continuing clean-up / re-organization)
      final_deleted <- unlink(x = pkgname, recursive = TRUE, force = TRUE)
    }
  } else {
    retlist <- inst
  }
  message("Finished creation of ", pkgname, ".")
  return(retlist)
}
