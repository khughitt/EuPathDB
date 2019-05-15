#' Generate a BSgenome package from the eupathdb.
#'
#' Since we go to the trouble to try and generate nice orgdb/txdb/organismdbi
#' packages, it seems to me that we ought to also be able to make a readable
#' genome package.  I should probably use some of the logic from this to make
#' the organismdbi generator smarter.
#'
#' @param entry  Single eupathdb metadata entry.
#' @param version Which version of the eupathdb to use for creating the BSGenome?
#' @param dir  Working directory.
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @param reinstall  Rewrite an existing package directory.
#' @param ... Extra arguments for downloading metadata when not provided.
#' @return  List of package names generated (only 1).
#' @author atb
#' @export
make_eupath_bsgenome <- function(entry, version=NULL, dir="EuPathDB", copy_s3=FALSE,
                                 reinstall=FALSE, ...) {
  arglist <- list(...)
  author <- "Ashton Trey Belew <abelew@umd.edu>"
  if (!is.null(arglist[["author"]])) {
    author <- arglist[["author"]]
  }
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, version=version)
  pkgname <- pkgnames[["bsgenome"]]
  if (pkgname %in% installed.packages() & !isTRUE(reinstall)) {
    message(pkgname, " is already installed, set reinstall=TRUE if you wish to reinstall.")
    retlist <- list(
      "bsgenome_name" = pkgname
    )
    return(retlist)
  }

  ## Check that a directory exists to leave the final package
  dir <- file.path(dir)
  if (!file.exists(dir)) {
    tt <- dir.create(dir, recursive=TRUE)
  }
  ## Check for an incomplete installation directory and clear it out.
  if (file.exists(pkgname)) {
    final_deleted <- unlink(x=pkgname, recursive=TRUE, force=TRUE)
  }

  ## Figure out the version numbers and download urls.
  db_version <- entry[["SourceVersion"]]
  if (!is.null(version)) {
    db_version <- version
  }
  fasta_start <- entry[["SourceUrl"]]
  fasta_starturl <- sub(pattern="gff",
                        replacement="fasta",
                        x=fasta_start,
                        perl=TRUE)
  fasta_url <- sub(pattern="\\.gff", replacement="_Genome\\.fasta",
                   x=fasta_starturl)
  fasta_hostname <- sub(pattern="https://(.*)\\.org.*$",
                        replacement="\\1",
                        x=fasta_start)
  ## genome_filename <- file.path(dir, paste0(pkgname, ".fasta"))
  genome_filename <- file.path(dir, glue::glue("{pkgname}.fasta"))

  ## Find a spot to dump the fasta files
  bsgenome_dir <- file.path(dir, pkgname)
  if (!file.exists(bsgenome_dir)) {
    created <- dir.create(bsgenome_dir, recursive=TRUE)
  }
  ## Download them to this directory.
  downloaded <- download.file(url=fasta_url, destfile=genome_filename, quiet=FALSE)
  ## And extract all the individual chromosomes into this directory.
  input <- Biostrings::readDNAStringSet(genome_filename)
  output_list <- list()
  sequence_names <- "c("
  message("Writing chromosome files, this is slow for fragmented scaffolds.")
  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  if (isTRUE(show_progress)) {
    bar <- utils::txtProgressBar(style=3)
  }
  for (index in 1:length(input)) {
    if (isTRUE(show_progress)) {
      pct_done <- index / length(input)
      setTxtProgressBar(bar, pct_done)
    }
    chr <- names(input)[index]
    chr_name <- strsplit(chr, split=" ")[[1]][1]
    ## chr_file <- file.path(bsgenome_dir, paste0(chr_name, ".fa"))
    chr_file <- file.path(bsgenome_dir, glue::glue("{chr_name}.fa"))
    output <- Biostrings::writeXStringSet(input[index], chr_file, append=FALSE,
                                          compress=FALSE, format="fasta")
    output_list[[chr_name]] <- chr_file
    sequence_names <- paste0(sequence_names, '"', chr_name, '", ')
  }
  if (isTRUE(show_progress)) {
    close(bar)
  }
  sequence_names <- gsub(pattern=", $", replacement=")", x=sequence_names)

  ## Now start creating the DESCRIPTION file
  desc_file <- file.path(bsgenome_dir, "DESCRIPTION")
  descript <- desc::description$new("!new")
  descript$set(Package=pkgname)
  title <- glue::glue("{taxa[['genus']]} {taxa[['species']]} strain {taxa[['strain']]} \\
                version {db_version}")

  descript$set(Title=title)
  descript$set(Author=author)
  version_string <- format(Sys.time(), "%Y.%m")
  descript$set(Version=version_string)
  descript$set(Maintainer=author)
  descript$set(Description=glue::glue("A full genome from the eupathdb for {title}."))
  descript$set(License="Artistic-2.0")
  descript$set(URL="https://eupathdb.org")
  descript$set(BugReports="https://github.com/elsayed-lab")
  descript$set(seqs_srcdir=bsgenome_dir)
  descript$set(seqnames=sequence_names)
  descript$set(organism=taxa[["taxon"]])
  descript$set(common_name=taxa[["genus_species"]])
  descript$set(provider=fasta_hostname)
  descript$set(provider_version=glue::glue("{fasta_hostname} {db_version}"))
  descript$set(release_date=format(Sys.time(), "%Y%m%d"))
  descript$set(BSgenomeObjname=glue::glue("{taxa[['genus_species']]}_{taxa[['strain']]}"))
  descript$set(release_name=as.character(db_version))
  descript$set(organism_biocview=glue::glue("{taxa[['genus_species']]}_{taxa[['strain']]}"))
  descript$del("LazyData")
  descript$del("Authors@R")
  descript$del("URL")
  descript$del("BugReports")
  descript$del("Encoding")
  description_file <- file.path(bsgenome_dir, "DESCRIPTION")
  descript$write(description_file)

  ## Generate the package, this puts it into the cwd.
  message("Starting forgeBSgenomeDataPkg().")
  tt <- requireNamespace("Biostrings")
  ## Otherwise I get error in cannot find uniqueLetters (this seems to be a new development)
  ## Invoking library(Biostrings") annoys R CMD check, but I am not sure there is a good
  ## way around that due to limitations of Biostrings, lets see.
  tt <- try(attachNamespace("Biostrings"), silent=TRUE)
  annoying <- try(BSgenome::forgeBSgenomeDataPkg(description_file, verbose=FALSE))

  inst <- NULL
  if (class(annoying) != "try-error") {
    inst <- try(devtools::install(pkgname, quiet=TRUE))
  }

  if (isTRUE(copy_s3)) {
    source_dir <- basename(bsgenome_dir)
    s3_file <- entry[["BsgenomeFile"]]
    copied <- copy_s3_file(src_dir=source_dir, type="bsgenome", s3_file=s3_file)
    if (isTRUE(copied)) {
      message("Successfully copied the genome 2bit file to the s3 staging directory.")
    }
  }

  retlist <- list()
  if (class(inst) != "try-error" & !is.null(inst)) {
    retlist[["bsgenome_name"]] <- pkgname
    ## Clean up a little.
    deleted <- unlink(x=bsgenome_dir, recursive=TRUE, force=TRUE)
    built <- try(devtools::build(pkgname, quiet=TRUE))
    if (class(built) != "try-error") {
      final_path <- move_final_package(bsgenome_dir, type="bsgenome", dir=dir)
      final_deleted <- unlink(x=pkgname, recursive=TRUE, force=TRUE)
    }
  } else {
    retlist <- inst
  }

  return(retlist)
}
