#' Generate a BSgenome package from the eupathdb.
#'
#' Since we go to the trouble to try and generate nice orgdb/txdb/organismdbi
#' packages, it seems to me that we ought to also be able to make a readable
#' genome package.  I should probably use some of the logic from this to make
#' the organismdbi generator smarter.
#'
#' @param entry Single eupathdb metadata entry.
#' @param eu_version Which version of the eupathdb to use for creating the BSGenome?
#' @param build_dir Working directory.
#' @param copy_s3 Copy the 2bit file into an s3 staging directory for copying to AnnotationHub?
#' @param install Install the resulting package?
#' @param reinstall Rewrite an existing package directory.
#' @param ... Extra arguments for downloading metadata when not provided.
#' @return List of package names generated (only 1).
#' @author atb
#' @export
make_eupath_bsgenome <- function(entry, eu_version = NULL, build_dir = "build",
                                 copy_s3 = FALSE, install = TRUE, reinstall = FALSE,
                                 author = NULL, verbose = FALSE, build = TRUE) {
  if (is.null(author)) {
    author <- "Ashton Trey Belew <abelew@umd.edu>"
  }
  if (is.null(entry)) {
    stop("Need an entry.")
  }
  versions <- get_versions(eu_version = eu_version)
  eu_version <- versions[["eu_version"]]
  taxa <- make_taxon_names(entry)
  pkgnames <- get_eupath_pkgnames(entry, eu_version = eu_version)
  pkgname <- pkgnames[["bsgenome"]]
  if (pkgname %in% installed.packages() & !isTRUE(reinstall)) {
    message(" ", pkgname, " is already installed.")
    retlist <- list(
      "bsgenome_name" = pkgname)
    return(retlist)
  }

  ## Check that a directory exists to leave the final package
  build_dir <- file.path(build_dir)
  if (!file.exists(build_dir)) {
    tt <- dir.create(build_dir, recursive = TRUE)
  }
  ## Check for an incomplete installation directory and clear it out.
  if (file.exists(pkgname)) {
    final_deleted <- unlink(x = pkgname, recursive = TRUE, force = TRUE)
  }

  ## Figure out the version numbers and download urls.
  db_version <- entry[["SourceVersion"]]
  if (!is.null(eu_version)) {
    db_version <- gsub(x = eu_version, pattern = "^(\\d)(.*)$", replacement = "v\\1\\2")
  }
  fasta_start <- entry[["URLGenome"]]
  ## One might reasonably look at the next line and think 'wtf'.  Some metadata shows incorrect
  ## protocols and/or incorrect TLDs for the provided download links.  E.g. they report http when
  ## only https works and/or say the download link is at zzz.net when it is actually zzz.org...
  ## I wrote the eupathdb folks about this a long time ago, this may therefore not be needed anymore.
  fasta_hostname <- sub(pattern = "(https|http)://(.*)\\.(org|net).*$",
                         replacement = "\\2",
                         x = fasta_start)
  ## genome_filename <- file.path(build_dir, paste0(pkgname, ".fasta"))
  download_dir <- file.path(build_dir, "fasta")
  if (!file.exists(download_dir)) {
    created <- dir.create(download_dir, recursive = TRUE)
  }
  genome_filename <- file.path(download_dir, glue::glue("{pkgname}.fasta"))

  ## Find a spot to dump the fasta files
  bsgenome_dir <- file.path(build_dir, pkgname)
  if (!file.exists(bsgenome_dir)) {
    created <- dir.create(bsgenome_dir, recursive = TRUE)
  }
  ## Download them to this directory.
  if (file.exists(genome_filename)) {
    message("The fasta file was already downloaded.")
  } else {
    downloaded <- download.file(url = fasta_start, destfile = genome_filename,
                                quiet = FALSE)
  }
  ## Extract all the individual chromosomes into this directory.
  input <- Biostrings::readDNAStringSet(genome_filename)
  output_list <- list()
  sequence_names <- "c("
  message(" Writing chromosome files, this is slow for fragmented scaffolds.")
  show_progress <- interactive() && is.null(getOption("knitr.in.progress"))
  #if (isTRUE(show_progress)) {
  #  bar <- utils::txtProgressBar(style = 3)
  #}
  genome_prefix <- NULL
  for (index in seq_len(length(input))) {
   # if (isTRUE(show_progress)) {
   #   pct_done <- index / length(input)
   #   setTxtProgressBar(bar, pct_done)
   # }
    chr <- names(input)[index]
    chr_name <- strsplit(chr, split = " ")[[1]][1]
    if (is.null(genome_prefix)) {
      genome_prefix <- gsub(x = chr_name, pattern = "(.*)\\.(.*)", replacement = "\\1")
    }
    ## chr_file <- file.path(bsgenome_dir, paste0(chr_name, ".fa"))
    chr_file <- file.path(bsgenome_dir, glue::glue("{chr_name}.fa"))
    output <- Biostrings::writeXStringSet(input[index], chr_file, append = FALSE,
                                          compress = FALSE, format = "fasta")
    output_list[[chr_name]] <- chr_file
    sequence_names <- paste0(sequence_names, '"', chr_name, '", ')
  }
  #if (isTRUE(show_progress)) {
  #  close(bar)
  #}
  message("Finished writing ", length(input), " contigs.")
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
  descript$set(release_date = format(Sys.time(), "%Y%m%d"))
  descript$set(BSgenomeObjname = glue::glue("{taxa[['genus_species']]}_{taxa[['strain']]}"))
  ## descript$set(provider_version = glue::glue("{fasta_hostname} {db_version}"))
  ## descript$set(release_name = as.character(db_version))
  ## descript$set(release_name = glue::glue("{fasta_hostname} {db_version}"))
  ## descript$set(genome = genome_prefix)
  descript$set(genome = "hg38")  ## There is some weird BS with this field, so I am faking it.
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
  pkg_builder <- sm(BSgenome::forgeBSgenomeDataPkg(description_file), wrap = TRUE)
  if ("try-error" %in% class(pkg_builder)) {
    message("forgeBSgenomeDataPkg failed with error: ")
    message("A likely reason is too many open files, which may be changed in /etc/sysctl.conf")
    print(pkg_builder)
    pkg_builder <- NULL
    return(NULL)
  }

  built <- NULL
  workedp <- ! "try-error" %in% class(pkg_builder)
  if (isTRUE(workedp)) {
    built <- try(devtools::build(pkgname, quiet = TRUE))
    workedp <- ! "try-error" %in% class(built)
  }

  if (isTRUE(workedp)) {
    if (isTRUE(install)) {
      inst <- try(devtools::install(pkgname, quiet = TRUE))
    }
    if (isTRUE(copy_s3)) {
      source_dir <- basename(bsgenome_dir)
      s3_file <- entry[["BsgenomeFile"]]
      copied <- copy_s3_file(src_dir = source_dir, type = "bsgenome", s3_file = s3_file)
      if (isTRUE(copied)) {
        message(" Successfully copied the genome 2bit file to the s3 staging directory.")
      }
    }

    pkg_archive_dir <- file.path(build_dir, "packages", "bsgenome")
    if (!file.exists(pkg_archive_dir)) {
      tt <- dir.create(pkg_archive_dir, recursive = TRUE)
    }

    ## If everything worked and we did whatever it is we think is appropriate with the results,
    ## then it should be safe to clean up the intermediates.
    message("Cleaning up the bsgenome staging directory: ", bsgenome_dir, ".")
    if (file.exists(bsgenome_dir)) {
      deleted <- unlink(x = bsgenome_dir, recursive = TRUE, force = TRUE)
    }
    weirdo <- basename(bsgenome_dir)
    if (file.exists(weirdo)) {
      deleted <- unlink(x = weirdo, recursive = TRUE, force = TRUE)
    }
    downloaded_fasta <- paste0(weirdo, ".fasta")
    deleted <- unlink(x = downloaded_fasta, force = TRUE)
    build_fasta <- file.path("build", downloaded_fasta)
    deleted <- unlink(x = build_fasta, force = TRUE)

    if (!is.null(built)) {
      message("Moving the tarball to the archive directory.")
      destination <- file.path(pkg_archive_dir, basename(built))
      tt <- file.rename(built, destination)
    }

    if (file.exists(pkgname)) {
      message("Cleaning up the forgeBSGenomeDataPkg directory.")
      deleted <- unlink(x = pkgname, recursive = TRUE, force = TRUE)
    }
  }

  retlist <- list(
    "bsgenome_name" = pkgname,
    "pkg_file" = destination)
  message("Finished creation of ", pkgname, ".")
  return(retlist)
}
