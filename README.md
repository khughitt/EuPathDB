# EuPathDB AnnotationHub Recipes and Access

This package contains code for generating Bioconductor objects
([OrgDb](https://bioconductor.org/packages/release/BiocViews.html#___OrgDb),
[OrganismDbi](https://bioconductor.org/packages/release/bioc/html/OrganismDbi.html),
[TxDb](https://bioconductor.org/packages/release/BiocViews.html#___TxDb),
[GRanges](https://bioconductor.org/packages/release/bioc/html/GenomicRanges.html), and
[BSGenome](https://bioconductor.org/packages/release/bioc/html/BSgenome.html)) for pathogens found
on one of the [EuPathDB](http://eupathdb.org/eupathdb/) databases:

- [AmoebaDB](http://amoebadb.org/)
- [CryptoDB](http://cryptodb.org/)
- [FungiDB](http://fungidb.org/)
- [GiardiaDB](http://giardiadb.org/)
- [MicrosporidiaDB](http://microsporidiadb.org/)
- [PiroplasmaDB](http://piroplasmadb.org/)
- [PlasmoDB](http://plasmodb.org/)
- [SchistoDB](http://schistodb.net/)
- [ToxoDB](http://toxodb.org/)
- [TrichDB](http://trichdb.org/)
- [TriTrypDB](http://tritrypdb.org/)

The primary purposes for this package are:

1.  Generate organism-specific Bioconductor resources to be made available through
[AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html).
2.  Generate installable packages for the various resources.
3.  Provide shortcuts for accessing these resources.

To access EuPathDB resources via AnnotationHub, simply load the AnnotationHub package and use the
query function like you would for any other AnnotationHub resource:

```r
library(AnnotationHub)

# create an AnnotationHub connection
ah <- AnnotationHub()

# use AnnotationHub to get an OrgDb object for T. gondii ME49
res <- query(ah, c("Toxoplasma gondii ME49", "OrgDb", "EuPathDB"))
orgdb <- res[[1]]
```

To create a local package of a EuPathDB resource, and get information from it:

```r
library(EuPathDB)

## I pretty much always use Leishmania major strain friedlin as my example.
lm_entry <- get_eupathdb_entry(species = "Friedlin")
lm_name <- sc_entry[["Species"]]
lm_name

orgdb_pkgname <- make_eupathdb_orgdb(lm_entry)
txdb_pkgname <- make_eupathdb_txdb(lm_entry)
bsgenome_pkgname <- make_eupathdb_bsgenome(lm_entry)
organismdbi_pkgname <- make_eupathdb_organismdbi(lm_entry)

## Get a big monster data table of annotations
major_annotations <- load_orgdb_annotations(orgdb_pkgname)

## Or GO categories
major_go <- load_orgdb_go(orgdb_pkgname)

avail_columns <- AnnotationDbi::columns(orgdb_pkgname)

## Or interpro categories
chosen_columns_idx <- grepl(x = avail_columns, pattern = "^INTERPRO")
chosen_columns <- avail_columns[chosen_columns_idx]
lm_interpro <- load_orgdb_go(lm_orgdb, columns = chosen_columns)

## Or Orthologs
chosen_columns_idx <- grepl(x = avail_columns, pattern = "^ORTHOLOGS")
chosen_columns <- avail_columns[chosen_columns_idx]
lm_ortho <- load_orgdb_go(lm_orgdb, columns = chosen_columns)

## Or Pathway data
chosen_columns_idx <- grepl(x = avail_columns, pattern = "^PATHWAY")
chosen_columns <- avail_columns[chosen_columns_idx]
lm_path <- load_orgdb_go(lm_orgdb, columns = chosen_columns)

## Or KEGG
chosen_columns_idx <- grepl(x = avail_columns, pattern = "KEGG")
chosen_columns <- avail_columns[chosen_columns_idx]
lm_kegg <- load_orgdb_go(lm_orgdb, columns = chosen_columns)
```

See also:

- [Accessing EuPathDB Resources using AnnotationHub](https://bioconductor.org/packages/release/data/annotation/vignettes/EuPathDB/inst/doc/EuPathDB.html)
- [AnnotationHub How-To’s](https://bioconductor.org/packages/release/bioc/vignettes/AnnotationHub/inst/doc/AnnotationHub-HOWTO.html)
- [Bioconductor - AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html)
