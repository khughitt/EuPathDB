# EuPathDB AnnotationHub Recipes and access

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

To access EuPathDB resources via AnnotationHub, simply load the
AnnotationHub package and use the query function like you would for
any other AnnotationHub resource:

```{r query_ah}
library(AnnotationHub)

# create an AnnotationHub connection
ah <- AnnotationHub()

# use AnnotationHub to get an OrgDb object for T. gondii ME49
res <- query(ah, c('Toxoplasma gondii ME49', 'OrgDb', 'EuPathDB'))
orgdb <- res[[1]]
```

To create a local package of a EuPathDB resource, and get information
from it; note the following code is also located in:
tests/testthat/test_99README.R

```{r}
library(EuPathDB)
## I pretty much always use Leishmania major strain friedlin as my example.

## This downloads metadata from every eupathdb resource:
## cryptodb, fungidb, giardiadb, microsporidiadb, piroplasmadb,
## plasmodb, toxodb, trichdb, and the tritrypdb.
eu_metadata <- download_eupath_metadata()
## One likely wants:
tritryp_metadata <- download_eupath_metadata(webservice = "tritrypdb")
## Download metadata from tritrypdb, this step is optional.
tritryp_metadata <- download_eupath_metadata(webservice = "tritrypdb")
## Extract an entry of interest, if metadata is not provided it will
## download it (defaulting to all eupathdb webservices, which can take
## a little while.
lm_entry <- get_eupath_entry(species = "Friedlin", metadata = tritryp_metadata)
## Look at the entry of interest.
lm_name <- sc_entry[["Species"]]
lm_name
## Create an orgdb database
orgdb_pkgname <- make_eupath_orgdb(lm_entry)
## Create a txdb database, since there are so few introns in the
## trypanosomatids, it tends to be less interesting for them...
txdb_pkgname <- make_eupath_txdb(lm_entry)
## Create a bsgenome, note you _must_ increase the number of open
## files for this to work with fragmented assemblies.[1]
bsgenome_pkgname <- make_eupath_bsgenome(lm_entry)
## Create the union of the orgdb/Txdb; this has not been tested in a _long_ time
organismdbi_pkgname <- make_eupath_organismdbi(lm_entry)
## Get a big monster data table of annotations
pkgname <- as.character(orgdb_pkgname[[1]]) ## In theory I set a
## setMethod to handle this without the subset, but it doesn't work yet.
major_annotations <- load_orgdb_annotations(pkgname)
## Or GO categories
major_go <- load_orgdb_go(pkgname)
library(pkgname, character = TRUE)
avail_columns <- AnnotationDbi::columns(get0(pkgname))
## Or interpro categories
lm_interpro <- load_orgdb_go(pkgname, table = "interpro")
## Or Orthologs
lm_ortho <- load_orgdb_go(lm_orgdb, table = "orthologs")
## Or Pathway data
lm_path <- load_orgdb_go(lm_orgdb, table = "pathway")
## Or PDB
lm_pdb <- load_orgdb_go(lm_orgdb, table = "pdb")
## Or links to other database
lm_linkout <- load_orgdb_go(lm_orgdb, table = "linkout")
## Or publications
lm_pubmed <- load_orgdb_go(lm_orgdb, table = "pubmed")
```

Check the tests and vignettes for more examples!


1: If one attempts to make a bsgenome from a fragmented scaffold,
there is a good chance it will fail with 'Too many open files.'  On
linux, check your version of /etc/sysctl.conf (debian) for the entries
'sys.fs.fil-max' and/or 'fs.file-max'.  On my computer, they look like
this:

fs.file-max = 209708
sys.fs.file-max = 209708

You can check these values via 'sysctl -a'.  In addition, you will
want to make sure that you do not have a ulimit on the number of open
files.  Check this with 'ulimit -n'.  If I recall properly, the worst
assembly I have noticed has ~ 8k scaffolds.
