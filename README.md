# EuPathDB AnnotationHub Recipe

This package contains code for generating Bioconductor objects
([OrgDb](https://bioconductor.org/packages/release/BiocViews.html#___OrgDb), [OrganismDbi](https://bioconductor.org/packages/release/bioc/html/OrganismDbi.html), 
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
- [ToxoDB](http://toxodb.org/)
- [TrichDB](http://trichdb.org/)
- [TriTrypDB](http://tritrypdb.org/)

The primary purpose for this package is to generate organism-specific Bioconductor
resources to be made available through [AnnotationHub](https://bioconductor.org/packages/release/bioc/html/AnnotationHub.html).

Note that this package is _not_ necessary in order to access those resources on AnnotationHub, and
as such, most users do not need to install this package.

To access EuPathDB resources via AnnotationHub, simply load the AnnotationHub package and use the
query function like you would for any other AnnotationHub resource:

```r
library(AnnotationHub)

# create an AnnotationHub connection
ah <- AnnotationHub()

# use AnnotationHub to get an OrgDb object for T. gondii ME49
res <- query(ah, c('Toxoplasma gondii ME49', 'OrgDb', 'EuPathDB'))
orgdb <- res[[1]]
```

For more information on retrieving EuPathDB resources via AnnotationHub, refer to the EuPathDB
and AnnotationHub vignettes:

- [Accessing EuPathDB Resources using AnnotationHub](https://bioconductor.org/packages/release/data/annotation/vignettes/EuPathDB/inst/doc/EuPathDB.html)
- [AnnotationHub: Access the AnnotationHub Web Service](https://bioconductor.org/packages/release/bioc/vignettes/AnnotationHub/inst/doc/AnnotationHub.html)


