## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----01install_pkgs, eval=FALSE------------------------------------------
#  install.packages("BiocManager")
#  BiocManager::install("AnnotationHub")

## ----02load_ah-----------------------------------------------------------
library("EuPathDB")
library("AnnotationHub")

# create an AnnotationHub connection
ah <- AnnotationHub()

# search for all EuPathDB resources
meta <- query(ah, "EuPathDB")

length(meta)
head(meta)

# types of EuPathDB data available
table(meta$rdataclass)

# distribution of resources by specific databases
table(meta$dataprovider)

# list of organisms for which resources are available
length(unique(meta$species))
head(unique(meta$species))

## ----03query_major-------------------------------------------------------
res <- query(ah, c("OrgDb", "Leishmania major strain Friedlin"))
res

## ----04get_orgdb, eval=FALSE---------------------------------------------
#  chosen_ah <- res[[1]]
#  chosen_ah

## ----05query_major_columns, eval=FALSE-----------------------------------
#  # list available fields to retrieve
#  orgdb <- chosen_ah
#  columns(orgdb)
#  
#  # create a vector containing all gene ids for the organism
#  gids <- keys(orgdb, keytype="GID")
#  head(gids)
#  
#  # retrieve the chromosome, description, and biotype for each gene
#  dat <- select(orgdb, keys=gids, keytype="GID", columns=c("CHR", "TYPE", "GENEDESCRIPTION"))
#  
#  head(dat)
#  
#  table(dat$TYPE)
#  table(dat$CHR)
#  
#  # create a gene / GO term mapping
#  gene_go_mapping <- select(orgdb, keys=gids, keytype='GID',
#                            columns=c("GO_ID", "GO_TERM_NAME", "ONTOLOGY"))
#  head(gene_go_mapping)
#  
#  # retrieve KEGG, etc. pathway annotations
#  gene_pathway_mapping <- select(orgdb, keys=gids, keytype="GID",
#                                 columns=c("PATHWAY", "PATHWAY_SOURCE"))
#  table(gene_pathway_mapping$PATHWAY_SOURCE)
#  head(gene_pathway_mapping)

## ----06query_granges-----------------------------------------------------
# query AnnotationHub
res <- query(ah, c("Leishmania major strain Friedlin", "GRanges", "EuPathDB"))
res

# retrieve a GRanges instance associated with the result record
gr <- res[[1]]
summary(gr)
head(gr)

## ----07extract_granges---------------------------------------------------
# chromosome names
seqnames(gr)

# strand information
strand(gr)

# feature widths
head(width(gr))

## ----08get_types---------------------------------------------------------
# list of location types in the resource
table(gr$type)
table(gr@strand)

## ----09chr4--------------------------------------------------------------
# get the first three ranges
gr[1:3]

# get all gene entries on chromosome 4
chr4_genes <- gr[gr$type == 'gene' & seqnames(gr) == 'LmjF.04']
summary(chr4_genes)
## Hey, checkit, there are 130 genes on chromosome 4.

## ----10session_info------------------------------------------------------
sessionInfo()

