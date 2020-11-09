## ----style, echo=FALSE, results='asis', message=FALSE--------------------
BiocStyle::markdown()

## ----libraries, eval=FALSE-----------------------------------------------
#  library(hpgltools)
#  library(EuPathDB)

## ----load, eval=FALSE----------------------------------------------------
#  library(org.Tcruzi.CL.Brener.Esmeraldo.like.v43.eg.db)
#  keytypes(org.Tcruzi.CL.Brener.Esmeraldo.like.v43.eg.db)
#  tc_annot <- load_orgdb_annotations("org.Tcruzi.CL.Brener.Esmeraldo.like.v43.eg.db", fields="all")
#  genes <- tc_annot[["genes"]]
#  ## Hmm I wonder if I can modify my uniprot code to use the taxonomy ID?
#  
#  ##
#  test <- download_uniprot_proteome(taxonomy="353153")
#  uniprot_annot <- load_uniprot_annotations(file=test[["filename"]])
#  colnames(uniprot_annot)
#  
#  merged <- merge(genes, uniprot_annot, by.x="annot_uniprot_id", by.y="primary_accession")
#  dim(merged)
#  ## SWWWWEEEETTT!

