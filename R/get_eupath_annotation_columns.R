get_eupath_annotation_columns <- function(service = "fungidb") {

  shared_columns <- c(
    "primary_key", "has_missing_transcripts", "gene_name", "gene_source_id",
    "gene_previous_ids", "gene_product", "transcript_product", "gene_exon_count",
    "exon_count", "gene_transcript_count", "three_prime_utr_length", "five_prime_utr_length",
    "strand", "gene_type", "is_pseudo", "transcript_length", "gene_entrez_id", "uniprot_id",
    "chromosome", "gene_location_text", "location_text", "sequence_id", "organism",
    "gene_ortholog_number", "gene_orthomcl_name", "gene_paralog_number", "gene_hts_noncoding_snps",
    "gene_hts_nonsyn_syn_ratio", "gene_hts_nonsynonymous_snps", "gene_hts_stop_codon_snps",
    "gene_hts_synonymous_snps", "gene_total_hts_snps", "cds", "transcript_sequence",
    "protein_sequence", "protein_length", "cds_length", "molecular_weight", "isoelectric_point",
    "interpro_id", "interpro_description", "pfam_id", "pfam_description", "pirsf_id",
    "pirsf_description", "prositeprofiles_id", "prositeprofiles_description", "smart_id",
    "smart_description", "superfamily_id", "superfamily_description", "tigrfam_id",
    "tigrfam_description", "tm_count", "signalp_peptide", "signalp_scores")

  ## At some point I will figure out how to handle the fact that each webservice
  ## now has columns which are specific to it.  Presumably this will involve sending a GET
  ## to https://fungidb.org/fungidb/app/web-services-help?searchName=GenesByTaxon
  ## and figuring out how to parse out the columns; unfortunately they are now hidden by some
  ## bullshit javascript, so that will likely provide annoying.
  unshared_columns <- list(
    "fungidb" = c("fuck", "this"))
  return(shared_columns)
}
