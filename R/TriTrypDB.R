#!/usr/bin/env Rscript
#
# TriTrypDB AnnotationHub Recipe
# Author: Keith Hughitt (khughitt@umd.edu)
# November 05, 2015
#
# References
# ----------
# 1. http://bioconductor.org/packages/release/bioc/vignettes/AnnotationHub/inst/doc/AnnotationHubRecipes.html
# 2. https://github.com/Bioconductor-mirror/AnnotationHubData/tree/master/R
#
library('AnnotationForge')
library('AnnotationHubData')
library('dplyr')

# Current version
get_tritrypdb_version = function() {
    library(RCurl)
    tritrypdb_version_url = 'http://tritrypdb.org/common/downloads/Current_Release/Build_number'
    gsub('\n', '', as.character(getURLContent(tritrypdb_version_url)))
}

## helper to make metadata list from the data
.tritrypdb_metadata_from_url = function(base_url, metadata, version) {
	metadata %>% 
		mutate(
			title=sprintf('org.%s.tritryp26.sqlite', gene_prefix),
			genome=sprintf("TriTrypDB %s genomes", version),
			sourceVersion=sprintf("TriTrypDB %s", version),
			sourceURL=paste0(base_url, species_id),
			description=sprintf('TriTrypDB annotations for %s', species),
			rDataPath=sprintf('tritrypdb26/org.%s.tritryp26.sqlite', gene_prefix)
		) %>%
		select(-species_id, -gene_prefix)
}

# Create TriTrypDB AnnotationHubMetadata objects
create_tritrypdb_ahms = function(currentMetadata, justRunUnitTest, BiocVersion) {
	# Base URL for current version of TriTrypDB
    base_url = 'http://tritrypdb.org/common/downloads/Current_Release/'

	# Load species 
	species = read.csv('species.csv')

	# Current version
	version = get_tritrypdb_version()

    ## Make list of metadata in a helper function
    meta = .tritrypdb_metadata_from_url(base_url, species, version)

    ## then make AnnotationHubMetadata objects.
    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=list(
			BiocVersion=BiocVersion,
			SourceType="TriTrypDB",
			Coordinate_1_based=TRUE,
			DataProvider="TriTrypDB",
			Maintainer="Keith Hughitt <khughitt@umd.edu>",
			RDataClass="TriTrypDb",
			DispatchClass="SQLiteFile",
			RDataDateAdded=Sys.time(),
			RDataVersion="0.0.1",
			Recipe="AnnotaionHubData:::tritryp_to_dbs_recipe",
			Tags=c("TriTrypDB", "EuPathDB", "Trypanosome", "Kinetoplastid", "Annotation")
		))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe
## object.
tritryp_to_dbs_recipe = function(ahm){
    input_files = metadata(ahm)$SourceFile 
    # InParanoid-specific -- need to replace...
    # https://github.com/Bioconductor-mirror/AnnotationForge/blob/master/R/makeInparanoidDbs.R
    dbname = makeInpDb(dir=file.path(input_files,""),
                        dataDir=tempdir())
    db = loadDb(file=dbname)
    outputPath = file.path(metadata(ahm)$AnnotationHubRoot,
                            metadata(ahm)$RDataPath)
    saveDb(db, file=outputPath) 
    outputFile(ahm)
}

## STEP 3:  Call the helper to set up the newResources() method
makeAnnotationHubResource("TriTrypDBImportPreparer", create_tritrypdb_ahms)

