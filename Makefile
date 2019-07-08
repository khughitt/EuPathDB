VERSION=1.0.0
export _R_CHECK_FORCE_SUGGESTS_=FALSE

all: clean roxygen reference check build test

build:
	@echo "Performing build with R CMD build"
	R CMD build .

check: roxygen
	@echo "Performing check with R CMD check"
	rm -rf ./..Rcheck 2>/dev/null 1>&2
	export _R_CHECK_FORCE_SUGGESTS_=FALSE && R CMD check . --no-build-vignettes
	@rm -rf ./..Rcheck 2>/dev/null 1>&2

clean:
	@echo "Cleaning up"

clean_vignette:
	rm -f vignettes/*.rda vignettes/*.map vignettes/*.Rdata

deps:
	@echo "Invoking devtools::install_dev_deps()"
	R -e "suppressPackageStartupMessages(suppressMessages(source('http://bioconductor.org/biocLite.R')));\
all = as.data.frame(devtools::dev_package_deps('.', dependencies=TRUE)); needed = all[['diff']] < 0; needed = all[needed, 'package']; for (t in needed) { if (class(try(suppressMessages(eval(parse(text=paste0('library(', t, ')')))))) == 'try-error') { BiocManager::install(t, update=FALSE) } }"

document: roxygen vignette reference

install:
	@echo "Performing R CMD INSTALL ."
	R CMD INSTALL .

prereq:
	@echo "Checking a few prerequisites."
	R -e "suppressPackageStartupMessages(suppressMessages(source('http://bioconductor.org/biocLite.R')));\
bioc_prereq <- c('BiocStyle','GenomicRanges','GenomeInfoDbData','AnnotationHubData','Biostrings','dplyr','rvest','AnnotationHub','R.utils', 'testthat','roxygen2','Biobase','devtools','rmarkdown','knitr','data.table','foreach');\
for (req in bioc_prereq) { if (class(try(suppressMessages(eval(parse(text=paste0('library(', req, ')')))))) == 'try-error') { BiocManager::install(req, update=FALSE) } } \
## hahaha looks like lisp!"

push:
	echo "Pushing to github."
	git commit -a && git push

reference:
	@echo "Generating reference manual with R CMD Rd2pdf"
	@mkdir -p inst/doc
	@rm -f inst/doc/reference.pdf
	R CMD Rd2pdf . -o inst/doc/reference.pdf --no-preview

roxygen:
	@echo "Generating documentation with devtools::document()"
	R -e "suppressPackageStartupMessages(devtools::document())"

s3:
	@echo "Invoking the aws client to upload the sqlite/rda/etc files to s3."
	aws configure --profile AnnotationContributor
	aws --profile AnnotationContributor s3 cp inst/scripts/EuPathDB/OrgDb "s3://annotation-contributor/EuPathDB/OrgDb" --recursive --acl public-read
##	aws --profile AnnotationContributor s3 cp inst/scripts/EuPathDB/GRanges "s3://annotation-contributor/EuPathDB/GRanges" --recursive --acl public-read
##	aws --profile AnnotationContributor s3 cp inst/scripts/EuPathDB/TxDb "s3://annotation-contributor/EuPathDB/TxDb" --recursive --acl public-read
##	aws --profile AnnotationContributor s3 cp inst/scripts/EuPathDB/OrganismDBI "s3://annotation-contributor/EuPathDB/OrganismDBI" --recursive --acl public-read
##	aws --profile AnnotationContributor s3 cp inst/scripts/EuPathDB/BSGenome "s3://annotation-contributor/EuPathDB/BSGenome" --recursive --acl public-read

suggests:
	@echo "Installing suggested packages."
	R -e "source('http://bioconductor.org/biocLite.R');\
library(desc);\
d = description\$$new(); suggests = d\$$get('Suggests');\
 suggests = gsub(pattern='\\n', replacement='', x=suggests);\
 suggests = gsub(pattern=' ', replacement='', x=suggests);\
 suggests = strsplit(x=suggests, split=',');\
 for (pkg in suggests[[1]]) { if (! pkg %in% installed.packages()) { biocLite(pkg); } else { message(paste0(pkg, ' is already installed.')) } };"

test: roxygen
	R CMD INSTALL .
	@echo "Running run_tests.R"
	tests/testthat.R

vignette: reference
	@cp inst/doc/reference.pdf inst/
	@echo "Building vignettes with devtools::build_vignettes()"
	R -e "devtools::build_vignettes()"
	@cp inst/reference.pdf doc/
	@cp inst/reference.Rnw doc/

vt:	clean_vignette vignette reference install
