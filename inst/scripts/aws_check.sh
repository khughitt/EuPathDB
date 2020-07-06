#!/usr/bin/env bash

aws --profile AnnotationContributor s3api list-objects --bucket annotationhub --page-size 8000 --query 'Contents[].{Key: Key}' | cut -f2 -d$'\t' | grep '^EuPathDB/GRanges/3.10' | cut -f4 -d'/' > GRangeS3List.txt
aws --profile AnnotationContributor s3api list-objects --bucket annotationhub --page-size 8000 --query 'Contents[].{Key: Key}' | cut -f2 -d$'\t' | grep '^EuPathDB/OrgDb/3.10' | cut -f4 -d'/' > OrgDbS3List.txt
aws --profile AnnotationContributor s3api list-objects --bucket annotationhub --page-size 8000 --query 'Contents[].{Key: Key}' | cut -f2 -d$'\t' | grep '^EuPathDB/TxDb/3.10' | cut -f4 -d'/' > TxDbS3List.txt
