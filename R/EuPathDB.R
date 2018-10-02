#' EuPathDB: Access EuPathDB annotations using AnnotationHub
#'
#' EuPathDB provides an R interface for retrieving annotation resources from
#' the EuPathDB databases: AmoebaDB, CryptoDB, FungiDB, GiardiaDB,
#' MicrosporidiaDB, PiroplasmaDB, PlasmoDB, ToxoDB, TrichDB, and TriTrypDB
#' using the Bioconductor AnnotationHub framework.
#'
#' There are currently two types of Bioconductor resources which can be
#' retrieved for 194 supported organisms from the various EuPathDB databases:
#'
#' \itemize{
#' \item OrgDB resources
#' \item GRanges resources
#' }
#'
#' The OrgDB resources provides gene level information including chromosome,
#' location, name, description, orthologs, and associated GO terms.
#'
#' The GRanges resources provide transcript-level information such as known
#' exons and their corresponding locations.
#'
#' Each of these resources are generated using information obtained from the
#' EuPathDB GFF files along with queries made through the various EuPathDB web
#' APIs.
#'
#' For examples of how EuPathDB can be used to query and interact with
#' EuPathDB.org resources, take a look at the vignette:
#' \code{browseVignettes(package="EuPathDB")}
#'
#' Use \code{availableEuPathDB()} to get a vector of available organisms.
#'
#' @docType package
#' @name EuPathDB
#' @seealso \code{\link{AnnotationHub}}
#' @seealso \code{\link{GRanges}}
#' @seealso  \url{http://eupathdb.org/eupathdb/}
NULL

##' @title Get started with EuPathDB
##' @return Used for its side-effect of opening the package vignette. A
##'         vector of experiment identifiers.
##' @author Keith Hughitt
##' @aliases availableEuPathDB
##' @examples availableEuPathDB
EuPathDB <- function() {
    vignette("EuPathDB", package="EuPathDB")
}

availableEuPathDB <- c('Acanthamoeba castellanii str. Neff',
                       'Albugo candida 2VRR',
                       'Albugo laibachii Nc14',
                       'Allomyces macrogynus ATCC 38327',
                       'Anncaliia algerae PRA109',
                       'Anncaliia algerae PRA339',
                       'Aspergillus aculeatus ATCC 16872',
                       'Aspergillus brasiliensis CBS 101740',
                       'Aspergillus campestris IBT 28561',
                       'Aspergillus carbonarius ITEM 5010',
                       'Aspergillus clavatus NRRL 1',
                       'Aspergillus fischeri NRRL 181',
                       'Aspergillus flavus NRRL3357',
                       'Aspergillus fumigatus A1163',
                       'Aspergillus fumigatus Af293',
                       'Aspergillus glaucus CBS 516.65',
                       'Aspergillus luchuensis CBS 106.47',
                       'Aspergillus nidulans FGSC A4',
                       'Aspergillus niger ATCC 1015',
                       'Aspergillus niger CBS 513.88',
                       'Aspergillus novofumigatus IBT 16806',
                       'Aspergillus ochraceoroseus IBT 24754',
                       'Aspergillus oryzae RIB40',
                       'Aspergillus steynii IBT 23096',
                       'Aspergillus sydowii CBS 593.65',
                       'Aspergillus terreus NIH2624',
                       'Aspergillus tubingensis CBS 134.48',
                       'Aspergillus versicolor CBS 583.65',
                       'Aspergillus wentii DTO 134E9',
                       'Aspergillus zonatus CBS 506.65',
                       'Babesia bovis T2Bo',
                       'Babesia microti strain RI',
                       'Batrachochytrium dendrobatidis JEL423',
                       'Botrytis cinerea B05.10',
                       'Candida albicans SC5314',
                       'Candida albicans WO-1',
                       'Candida glabrata CBS 138',
                       'Chromera velia CCMP2878',
                       'Clavispora lusitaniae ATCC 42720',
                       'Coccidioides immitis H538.4',
                       'Coccidioides immitis RS',
                       'Coccidioides posadasii C735 delta SOWgp',
                       'Coccidioides posadasii RMSCC 3488',
                       'Coccidioides posadasii str. Silveira',
                       'Coprinopsis cinerea okayama7#130',
                       'Cryptococcus deuterogattii R265',
                       'Cryptococcus gattii CA1873',
                       'Cryptococcus gattii EJB2',
                       'Cryptococcus gattii VGIV IND107',
                       'Cryptococcus gattii WM276',
                       'Cryptococcus neoformans var. grubii H99',
                       'Cryptococcus neoformans var. neoformans B-3501A',
                       'Cryptococcus neoformans var. neoformans JEC21',
                       'Cryptosporidium hominis TU502',
                       'Cryptosporidium muris RN66',
                       'Cryptosporidium parvum Iowa II',
                       'Edhazardia aedis USNM 41457',
                       'Eimeria tenella strain Houghton',
                       'Encephalitozoon cuniculi EC1',
                       'Encephalitozoon cuniculi EC2',
                       'Encephalitozoon cuniculi EC3',
                       'Encephalitozoon cuniculi GB-M1',
                       'Encephalitozoon hellem ATCC 50504',
                       'Encephalitozoon intestinalis ATCC 50506',
                       'Encephalitozoon romaleae SJ-2008',
                       'Entamoeba dispar SAW760',
                       'Entamoeba histolytica HM-1:IMSS',
                       'Entamoeba histolytica HM-1:IMSS-A',
                       'Entamoeba histolytica HM-1:IMSS-B',
                       'Entamoeba histolytica HM-3:IMSS',
                       'Entamoeba histolytica KU27',
                       'Entamoeba invadens IP1',
                       'Entamoeba nuttalli P19',
                       'Enterocytozoon bieneusi H348',
                       'Fusarium fujikuroi IMI 58289',
                       'Fusarium graminearum PH-1',
                       'Fusarium oxysporum f. sp. cubense race 1',
                       'Fusarium oxysporum f. sp. cubense race 4',
                       'Fusarium oxysporum f. sp. cubense tropical race 4 54006',
                       'Fusarium oxysporum f. sp. lycopersici 4287',
                       'Fusarium oxysporum f. sp. melonis 26406',
                       'Fusarium oxysporum Fo47',
                       'Fusarium verticillioides 7600',
                       'Giardia Assemblage A isolate WB',
                       'Giardia Assemblage B isolate GS',
                       'Giardia Assemblage E isolate P15',
                       'Histoplasma capsulatum G186AR',
                       'Histoplasma capsulatum G217B',
                       'Histoplasma capsulatum H143',
                       'Histoplasma capsulatum H88',
                       'Histoplasma capsulatum NAm1',
                       'Hyaloperonospora arabidopsidis Emoy2',
                       'Leishmania aethiopica L147',
                       'Leishmania braziliensis MHOM/BR/75/M2903',
                       'Leishmania braziliensis MHOM/BR/75/M2904',
                       'Leishmania donovani BPK282A1',
                       'Leishmania infantum JPCM5',
                       'Leishmania major strain Friedlin',
                       'Leishmania major strain LV39c5',
                       'Leishmania major strain SD 75.1',
                       'Leishmania mexicana MHOM/GT/2001/U1103',
                       'Leishmania panamensis MHOM/COL/81/L13',
                       'Leishmania sp. MAR LEM2494',
                       'Leishmania tropica L590',
                       'Magnaporthe oryzae 70-15',
                       'Malassezia globosa CBS 7966',
                       'Melampsora larici-populina 98AG31',
                       'Mucor circinelloides f. lusitanicus CBS 277.49',
                       'Nematocida parisii ERTm1',
                       'Nematocida parisii ERTm3',
                       'Neospora caninum Liverpool',
                       'Neurospora crassa OR74A',
                       'Neurospora discreta FGSC 8579',
                       'Neurospora tetrasperma FGSC 2508',
                       'Nosema bombycis CQ1',
                       'Nosema ceranae BRL01',
                       'Ordospora colligata OC4',
                       'Paracoccidioides brasiliensis Pb03',
                       'Paracoccidioides brasiliensis Pb18',
                       'Paracoccidioides lutzii Pb01',
                       'Penicillium rubens Wisconsin 54-1255',
                       'Phanerochaete chrysosporium RP-78',
                       'Phycomyces blakesleeanus NRRL 1555(-)',
                       'Phytophthora capsici LT1534',
                       'Phytophthora cinnamomi var. cinnamomi CBS 144.22',
                       'Phytophthora infestans T30-4',
                       'Phytophthora parasitica INRA-310',
                       'Plasmodium berghei ANKA',
                       'Plasmodium chabaudi chabaudi',
                       'Plasmodium cynomolgi strain B',
                       'Plasmodium cynomolgi strain M',
                       'Plasmodium falciparum 3D7',
                       'Plasmodium inui San Antonio 1',
                       'Plasmodium knowlesi strain H',
                       'Plasmodium vivax Sal-1',
                       'Plasmodium yoelii yoelii 17XNL',
                       'Pneumocystis jirovecii SE8',
                       'Puccinia graminis f. sp. tritici CRL 75-36-700-3',
                       'Pythium aphanidermatum DAOM BR444',
                       'Pythium arrhenomanes ATCC 12531',
                       'Pythium irregulare DAOM BR486',
                       'Pythium iwayamai DAOM BR242034',
                       'Pythium ultimum DAOM BR144',
                       'Pythium ultimum var. sporangiiferum BR650',
                       'Pythium vexans DAOM BR484',
                       'Rhizopus delemar RA 99-880',
                       'Saccharomyces cerevisiae S288c',
                       'Saprolegnia diclina VS20',
                       'Saprolegnia parasitica CBS 223.65',
                       'Schizosaccharomyces japonicus yFS275',
                       'Schizosaccharomyces octosporus yFS286',
                       'Schizosaccharomyces pombe 972h-',
                       'Sclerotinia sclerotiorum 1980 UF-70',
                       'Sordaria macrospora k-hell',
                       'Spizellomyces punctatus DAOM BR117',
                       'Sporisorium reilianum SRZ2',
                       'Sporothrix schenckii 1099-18',
                       'Spraguea lophii 42_110',
                       'Talaromyces marneffei ATCC 18224',
                       'Talaromyces stipitatus ATCC 10500',
                       'Theileria annulata strain Ankara',
                       'Theileria equi strain WA',
                       'Theileria orientalis strain Shintoku',
                       'Theileria parva strain Muguga',
                       'Toxoplasma gondii ARI',
                       'Toxoplasma gondii FOU',
                       'Toxoplasma gondii GAB2-2007-GAL-DOM2',
                       'Toxoplasma gondii GT1',
                       'Toxoplasma gondii MAS',
                       'Toxoplasma gondii ME49',
                       'Toxoplasma gondii p89',
                       'Toxoplasma gondii RH',
                       'Toxoplasma gondii RUB',
                       'Toxoplasma gondii TgCatPRC2',
                       'Toxoplasma gondii VAND',
                       'Toxoplasma gondii VEG',
                       'Tremella mesenterica DSM 1558',
                       'Trichoderma reesei QM6a',
                       'Trichomonas vaginalis G3',
                       'Trypanosoma brucei brucei TREU927',
                       'Trypanosoma brucei gambiense DAL972',
                       'Trypanosoma congolense IL3000',
                       'Trypanosoma cruzi CL Brener Esmeraldo-like',
                       'Trypanosoma cruzi Dm28c',
                       'Trypanosoma cruzi strain CL Brener',
                       'Trypanosoma rangeli SC58',
                       'Trypanosoma vivax Y486',
                       'Uncinocarpus reesii 1704',
                       'Ustilago maydis 521',
                       'Vavraia culicis subsp. floridensis',
                       'Vitrella brassicaformis CCMP3155',
                       'Vittaforma corneae ATCC 50505',
                       'Yarrowia lipolytica CLIB122',
                       'Zymoseptoria tritici IPO323')
 
