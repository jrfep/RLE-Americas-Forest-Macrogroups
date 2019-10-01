#! R --vanilla

## working directory and path to scripts
script.dir <- Sys.getenv("SCRIPTDIR")
work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)

## path to xml-generator scripts:
inc.dir <- sprintf("%s/inc/R/xml-generator",script.dir)
## path for output
out.dir <- sprintf("%s/documentation/xml",script.dir)
output.file <- sprintf("%s/RA_Forest_Macrogroups_%s.xml",out.dir,ff)
## path for restricted assets (see assets documentation)
restricted.dir <- sprintf("%s/assets/descriptive-docs/restricted",script.dir)

## Load data
## a) EcoVeg typology
if (!exists("tipologia"))
    tipologia <- read.csv(sprintf("%s/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv",rtd.dir), stringsAsFactors=F)
tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$frmt <-  substr(tipologia$Division.Code,1,5)
## b) Macrogroup concepts
rsm.MG1 <- read.csv(sprintf("%s/TablaBosquesNS.csv",rtd.dir),
  stringsAsFactors=F)
rsm.MG2 <- read.csv(sprintf("%s/TablaBosquesNS2.csv", rtd.dir), stringsAsFactors=F)
rsm.MG3 <- read.csv(sprintf("%s/TablaBosquesNS3.csv", rtd.dir), stringsAsFactors=F)
rsm.MG4 <- read.csv(sprintf("%s/TablaBosquesNS4.csv", rtd.dir), stringsAsFactors=F)
rsm.MG5 <- read.csv(sprintf("%s/TablaBosquesNS5.csv", rtd.dir), stringsAsFactors=F)
## c) distribution validation
confTest <- read.csv(sprintf("%s/results/tables/TableS2_confidenceTests.csv", script.dir), stringsAsFactors=F)
## d) assessment outcomes
load(sprintf("%s/results/Rdata/20181123_MacrogroupsCountry.rda", script.dir))


 ## set-up auto-mode variables
 today <- "2019-09-30"
 auto.status <-"auto-generated-test"
 auto.agent <- "Rsaurio"
## needs an update?
 mi.reflabel <- "FerrerParis_Continental_ForestMacrogroup_2017"

## first, open the containing document
source(sprintf("%s/create-case-studies-doc.R",inc.dir))

## list of case studies
case.studies <- c("M134","M294")

## set counter to 1
  CS.counter <- 1

## for each case study:
for (case.study in case.studies) {
  CS.id <- sprintf("%s_%s",mi.reflabel, CS.counter)
  rsm <- subset(Macrogroups.Global,IVC.macrogroup_key %in% case.study)
  CS.name <- with(rsm,sprintf("%s: '%s, %s'",
    "Forest Macrogroups of the Americas",
    IVC.Name,
    ifelse(is.na(Overall.Category),"NE",
      ifelse(is.na(Overall.Bounds),Overall.Category,
        sprintf("%s (%s)",Overall.Category,Overall.Bounds)))))
  CS.altnames.en <- rsm$IVC.Name

  ## nodes related to assessment target (1 per case study)
  source(sprintf("%s/create-AT-nodes.R",inc.dir))

  ## nodes related to assessment units (1..n per case study)
  ## list of assessment units
  assessment.units <- c("")
  source(sprintf("%s/create-AU-nodes.R",inc.dir))

  ## now write case study
  source(sprintf("%s/create-case-study.R",inc.dir))


  CS.counter <- CS.counter + 1

}

## close xml document and write it to file
cat( saveXML(doc,file=output.file,
  prefix = '<?xml version="1.0" encoding="UTF-8"?>',
  encoding = "UTF-8"))
