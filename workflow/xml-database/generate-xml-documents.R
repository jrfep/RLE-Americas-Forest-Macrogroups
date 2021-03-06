#! R --vanilla
require(readxl)
require(readODS)
require(dplyr)
require(XML)
require(gdata)
require(ISOcodes)

## working directory and path to scripts
script.dir <- Sys.getenv("SCRIPTDIR")
gis.data <- Sys.getenv("GISDATA")
work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)


cat.weights <- c(NA,NA,0,1,2,3,4,5)
names(cat.weights) <- c("NE","DD","LC","NT","VU","EN","CR","CO")

## path to xml-generator scripts:
inc.dir <- sprintf("%s/inc/R/xml-generator",script.dir)
## path for output
out.dir <- sprintf("%s/documentation/xml",script.dir)
## path for restricted assets (see assets documentation)
rtd.dir <- sprintf("%s/assets/descriptive-docs/restricted",script.dir)

 ## set-up auto-mode variables
 today <- "2020-04-30"
 auto.status <-"auto-generated-test"
 auto.agent <- "Rsaurio"
## needs an update?
 mi.reflabel <- "FerrerParis_Continental_ForestMacrogroup_2017"

## Load external data tables and summaries
source(sprintf("%s/load-external-data.R",inc.dir))

  ## list of case studies we want to process
  case.studies <- c("M012","M027","M134","M294","M591", "M642","M649","M652","M503")

case.studies <- unique(Macrogroups.Global$IVC.macrogroup_key)
  ## set counter to 1
  CS.counter <- 1


## for each case study:
for (case.study in sample(case.studies)) {
  print(case.study)
  output.file <- sprintf("%s/RA_Forest_Macrogroups_%s.xml",out.dir,case.study)
  if (!file.exists(output.file)) {
    ## Begin by opening the containing document and set up global nodes
    source(sprintf("%s/create-case-studies-doc.R",inc.dir))


    assess.total <- subset(Macrogroups.Global,IVC.macrogroup_key %in% case.study)
    assess.spa <- subset(SpatialCriteria.Global,IVC.macrogroup_key %in% case.study)
    assess.fun <- subset(FunctionalCriteria.Global,IVC.macrogroup_key %in% case.study)

   ## nodes related to assessment target (1 per case study)
   source(sprintf("%s/create-AT-nodes.R",inc.dir))

   ## nodes related to assessment units (1..n per case study)
   ## list of assessment units
   ## IN THIS VERSION we are only considering the regional assessment
   source(sprintf("%s/create-AU-nodes.R",inc.dir))

   ## now write case study
   source(sprintf("%s/create-case-study.R",inc.dir))


   CS.counter <- CS.counter + 1
   ## close xml document and write it to file
   cat( saveXML(doc,file=output.file,
     prefix = '<?xml version="1.0" encoding="UTF-8"?>',
     encoding = "UTF-8"))
  rm(doc)
  gc()
  }

}
