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

## Load assessment outcomes
 load(dir(sprintf("%s/results/Rdata",script.dir),full.names=T))

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
