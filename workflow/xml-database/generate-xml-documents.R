#! R --vanilla

## path to xml-generator scripts:
inc.dir <- sprintf("%s/inc/R/xml-generator",script.dir)
## path for output
out.dir <- sprintf("%s/documentation/xml",script.dir)
sprintf("%s/RA_Forest_Macrogroups_%s.xml",out.dir,ff)

## first, open the containing document
source(sprintf("%s/create-case-studies-doc.R",inc.dir))

## list of case studies
case.studies <- c("")

## for each case study:
for (case.study in case.studies) {
  ## nodes related to assessment target (1 per case study)
  source(sprintf("%s/create-AT-nodes.R",inc.dir))

  ## nodes related to assessment units (1..n per case study)
  ## list of assessment units
  assessment.units <- c("")
  source(sprintf("%s/create-AU-nodes.R",inc.dir))

  ## now write case study
  source(sprintf("%s/create-case-study.R",inc.dir))
}

## close xml document and write it to file
cat( saveXML(doc,file=output.file,
  prefix = '<?xml version="1.0" encoding="UTF-8"?>',
  encoding = "UTF-8"))
