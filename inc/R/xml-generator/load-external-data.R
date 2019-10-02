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
confTest <- read_ods(sprintf("%s/results/OO_files/TableS2_confidenceTests.ods", script.dir),skip=1)
## d) assessment outcomes
load(sprintf("%s/results/Rdata/20181123_MacrogroupsCountry.rda", script.dir))
## e) Assigned Case study IDs
ATids <- read_excel(sprintf("%s/assets/db-management/CaseStudyID_ATid_America2018.xlsx", script.dir))
ATids %>% filter(Type %in% "Regional") -> ATids.reg
