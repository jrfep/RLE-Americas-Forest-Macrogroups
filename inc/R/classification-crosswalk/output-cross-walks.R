#! R --vanilla

## working directory and path to scripts
script.dir <- Sys.getenv("SCRIPTDIR")
work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)

## path to xml-generator scripts:
inc.dir <- sprintf("%s/inc/R/classification-crosswalk",script.dir)
## path for output
out.dir <- sprintf("%s/assets/descriptive-docs",script.dir)
output.file <- sprintf("%s/Macrogroup_Classification_Crosswalk.rda",out.dir)
## path for restricted assets (see assets documentation)
rtd.dir <- sprintf("%s/assets/descriptive-docs/restricted",script.dir)

##input
##EcoVeg typology
if (!exists("tipologia"))
    tipologia <- read.csv(sprintf("%s/EcoVeg_typology_hierarchy 30 Jan 30 2015.csv",rtd.dir), stringsAsFactors=F)
tipologia$grp <-  substr(tipologia$Division.Code,1,1)
tipologia$sgrp <-  substr(tipologia$Division.Code,1,3)
tipologia$frmt <-  substr(tipologia$Division.Code,1,5)

## d) assessment outcomes
load(sprintf("%s/results/Rdata/20181123_MacrogroupsCountry.rda", script.dir))

## switch

table(Macrogroups.Global$IVC.formation)
subset(Macrogroups.Global,IVC.formation %in% "1.A.1")
1.A.1
T1 Tropical-subtropical forests	T1.2 Tropical/Subtropical dry forests and scrubs
1. Forest	1.5. Forest – Subtropical/tropical dry

1.A.2
T1 Tropical-subtropical forests	T1.1 Tropical/Subtropical lowland rainforests
1. Forest	1.6. Forest – Subtropical/tropical moist lowland

subset(Macrogroups.Global,IVC.formation %in% "1.A.3")
1.A.3
T1 Tropical-subtropical forests	T1.3 Tropical/Subtropical montane rainforests
1. Forest	1.9. Forest – Subtropical/tropical moist montane

subset(Macrogroups.Global,IVC.formation %in% "1.A.4")
1.A.4
TF1 Palustrine wetlands	TF1.1  Tropical flooded forests and peat forests
1. Forest	1.8. Forest – Subtropical/tropical swamp

subset(Macrogroups.Global,IVC.formation %in% "1.A.5")
1.A.5
MFT1 Brackish tidal systems	MFT1.2 Intertidal forests and shrublands
1. Forest	1.7. Forest – Subtropical/tropical mangrove vegetation above high tide level

1.B.1
T2 Temperate-boreal forests & woodlands	T2.2 Temperate deciduous forests and shrublands
1. Forest	1.4. Forest – Temperate

1.B.2
1.B.3
tipologia$frmt


T1 Tropical-subtropical forests	T1.4 Tropical heath forests
T2 Temperate-boreal forests & woodlands	T2.1 Boreal and montane needle-leaved forest and woodland
T2 Temperate-boreal forests & woodlands	T2.3 Oceanic temperate rainforests
T2 Temperate-boreal forests & woodlands	T2.4 Warm temperate rainforests
T2 Temperate-boreal forests & woodlands	T2.5 Temperate pyric humid forests
T2 Temperate-boreal forests & woodlands	T2.6 Temperate pyric sclerophyll forests and woodlands
TF1 Palustrine wetlands	TF1.2 Subtropical/temperate forested wetlands

1. Forest	1.1. Forest – Boreal
1. Forest	1.2. Forest - Subarctic
1. Forest	1.3. Forest – Subantarctic

##output
cross.walks <- sprintf("%s/assets//", script.dir)
