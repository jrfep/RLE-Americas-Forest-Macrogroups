#! R --vanilla

## working directory and path to scripts
script.dir <- Sys.getenv("SCRIPTDIR")
work.dir <- Sys.getenv("WORKDIR")
setwd(work.dir)

## path to xml-generator scripts:
inc.dir <- sprintf("%s/inc/R/classification-crosswalk",script.dir)
## path for output
out.dir <- sprintf("%s/assets/descriptive-docs",script.dir)
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

## level 1 is the same for everyone:
mcdgs <- Macrogroups.Global$IVC.macrogroup_key
  dts <- rbind(
    data.frame(mcdg=mcdgs,classification="IUCN habitat version 3.0", level=1, name="1. Forest"),
    data.frame(mcdg=mcdgs,classification="RLE Global Typology version draft", level=1, name="Terrestrial"),
    data.frame(mcdg=mcdgs,classification="IVC version 2014", level=1, name="1 Forest and Woodland"))

for (mcdg in subset(Macrogroups.Global,grepl("1.A.[1-3]", IVC.formation))$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=2, name="T1 Tropical-subtropical forests"),
        data.frame(mcdg=mcdg, classification="IVC version 2014", level=2, name="1.A Tropical Forest & Woodland"))
}
for (mcdg in subset(Macrogroups.Global,grepl("1.A.4", IVC.formation))$IVC.macrogroup_key) {
  dts <- rbind(dts,
  data.frame(mcdg=mcdg, classification="IVC version 2014", level=2, name="1.A Tropical Forest and Woodland"),
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=2, name="TF1 Palustrine wetlands"),
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=3, name="TF1.1  Tropical flooded forests and peat forests"),
        data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0", level=2, name="1.8. Forest – Subtropical/tropical swamp"))
}
for (mcdg in subset(Macrogroups.Global,grepl("1.A.5", IVC.formation))$IVC.macrogroup_key) {
    dts <- rbind(dts,
    data.frame(mcdg=mcdg, classification="IVC version 2014", level=2, name="1.A Tropical Forest & Woodland"),
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=2, name="MFT1 Brackish tidal systems"),
            data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=3, name="MFT1.2 Intertidal forests and shrublands"),
        data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0", level=2, name="1.7. Forest – Subtropical/tropical mangrove vegetation above high tide level"))
}
for (mcdg in subset(Macrogroups.Global,grepl("^1.B",IVC.formation))$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg, classification="IVC version 2014", level=2, name="1.B Temperate and Boreal Forest and Woodland"),
        data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0", level=2, name="1.4. Forest – Temperate"))
}
for (mcdg in subset(Macrogroups.Global,grepl("1.A.1", IVC.formation))$IVC.macrogroup_key) {
  dts <- rbind(dts,
    data.frame(mcdg=mcdg,classification="RLE Global Typology version draft", level=3, name="T1.2 Tropical/Subtropical dry forests and scrubs"),
    data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0", level=2, name="1.5. Forest – Subtropical/tropical dry"))
}

for (mcdg in subset(Macrogroups.Global,IVC.formation %in% "1.A.2")$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0",level=2,name="1.6. Forest – Subtropical/tropical moist lowland"),
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T1.1 Tropical/Subtropical lowland rainforests"))
}
for (mcdg in subset(Macrogroups.Global,IVC.formation %in% "1.A.3")$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg,classification="IUCN habitat version 3.0",level=2,name="1.9. Forest – Subtropical/tropical moist montane"),
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T1.3 Tropical/Subtropical montane rainforests"))
}
for (mcdg in subset(Macrogroups.Global,grepl("1.B.[1-2]",IVC.formation))$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=2,name="T2 Temperate-boreal forests and woodlands"))
}
for (mcdg in subset(Macrogroups.Global,grepl("1.B.3",IVC.formation))$IVC.macrogroup_key) {
    dts <- rbind(dts,
        data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=2,name="TF1 Palustrine wetlands"),
            data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="TF1.2 Subtropical/temperate forested wetlands"))
}

for (mcdg in sprintf("M%03d",c(652,653,654,9,656,657,501))) {
      dts <- rbind(dts,
              data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T2.2 Temperate deciduous forests and shrublands"))
}

for (mcdg in sprintf("M%03d",c(7,655,658,659,12,13,14,16,159,502,20,21,22,500,26,27,23,25))) {
      dts <- rbind(dts,
              data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T2.1 Boreal and montane needle-leaved forest and woodland"))
}

for (mcdg in sprintf("M%03d",c(24))) {
      dts <- rbind(dts,
              data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T2.3 Oceanic temperate rainforests"))
}

for (mcdg in sprintf("M%03d",c(9))) {
      dts <- rbind(dts,
              data.frame(mcdg=mcdg,classification="RLE Global Typology version draft",level=3,name="T2.6 Temperate pyric sclerophyll forests and woodlands"))
}
dts <- rbind(dts,
  data.frame(mcdg=Macrogroups.Global$IVC.macrogroup_key, classification="IVC version 2014", level=3, name=tipologia[match(Macrogroups.Global$IVC.formation,tipologia$Division.Code),"formation"]))
  dts <- rbind(dts,
    data.frame(mcdg=Macrogroups.Global$IVC.macrogroup_key, classification="IVC version 2014", level=4, name=tipologia[match(Macrogroups.Global$IVC.division,tipologia$Division.Code),"division"]))
    dts <- rbind(dts,
      data.frame(mcdg=Macrogroups.Global$IVC.macrogroup_key, classification="IVC version 2014", level=5, name=Macrogroups.Global$IVC.Name))

dts <- dts[order(dts$classification,dts$level),]
    subset(dts,mcdg=="M134")

classification.cross.walk <- dts

##output
output.file <- sprintf("%s/Macrogroup_Classification_Crosswalk.rda",out.dir)
save(file=output.file,classification.cross.walk)
