## Load data
## a) ecosystem classification: run script in inc/R/classification-crosswalk
load(sprintf("%s/assets/descriptive-docs/Macrogroup_Classification_Crosswalk.rda", script.dir))

## b) Macrogroup concepts
if (file.exists(sprintf("%s/TablaBosquesNS.csv",rtd.dir))) {
  rsm.MG1 <- read.csv(sprintf("%s/TablaBosquesNS.csv",rtd.dir),
    stringsAsFactors=F)
  rsm.MG2 <- read.csv(sprintf("%s/TablaBosquesNS2.csv", rtd.dir), stringsAsFactors=F)
  rsm.MG3 <- read.csv(sprintf("%s/TablaBosquesNS3.csv", rtd.dir), stringsAsFactors=F)
  rsm.MG4 <- read.csv(sprintf("%s/TablaBosquesNS4.csv", rtd.dir), stringsAsFactors=F)
  rsm.MG5 <- read.csv(sprintf("%s/TablaBosquesNS5.csv", rtd.dir), stringsAsFactors=F)
}
## c) distribution validation
confTest <- read_ods(sprintf("%s/results/OO_files/TableS2_confidenceTests.ods", script.dir),skip=1)
## d) assessment outcomes
load(sprintf("%s/results/Rdata/20181123_MacrogroupsCountry.rda", script.dir))
## e) Assigned Case study IDs
ATids <- read_excel(sprintf("%s/assets/db-management/CaseStudyID_ATid_America2018.xlsx", script.dir))
ATids %>% filter(Type %in% "Regional") -> ATids.reg
## f) list of Threats
threat.match <- read_ods(sprintf("%s/assets/descriptive-docs/ThreatsPerMacrogroup.ods", script.dir),sheet=1)
threat.desc <- read_ods(sprintf("%s/assets/descriptive-docs/ThreatsPerMacrogroup.ods", script.dir),sheet=2)
threat.match <- unique(rbind(threat.match,
   data.frame(Macrogroup=subset(Macrogroups.Global,!A1 %in% "NE")$IVC.macrogroup_key, code="Deforestation"),
   data.frame(Macrogroup=subset(Macrogroups.Global,!C2a %in% "NE")$IVC.macrogroup_key, code="ClimateChange"),
   ##      data.frame(Macrogroup=subset(Macrogroups.Global,!C2b %in% "NE")$IVC.macrogroup_key, code="SurfaceWater"), ## need to asign a threat to  this, ignoring so far...
   data.frame(Macrogroup=subset(Macrogroups.Global,!D1 %in% "NE")$IVC.macrogroup_key, code="increasingPopulation"),
   data.frame(Macrogroup=subset(Macrogroups.Global,!D1 %in% "NE")$IVC.macrogroup_key, code="increasingAgriculture"),
   data.frame(Macrogroup=subset(Macrogroups.Global,!D3 %in% "NE")$IVC.macrogroup_key, code="increasingGrazing"),
      data.frame(Macrogroup=subset(Macrogroups.Global,!D2b %in% "NE")$IVC.macrogroup_key, code="Defaunation")))

## g) basic spatial data
##TMWB <- read.dbf(sprintf("%s/admin/TMWB/TM_WORLD_BORDERS-0.3.dbf",gis.data))

Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Virgin Islands"] <- "Virgin Islands, U.S."
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "British Virgin Islands"] <- "Virgin Islands, British"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Turks and Caicas Islands"]   <- "Turks and Caicos Islands"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Bonaire, Saint Eustatius and Saba"] <- "Bonaire, Sint Eustatius and Saba"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Sint Maarten"] <- "Sint Maarten (Dutch part)"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Saint Martin"] <- "Saint Martin (French part)"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Venezuela"] <- "Venezuela, Bolivarian Republic of"
Macrogroups.Country$Country[Macrogroups.Country$Country %in% "Bolivia"] <- "Bolivia, Plurinational State of"

##country.list <- unique(Macrogroups.Country$Country)
##iso.list <- ISO_3166_1[match(country.list, ISO_3166_1$Name),"Alpha_2"]
##country.list[is.na(iso.list)]

## assessment summary
OverallCat <- read.csv(sprintf("%s/results/csvs/OverallCat.csv",script.dir))

## lower left and upper right coordinates of bounding box:
xys <-  read.table("MG_latlon_range.txt",col.names=c("code","xmin","xmax","ymin","ymax","area"),  na.strings = "*",colClasses=c("numeric"))
llc <- rgdal::project(as.matrix(xys[,c(2,4)]), proj="+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",inv=T)
urc <- rgdal::project(as.matrix(xys[,c(3,5)]), proj="+proj=robin +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",inv=T)

rownames(llc) <- sprintf("M%03d",xys$code)
rownames(urc) <- sprintf("M%03d",xys$code)
