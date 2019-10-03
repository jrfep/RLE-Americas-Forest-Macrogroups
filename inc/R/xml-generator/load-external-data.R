## Load data
## a) ecosystem classification: run script in inc/R/classification-crosswalk
load(sprintf("%s/assets/descriptive-docs/Macrogroup_Classification_Crosswalk.rda", script.dir))

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
## f) list of Threats
threat.match <- read_ods(sprintf("%s/assets/descriptive-docs/ThreatsPerMacrogroup.ods", script.dir),sheet=1)
threat.desc <- read_ods(sprintf("%s/assets/descriptive-docs/ThreatsPerMacrogroup.ods", script.dir),sheet=2)

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
