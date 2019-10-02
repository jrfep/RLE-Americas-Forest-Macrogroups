## Load data
## a) ecosystem classification: cambiar la tipologia por una tabla estructurada
cross.walks <- sprintf("%s/assets//", script.dir)

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
