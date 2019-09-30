
################
## Chile
###############

###############
## INPUTS
################

## Informe en Pdf

##información extraida del informe
chile.P <- subset(RLEDB[[2]],reference_code %in% "MAC_RLE_Chile_2015")
##información extraida de la publicación de Alaniz
chile.A <- subset(RLEDB[[2]],reference_code %in% "Alaniz_Chile_CentralChileHotspot_2016")


chile.A$ecoid <- NA
for (k in 1:nrow(chile.A)) {
    buscar <- rep(0,nrow(chile.P))
    for (bb in strsplit(chile.A$EcoName[k]," ")[[1]]) {
        buscar <- buscar+grepl(sprintf(" %s$",bb),chile.P$EcoName)+
            grepl(sprintf(" %s ",bb),chile.P$EcoName)
            grepl(sprintf("^%s ",bb),chile.P$EcoName)
    }
    chile.A$ecoid[k] <- chile.P[which.max(buscar),"ecosystem_ID"]

}


## shapefile

chile <- shapefile("~/respaldoAmericasDataFiles/Chile_IUCN_redlist.shp")

table(chile$Formacion)
gc()

## match between Chile and Macrogroups
ChileMGs <- read.xls("/home/jferrer/respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Chile Case Study/Chile veg_xwalk_IVC.xlsx")

chileB <- subset(RLEDB[[4]],grepl("Alaniz",ID))



match(chile@data$Pisos,chile.P$EcoName)
gc()

path <- "~/Dropbox/Provita/doc/000_BDRLE"
path <- "~/Provita/000_BDRLE"
source(sprintf("%s/xml01/inc/Chile/00_chile_ecosystems.R",path))
source(sprintf("%s/xml01/inc/Chile/01_chile_casestudies.R",path))

## Alaniz original file, but will need manual edits for each case study
##
source(sprintf("%s/xml01/inc/Chile/02_chile_Alaniz.R",path))


