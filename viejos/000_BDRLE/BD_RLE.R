########
## ~/bin/basex/bin/basexhttp ## iniciar el servidor


##R --vanilla
require(ROpenOffice)
require(raster)
require(gdata)
require(leaflet)
require(htmlwidgets)
setwd("~/tmp/Provita")
dbdata.dir <- "~/Provita/data/RLEDB/"

##RLEDB <- read.ods(sprintf("%s/RLE assessment summary tables.ods",dbdata.dir))
##table(RLEDB[[1]]$reference_code %in% RLEDB[[2]]$reference_code)
##table(RLEDB[[2]]$reference_code %in% c("Alaniz_Chile_CentralChileHotspot_2016","FerrerParis_Continental_ForestMacrogroup_2017","Etter_RLE_Colombia_2015","MAC_RLE_Chile_2015"))
##gc()
##Vero.DB <-read.xls(sprintf("%s/Last Version RLE Database.xlsx",dbdata.dir))
##Vero.DB.todo <-read.xls(sprintf("%s/Last Version RLE Database.xlsx",dbdata.dir),sheet=2,stringsAsFactors=F)
##gc()

HQ.DB <-read.xls(sprintf("%s/RLE_Last_Version_database_HQ_November2017.xlsx",dbdata.dir),sheet=2,stringsAsFactors=F)

Jess.DB <-read.xls(sprintf("%s/RLE_database_16Nov2017_Jess.xlsx",dbdata.dir),sheet=1,stringsAsFactors=F,fileEncoding="latin1")
table(HQ.DB$Reference_code)
sort(with(HQ.DB,tapply(Ecosystem_ID_bis,list(Reference_code),function(x) length(unique(x)))))
sort(with(Jess.DB,tapply(Ecosystem_ID_bis,list(Reference_code),function(x) length(unique(x)))))


 xml.db <- "~/Dropbox/Provita/xml/RLE_test1"

refs <- unique(HQ.DB$Reference_code)
refs <- refs[!refs %in% c("","MAC_RLE_Chile_2015", "Alaniz_Chile_CentralChileHotspot_2016","Lindgaard_Norway_NorwegianRL_2011")]
for (mi.ref in refs) {
    ss <- subset(HQ.DB,Reference_code %in% mi.ref)
    
    doc = newXMLDoc()
    top = newXMLNode("Case-Studies",doc=doc)
    for (k in unique(ss$Ecosystem_ID_bis)) {
        kk <- subset(ss,Ecosystem_ID_bis %in% k)
        name.e <-iconv( unique(trim(kk$EcoName)),"latin1","utf8")
        name.o <- iconv(unique(trim(kk$EcoName_original)),"latin1","utf8")
        final.cat <- with(subset(kk,RLE.criterion. %in% "Overall"),
                      ifelse(RLE.bounds != "",
                             sprintf("%s (%s)",Overall,RLE.bounds),
                             Overall))
        
        if (length(final.cat )==0) {
            for (posible.categoria in c("LC","NT","VU","EN","CR","CO")) {
                if (any(kk$Overall %in% posible.categoria))
                    final.cat <- posible.categoria
            }
        }
        if (final.cat=="LC") {
            oa.cats <- ""
        } else {
            oa.cats <- subset(kk,!RLE.criterion. %in% "Overall")[subset(kk,!RLE.criterion. %in% "Overall")$Overall == subset(kk,RLE.criterion. %in% "Overall")$Overall,"RLE.criterion."]
            oa.cats <- oa.cats[!   oa.cats %in% c("A","B","C","D")]
            
        }
        
        
        if (mi.ref %in% c("Benson_CS_VineThicketSEAustralia_2013",
                          "Bonifacio_CS_KRSWetland_2013",
                          "Bonifacio_CS_SeagrassCommunity_2013",
                          "Essl_CS_GermanTamarisk_2013",
                          "FaberLnagendoen_CS_GreatLakesAlvar_2013",
                          "Keith_CS_CapeFlatsSandFynbos_2013",
                          "Keith_CS_CoastalSandstone_2013",
                          "Keith_CS_GiantKelp_2013",
                          "MacNally_RiverRedGum_2013",

                          "Keith_CS_CoolibahBlackBox_2013",
                          "Lester_CS_CoorongLagoons_2013",
                          "Moat_CS_TapiaForest_2013",
                          "OliveiraMiranda_CS_TepuiShrublands_2013 ",
                          "Riecken_CS_RaisedBogs_2013",
                          "Riecken_CS_RaisedBogs_2013","OliveiraMiranda_CS_TepuiShrublands_2013","Kingsford_CS_MurrayDarlingBasin_2013","Keith_CS_GonakierForest_2013","Keith_CS_CaribbeanCoralReefs_2013","Keith_CS_AralSea_2013","Holdaway_CS_GraniteGravelFields_2013","Poulin_CS_EuropeanReedbeds_2013")) {
            ref.lab <- "Keith_CS_Foundations_2013"
        } else {
            ref.lab <- mi.ref
        }
        
        source("~/Dropbox/Provita/doc/000_BDRLE/inc01_procesarFichasHQ.R")
    }

    print(mi.ref)
    cat( saveXML(doc,file=sprintf("%s/RA_%s.xml",xml.db,mi.ref),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
    
    rm(doc,top,RA,CS,AT,AU)
    gc()
}


################
## Jess

refs <- unique(Jess.DB$Reference_code)
for (mi.ref in refs) {
    ss <- subset(Jess.DB,Reference_code %in% mi.ref)
    
    doc = newXMLDoc()
    top = newXMLNode("Case-Studies",doc=doc)
    for (k in unique(ss$Ecosystem_ID_bis)) {
        kk <- subset(ss,Ecosystem_ID_bis %in% k)
        name.e <-iconv( unique(trim(kk$EcoName)),"latin1","utf8")
        name.o <- iconv(unique(trim(kk$EcoName_original)),"latin1","utf8")
        final.cat <- with(subset(kk,RLE_criterion %in% "Overall"),
                      ifelse(RLE.bounds != "",
                             sprintf("%s (%s)",Overall,RLE.bounds),
                             Overall))
        
        if (length(final.cat )==0) {
            for (posible.categoria in c("LC","NT","VU","EN","CR","CO")) {
                if (any(kk$Overall %in% posible.categoria))
                    final.cat <- posible.categoria
            }
        }
        if (length(final.cat)>0) {
            if (final.cat=="LC") {
                oa.cats <- ""
            } else {
                oa.cats <- subset(kk,!RLE_criterion %in% "Overall")[subset(kk,!RLE_criterion %in% "Overall")$Overall == subset(kk,RLE_criterion %in% "Overall")$Overall,"RLE_criterion"]
                oa.cats <- oa.cats[!   oa.cats %in% c("A","B","C","D")]
                
            }
            
            if (mi.ref == "Bland_Caribbean_MesoAmericanReef_2017") {
                ref.lab <- "Bland_MesoAmericanReef_2017"
            } else {
                if (mi.ref == "Barrett_Australia_MountainSummit_2015") {
                    ref.lab <- "Barret_Australia_MountainSummit_2015"
                } else {
                    ref.lab <- mi.ref
                }
            }

            source("~/Dropbox/Provita/doc/000_BDRLE/inc01_procesarFichasHQ.R")
        }
    }
    print(mi.ref)
    cat( saveXML(doc,file=sprintf("%s/RA_%s.xml",xml.db,mi.ref),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
    
    rm(doc,top,RA,CS,AT,AU)
    gc()
}



   ##unique values for a ecosystem
    c("Assesor","Year","Assessment.type","Assessment.type.level.","Country",
      "ISO.country.region.code", "Continent", "Realm...Ecosystem.typology",
      "Realm...Sub",    "Ecosystem.distribution", "Latitude.", "Logitude"  ,
      "IUCN.Habitat", "IUCN.Habitat.Sub.category" , "IUCN.Threat.") 
    
    ## details

    c("RLE.version.","Version_info","RLE.parameter","RLE.parameter.value","Overall","RLE.bounds","RLE.criterion.", "Spatial.data..criteria.A...B.","Indicator.criteria.C.D", "Time.series.data..C.D.E.",          "Extra.information")  ##"Hyperlink.full.text." 


##Vero.DB.Asia <-read.xls("~/Descargas/Last Version RLE Database.xlsx",sheet=6)
##Vero.DB.Europe <-read.xls("~/Descargas/Last Version RLE Database.xlsx",sheet=7)
##Vero.DB.Oceania <-read.xls("~/Descargas/Last Version RLE Database.xlsx",sheet=8)

##subset(Vero.DB.Asia,Ecosystem_ID_bis %in% "Ke_Aral_KAZ_UZB_2013:1")
##subset(Vero.DB.Oceania,Ecosystem_ID_bis %in% "HolUnc_SandstoneEP_NZ_2012:47")
table(trim(gsub("[\\\\. ]+"," ",Vero.DB.todo$IUCN.Habitat)))

table(RLEDB[[1]]$reference_label[match(Vero.DB.todo$Reference_code,RLEDB[[1]]$reference_code)])


table(RLEDB[[1]]$reference_label[match(RLEDB[[2]]$reference_code,RLEDB[[1]]$reference_code)])

lista.eco.JR <- table(RLEDB[[1]]$reference_label[match(RLEDB[[2]]$reference_code,RLEDB[[1]]$reference_code)])
##lista.eco.JR <- table(RLEDB[[1]]$reference_label[match(RLEDB[[3]]$reference_code,RLEDB[[1]]$reference_code)])
lista.eco.Vero <- aggregate(Vero.DB.todo$Ecosystem_ID,
                         list(RLEDB[[1]]$reference_label[match(Vero.DB.todo$Reference_code,RLEDB[[1]]$reference_code)]),function(x) length(unique(x)))



## estan corridas los nombres de las columnas
subset(Vero.DB.Oceania,Reference_code %in% "Holdaway_Uncommon_NewZealand_2012")$IUCN.Habitat.Sub.category

subset(Vero.DB.Oceania,Reference_code %in% "Holdaway_Uncommon_NewZealand_2012")$IUCN.Threat
strsplit(as.character(subset(Vero.DB.Oceania,Reference_code %in% "Holdaway_Uncommon_NewZealand_2012")$RLE.parameter),"\n")

strsplit(as.character(subset(Vero.DB.todo,Reference_code %in% "Holdaway_Uncommon_NewZealand_2012")$IUCN.Threat.),"\n")

##OliveiraMiranda_RLE_Venezuela_2013



SpatialData <- data.frame()
for (j in 1:length(chile)) {
##for (j in 1:5) {
  SpatialData <-   rbind(SpatialData,
               data.frame(ecoName=chile@data$Piso[j],
                    crs=as.character(chile@proj4string),
                    
                    xmin=as.vector(extent(chile[j,]))[1],
                    xmax=as.vector(extent(chile[j,]))[2],
                    ymin=as.vector(extent(chile[j,]))[3],
                    ymax=as.vector(extent(chile[j,]))[4],
                    x= chile[j,]@polygons[[1]]@labpt[1],
                    y= chile[j,]@polygons[[1]]@labpt[2],
                    feature.id.column="ID",
                    feature.id.code=chile@data$ID[j],
                    feature.type=sprintf("vector with %s polygons",length(chile[j,]@polygons[[1]]@Polygons)),
                    feature.area=area(chile[j,]),
                    sourcefile="Chile_IUCN_redlist.shp",
                    file.format="shapefile"))
  
}

SpatialData2 <- data.frame()
for (k in dir("~/tmp/MGshp/MGshp","shp",full.names=T)) {
  qry <- gsub(".shp","",gsub("/home/jferrer/tmp/MGshp/MGshp/M","",k))
  if (any(grepl(qry,substr(RLEDB[[2]]$EcoName,10,12)))){
    MGshp <- shapefile(k)
    SpatialData2 <-   rbind(SpatialData2,
                 data.frame(ecoName=as.character(RLEDB[[2]]$EcoName[grep(qry,substr(RLEDB[[2]]$EcoName,10,12))]),
                       crs=as.character(MGshp@proj4string),
                       
                       xmin=as.vector(extent(MGshp))[1],
                       xmax=as.vector(extent(MGshp))[2],
                       ymin=as.vector(extent(MGshp))[3],
                       ymax=as.vector(extent(MGshp))[4],
                       x= median(unlist(lapply(MGshp@polygons,function(x) x@labpt[1]))),

                       y= median(unlist(lapply(MGshp@polygons,function(x) x@labpt[2]))),
                       feature.id.column=NA,
                       feature.id.code=NA,
                       feature.type=sprintf("vector with %s polygons",length(MGshp@polygons)),
                       feature.area=sum(area(MGshp)),
                       sourcefile=k,
                       file.format="shapefile"))
    rm(MGshp)
    gc()
  }
}





RLEDB[[2]][200,]

coordinates(SpatialData2) <- c("x","y")
proj4string(SpatialData2) <- as.character(SpatialData2@data$crs[1])
Macrogrupos.ll <- spTransform(SpatialData2,"+proj=longlat +datum=WGS84")

coordinates(SpatialData) <- c("x","y")
proj4string(SpatialData) <- chile@proj4string


SpatialData.ll <- spTransform(SpatialData,"+proj=longlat +datum=WGS84")



cs.Nick <- read.xls("~/Provita/data/RLEDB/RLECase Studies - Remote Sensing Review.xlsx")

IUCNicons <- iconList(

  CO = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|000000&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20
  ),
  CR = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|FF0000&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20
## shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
## shadowWidth = 50, shadowHeight = 64,
## shadowAnchorX = 4, shadowAnchorY = 62
),

EN = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|FFA500&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
),
VU = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|FFFF00&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
),
NT = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|ADFF2F&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
),
LC = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|008000&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
),
NE = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|C8C8C8&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
),
DD = makeIcon(
 iconUrl = "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|808080&chf=a,s,ee00FFFF",
 iconWidth = 25, iconHeight = 35,
 iconAnchorX = 20, iconAnchorY = 20,
))


markerData <- 
  rbind(data.frame(lat=coordinates(SpatialData.ll)[,2],
           lng=coordinates(SpatialData.ll)[,1],
           desc=sprintf("<b>Ecosystem type</b> %s <br/>
<b>Overall Category</b> %s<br/>
<b>Ecosystem distribution data</b> Luebert and Pliscoff (2006)<br/>
<b>Assessor</b> P. Pliscoff (2015); Ministerio del Ambiente de Chile<br/>",SpatialData.ll@data[,"ecoName"],chile@data[match(SpatialData.ll@data[,"ecoName"],chile@data$Pisos),"Final"]),
           Cat=chile@data[match(SpatialData.ll@data[,"ecoName"],chile@data$Pisos),"Final"]),
     data.frame(lat=coordinates(Macrogrupos.ll)[,2],
           lng=coordinates(Macrogrupos.ll)[,1],
           desc=sprintf("<b>Ecosystem type</b> %s <br/>
<b>Overall Category</b> %s<br/>
<b>Ecosystem distribution data</b> www.natureserve.com<br/>
<b>Assessor</b> Ferrer-Paris et al. (Submitted)<br/>",
             Macrogrupos.ll@data[,"ecoName"],
             RLEDB[[4]][match(Macrogrupos.ll@data[,"ecoName"],RLEDB[[4]]$EcoName),"RiskCat"]),
           Cat=RLEDB[[4]][match(Macrogrupos.ll@data[,"ecoName"],RLEDB[[4]]$EcoName),"RiskCat"]),

data.frame(lat=cs.Nick[,"latitude"],
      lng=cs.Nick[,"longitude"],
      desc=sprintf("<b>Ecosystem type</b> %s <br/>
<b>Overall Category</b> %s<br/>
<b>Ecosystem distribution data</b> ??<br/>
<b>Assessor</b> %s<br/>",
        cs.Nick[,"ecoName"],cs.Nick[,"riskCat"],
        cs.Nick[,"reference"]),
      Cat=substr(cs.Nick[,"riskCat"],0,2)))
        
m <- leaflet(markerData) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
    ##addTiles() %>% # Add default OpenStreetMap map tiles
    addMarkers(
         popup=markerData$desc,icon=~IUCNicons[Cat])


##m # Print the map
saveWidget(widget = m, file="/var/www/html/pruebas/Mapa.html", selfcontained = FALSE)

  logos <- "<div style='background-color:#560000;color:#F0F0F0'><a href='https://www.iucn.org/'><img src='img/iucn-white.png' height=40/></a>
<a href='https://www.iucn.org/about/union/commissions/cem/'><img src='img/cem-white.png' height=40/></a><br/><b><font size=2>IUCN Red List of Ecosystems</font></b></div>"

resumen <- sprintf("<p>
This map shows the approximate distribution of IUCN RLE assessments in the World.<br/>Each marker represents an ecosystem, with very simple descriptive data for each case.<br/> Markers are automatically clustered together. Zooming in or clicking on cluster will allow to see the single markers.</p> <p>This map includes %s ecosystem types from selected sources:<br/> Case studies from Keith et al. (2013), Chile National Assessment by Pliscoff (2015), and the continental assessment by Ferrer-Paris et al. (MS). </p>",nrow(markerData))
m <- leaflet(markerData) %>% addTiles()%>%
    addControl(logos, position = "topright") %>%
    addControl(resumen, position = "bottomleft") %>%
 addMarkers(
  popup=markerData$desc,icon=~IUCNicons[Cat],
  clusterOptions = markerClusterOptions()
)
saveWidget(widget = m, file="/var/www/html/pruebas/Mapa2.html", selfcontained = FALSE)

system("mkdir /var/www/html/pruebas/MGs")
for (kk in cdgs) {
  gc()
  k <- sprintf("/home/jferrer/tmp/MGshp/MGshp/%s.shp",kk)
  MGshp <- shapefile(k)
  object.size(MGshp)
  easy <- rmapshaper::ms_simplify(MGshp)
  object.size(easy)

  squeezy <- spTransform(easy,crs("+proj=longlat +datum=WGS84"))


  ## addAttribution("Ecosystem potential distribution by <a href='http://www.natureserve.com'>NatureServe</a>") %>%
  slc <- subset(tipologia,macrogroup_key %in% kk)
  slc$division <- subset(tipologia,Division.Code %in% slc$Division.Code & !division %in% "")$division
  slc$class <- subset(tipologia,grp %in% slc$grp & !class %in% "")$class
  slc$subclass <- subset(tipologia,sgrp %in% slc$sgrp & !subclass %in% "")$subclass
  slc$formation <- subset(tipologia,format %in% slc$format & !formation %in% "")$formation

  descripcion <- with(slc,
            sprintf("<b>%s.%s %s</b> Overall category: <b>%s</b> <br/>
<p>Ecosystem classification according to International Vegetation Classification:<br/> Class %s / Subclass %s <br/> Formation %s / Division <a href='../Division/%s.html'>%s</a><br/>Potential distribution map modified from <a href='http://www.natureserve.com'>NatureServe</a></p>
IUCN RLE Assessment by Ferrer-Paris et al. (MS) in 2017.
<table>
<tr><th fgcolor='#FFFFFF' bgcolor='#000000'></th><th fgcolor='#FFFFFF' bgcolor='#000000'>A</th><th fgcolor='#FFFFFF' bgcolor='#000000'>B</th><th fgcolor='#FFFFFF' bgcolor='#000000'>C</th><th fgcolor='#FFFFFF' bgcolor='#000000'>D</th><th fgcolor='#FFFFFF' bgcolor='#000000'>E</th></tr>
<tr><th fgcolor='#FFFFFF' bgcolor='#000000'>1</th><td></td><td>%s</td><td></td><td>%s</td><td></td></tr>
<tr><th fgcolor='#FFFFFF' bgcolor='#000000'>2</th><td>%s</td><td>%s</td><td></td><td></td><td></td></tr>
<tr><th fgcolor='#FFFFFF' bgcolor='#000000'>3</th><td>%s</td><td></td><td></td><td></td><td></td></tr>
</table>
According to criteria and guidelines in <a href='https://portals.iucn.org/library/node/45794'>Bland et al. 2017</a>",
                Division.Code,
                gsub("M","",macrogroup_key),
                macrogroup_name,
                subset(d.todo,mcdg %in% kk)$Cat,

                class, subclass, formation,gsub(" ","_",Division.Code),division,
                subset(d.todo,mcdg %in% kk)$B1,
                subset(d.todo,mcdg %in% kk)$D,
                subset(d.todo,mcdg %in% kk)$A2.mean,
                subset(d.todo,mcdg %in% kk)$B2,
                subset(d.todo,mcdg %in% kk)$A3.mean
))


 logos <- "<div style='background-color:#560000;color:#F0F0F0'><a href='https://www.iucn.org/'><img src='../img/iucn-white.png' height=40/></a>
<a href='https://www.iucn.org/about/union/commissions/cem/'><img src='../img/cem-white.png' height=40/></a><br/><b><font size=2>IUCN Red List of Ecosystems</font></b></div>"

  
  h <- leaflet(squeezy) %>%
    addControl(descripcion, position = "bottomleft") %>%
    addControl(logos, position = "topright") %>%
      addMiniMap(toggleDisplay = TRUE, position = "bottomleft")  %>%
        addProviderTiles(providers$Stamen.Toner) %>%
          addPolygons(color = "#049424", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5
                ) 

  saveWidget(widget = h, file=sprintf("/var/www/html/pruebas/MGs/%s.html",kk), selfcontained = FALSE)
  rm(easy,squeezy,MGshp,h)
  print(kk)
}


##%>% addPopups(median(coordinates(squeezy)[,1]),
##     median(coordinates(squeezy)[,2]),
##     "<b>Macrogroup M873</b><br/>Assessment by Ferrer-Paris et al. (MS) in 2017.",
##     options = popupOptions(closeButton = TRUE))
  
require(xml2)
system("mkdir /var/www/html/pruebas/Division")
for (k in unique(subset(tipologia,macrogroup_key %in% cdgs)$Division.Code)) {
  slc <- subset(tipologia,Division.Code %in% k)


  h <- read_html(sprintf("
<h1>IUCN Red List of Ecosystem Assessment</h1>
Continental scale assessment of Forest Macrogroups (Ferrer-Paris et al. MS)

<h3>International Vegetation Clasification</h3>
<h3>Class %s </h3>
<h3>Subclass %s </h3>
<h3>Formation <a href='../Formation/%s.html'>%s</a> </h3>
<h2>Division %s</h2><ul>%s</ul>",
              subset(tipologia,grp %in% subset(slc,!division %in% "")$grp & !class %in% "")$class,
              subset(tipologia,sgrp %in% subset(slc,!division %in% "")$sgrp & !subclass %in% "")$subclass,
              subset(tipologia,format %in% subset(slc,!division %in% "")$format & !formation %in% "")$format,
              subset(tipologia,format %in% subset(slc,!division %in% "")$format & !formation %in% "")$formation,
              
              subset(slc,!division %in% "")$division,
              paste(sprintf("<li> <a href='../MGs/%1$s.html'>%1$s %2$s</a> <b>%3$s</b></li>",
                     unique(subset(slc,macrogroup_key %in% cdgs)$macrogroup_key),
                     unique(subset(slc,macrogroup_key %in% cdgs)$macrogroup_name),
                     d.todo[match(unique(subset(slc,macrogroup_key %in% cdgs)$macrogroup_key),d.todo$mcdg),"Cat"]),sep="",collapse="")
                ))

  tmp <- sprintf("/var/www/html/pruebas/Division/%s.html",gsub(" ","_",k))
  write_html(h, tmp, options = "format")

}

system("mkdir -p /var/www/html/pruebas/Formation")
for (k in unique(subset(tipologia,macrogroup_key %in% cdgs)$format)) {
  slc <- subset(tipologia,format %in% k)


  h <- read_html(sprintf("
<h1>IUCN Red List of Ecosystem Assessment</h1>
Continental scale assessment of Forest Macrogroups (Ferrer-Paris et al. MS)

<h3>International Vegetation Clasification</h3>
<h3>Class %s </h3>
<h3>Subclass %s </h3>
<h2>Formation %s </h2>
<ul>%s</ul>",
              subset(tipologia,grp %in% subset(slc,!division %in% "")$grp & !class %in% "")$class,
              subset(tipologia,sgrp %in% subset(slc,!division %in% "")$sgrp & !subclass %in% "")$subclass,
              subset(tipologia,format %in% subset(slc,!division %in% "")$format & !formation %in% "")$formation,
              
              paste(sprintf("<li> <a href='../Division/%1$s.html'>%2$s</a> </li>",
                     unique(subset(slc,Division.Code %in% d.todo$dvs & !division %in% "")$Division.Code),
                     unique(subset(slc,Division.Code %in% d.todo$dvs & !division %in% "")$division)),sep="",collapse="")
                 ))
  
          
  tmp <- sprintf("/var/www/html/pruebas/Formation/%s.html",gsub(" ","_",k))
  write_html(h, tmp, options = "format")

}






################
## Colombia
###############
colombia <- shapefile("~/respaldoAmericasDataFiles/EcoOri_12052015_2014_TodosCriterios.shp")

SpatialData3 <- data.frame()
for (j in 1:length(colombia)) {
##for (j in 1:5) {
  SpatialData3 <-   rbind(SpatialData3,
               data.frame(ecoName=colombia@data$COD[j],
                    crs=as.character(colombia@proj4string),
                    
                    xmin=as.vector(extent(colombia[j,]))[1],
                    xmax=as.vector(extent(colombia[j,]))[2],
                    ymin=as.vector(extent(colombia[j,]))[3],
                    ymax=as.vector(extent(colombia[j,]))[4],
                    x= colombia[j,]@polygons[[1]]@labpt[1],
                    y= colombia[j,]@polygons[[1]]@labpt[2],
                    feature.id.column="COD",
                    feature.id.code=colombia@data$COD[j],
                    feature.type=sprintf("vector with %s polygons",length(colombia[j,]@polygons[[1]]@Polygons)),
                    feature.area=area(colombia[j,]),
                    sourcefile="EcoOri_12052015_2014_TodosCriterios.shp",
                    file.format="shapefile"))
  
}


coordinates(SpatialData3) <- c("x","y")
proj4string(SpatialData3) <- colombia@proj4string


Colombia.ll <- spTransform(SpatialData3,"+proj=longlat +datum=WGS84")


     data.frame(lat=coordinates(Colombia.ll)[,2],
           lng=coordinates(Colombia.ll)[,1],
           desc=sprintf("<b>Ecosystem type</b> %s <br/>
<b>Overall Category</b> %s<br/>
<b>Ecosystem distribution data</b> Etter et al. (2015)<br/>
<b>Assessor</b> Etter et al. (2015)<br/>",Colombia.ll@data[,"ecoName"],colombia@data[match(Colombia.ll@data[,"ecoName"],colombia@data$COD),"EvFinal"]),
           Cat=colombia@data[match(Colombia.ll@data[,"ecoName"],colombia@data$COD),"EvFinal"]),




zip -r IVC.zip MGs Division/ Formation/


#########
###FTP
##files.000webhost.com
##hylophagous-destina
##passwd 

tabla.pais1 <- read.table("tabSAM/Paises_SAMv7pot.txt")
tabla.pais2 <- read.table("tabNAC/Paises_NACv5pot.txt")

tabla.paises <- rbind(tabla.pais1,tabla.pais2)
tabla.paises <- subset(tabla.paises,!V1 %in% "*")
ds <- data.frame()
for (k in unique(tabla.paises$V2)) {
  if (k %in% paises$OBJECTID) {
    slc <- unique(sprintf("M%03d",
               as.numeric(as.character(subset(tabla.paises,V2 %in% k)$V1))))
    d <- data.frame(mcdg=slc)
    
    d <- merge(d, subset(d.todo,mcdg %in% slc)[,c("mcdg","Cat")],by="mcdg",all.x=T)
    d[d$mcdg %in% tipologia$macrogroup_key & is.na(d$Cat),"Cat"] <- "NE"
    d <- subset(d,!is.na(Cat))
    d$Pais <- subset(paises,OBJECTID %in% k)$NAME
    
    ds <- rbind(ds,d)
  }
}


tbl <- table(droplevels(ds$Pais),factor(ds$Cat,levels=c("NE","DD","LC","NT","VU","EN","CR","CO")))



ds$Pais <- factor(ds$Pais,levels=rownames(tbl)[order(rowSums(tbl))])
ds$Cat <- factor(ds$Cat,levels=c("NE","DD","LC","NT","VU","EN","CR","CO"))
##d <- Tcs[,1:2]
##d$o <- nrow(Tcs)-rank(Tcs$n)

barplot(t(tbl[rev(order(rowSums(tbl))),]),col=rev(clrs.UICN),las=2)
require(ggplot2)

library(plotly)

##http://ggplot2.tidyverse.org/reference/geom_bar.html
##http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

g <- ggplot(ds, aes(Pais))
# Number of cars in each class:
p=g + geom_bar(aes(fill = Cat), position = position_stack(reverse = TRUE)) + scale_fill_manual(values=clrs.UICN)+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

q <- ggplotly(p)
htmlwidgets::saveWidget(q, "/var/www/html/pruebas/Paises.html",selfcontained=F)


#############
## referencias/páginas XML
##https://www.altova.com/mobiletogether/xpath-intro
##http://blogs.plos.org/tech/how-to-find-an-appropriate-research-data-repository/
##https://en.wikipedia.org/wiki/Data_modeling
##https://en.wikipedia.org/wiki/Software_prototyping#Throwaway_prototyping
##https://nativexmldatabase.com/2010/08/22/xml-versus-relational-database-performance/
##http://www.rpbourret.com/xml/XMLAndDatabases.htm
##https://www.xml.com/pub/a/2001/10/31/nativexmldb.html
##http://phobos103.inf.uni-konstanz.de/xml15/
##http://www.webopedia.com/TERM/D/data_flow_modeling.html
##https://developer.marklogic.com/learn/data-modeling
##https://developer.marklogic.com/learn/get-started-apps


##########
## usar el label en las referencias para identificar los documentos y asociar los assessments a esta label, luego sacar la lista de assessments por cada documento.



    
for $v in //record/label
let $i := //case-study[ref-label=$v]
return  $i

for $v in //record/label
let $i := //Case-Study[ref-label=$v]
order by $v
return  ("|",$v/text(),"||",count($i//ecosystem-id),"|
")






require(BaseXClient)
BaseXSession(host = "localhost", port = 1984)

xmlRepo <- "~/Dropbox/Provita/xml/RLE_test1/"
doc <- xmlTreeParse(sprintf("%s/ecosystems_EU_prueba.xml",xmlRepo))

r <- xmlRoot(doc)
length(r)

xmlSApply(r[[2]], xmlName)
sort(xmlSApply(r, xmlAttrs))
