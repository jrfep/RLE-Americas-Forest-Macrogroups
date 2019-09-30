

subset(b.pres,pais==54)
subset(b.hist,pais==54)

sh <- subset(b.hist,pais==54 & mcdg %in% "M563")
sp <- subset(b.pres,pais==54 & mcdg %in% "M563" )

        

critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
               children=c(
                   newXMLNode("key-indicator-variable","forest cover"),
                   newXMLNode("key-indicator-variable","woodland distribution"),
                   newXMLNode("indicator-data","remote sensor"),
                   newXMLNode("indicator-data","cartographic reconstruction"),
                   newXMLNode("data-source","Modis"),
                   newXMLNode("data-source","GFC2015"),
                   newXMLNode("data-source","Anthromes"),
                   newXMLNode("method-of-measurement","Spatio-temporal analysis")))

        source(sprintf("%s/%s/inc30_subcritA1.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc31_subcritA2a.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc32_subcritA2b.R",mi.path,mi.dir))
        source(sprintf("%s/%s/inc33_subcritA3.R",mi.path,mi.dir))


critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="B",version="RLE 2.0"),
               children=c(
                   newXMLNode("key-indicator-variable","forest cover"),
                   newXMLNode("indicator-data","remote sensor"),
                   newXMLNode("data-source","Modis"),
                   newXMLNode("data-source","GFC2015"),
                   newXMLNode("method-of-measurement","Spatial analysis")))


