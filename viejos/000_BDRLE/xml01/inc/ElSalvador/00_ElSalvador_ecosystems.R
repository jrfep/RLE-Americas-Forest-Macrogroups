doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

##"En el caso de El Salvador


for (k in 1:nrow(el.salvador)) {
    mid <- el.salvador[k,"ecosystem_ID"]
    g <- match(mid,ES.info$ecosystem_ID)
    ss <- subset(ES.2,Ecosystem_ID_bis %in% sub("Crespin_RLE_ElSalvador_2015","Crespin_ElSalvador_SLV_2015",mid))
    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", el.salvador$EcoName[k],attrs=list(lang="es")),
              newXMLNode("system",el.salvador$Realm[k] ),
              newXMLNode("description-source", "published national classification"),
              
              newXMLNode("comments", "Original information from Vreugdenhil et al. (2012).")
              ))

    cc1 <- c()
    cc2 <- c()
    
    if (!is.na(ES.info[g,"Vegetation"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "vegetation type"))
        cc2 <- c(cc2,newXMLNode("Vegetation", ES.info[g,"Vegetation"],attrs=list(lang="en")))
    }
    if (!is.na(ES.info[g,"Geology"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "geology"))
        cc2 <- c(cc2,newXMLNode("Geology", ES.info[g,"Geology"],attrs=list(lang="en")))
    }
    if (!is.na(ES.info[g,"Characteristic biota"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "characteristic biota"))
        cc2 <- c(cc2,newXMLNode("Characteristic-biota", ES.info[g,"Characteristic biota"],attrs=list(lang="en")))
   }
    if (!is.na(ES.info[g,"Topography"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "topography"))
        cc2 <- c(cc2,newXMLNode("Topography", ES.info[g,"Topography"],attrs=list(lang="en")))
   }
    if (!is.na(ES.info[g,"Hydrology"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "hydric regime"))
        cc2 <- c(cc2,newXMLNode("Hydric-regime", ES.info[g,"Hydrology"],attrs=list(lang="en")))
    }
    if (!is.na(ES.info[g,"Soil"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "soil"))
        cc2 <- c(cc2,newXMLNode("Soil", ES.info[g,"Soil"],attrs=list(lang="en")))
    }
    if (!is.na(ES.info[g,"Definition"])) {
        cc2 <- c(cc2,newXMLNode("Description-notes", ES.info[g,"Definition"],attrs=list(lang="en")))
        
    }
    if (!is.null(cc1))
        a <- addChildren(a, cc1)
    if (!is.null(cc2))
        a <- addChildren(a, cc2)

    b = newXMLNode("ecosystem-classification",attrs=list(id="Vreugdenhil",version="2012",selected="yes"),
        children=c(
            newXMLNode("classification-system","Ecosistemas de El Salvador"),
            newXMLNode("level0",ES.info[g,"Ecosystem"]),
            newXMLNode("scale","national"),
            newXMLNode("notes","Ecosystems were defined after the United Nations Educational, Scientific and Cultural Organi- zation classification system (UNESCO 1973) which relies on vegetation structure and physiognomy, elevation and hydric regime."),
            
            newXMLNode("reference","Vreugdenhil D., Linares J., Komar O., Henríquez V. E., Barraza J. E. & Machado M. (2012) Mapa de los ecosistemas de El Salvador, actualización 2012 con detección de cambios 1999 – 2011. World Institute for Conservation and Environment, Shepherdstown, USA.")
        ))
    a <- addChildren(a, b)

    
    d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="3.1",selected="no"),
        children=c(
            newXMLNode("classification-system","IUCN Habitats Classification Scheme"),
            newXMLNode("level1",unique(ss[,"IUCN.Habitat.Sub.category"]),attrs=list(name="level 1")),
            newXMLNode("level0",unique(ss[,"IUCN.Habitat"]),attrs=list(name="level 0")),
            newXMLNode("scale","global"),
            newXMLNode("reference","IUCN Habitats Classification Scheme"),
            newXMLNode("version","3.1"),
            newXMLNode("url","http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3"),
            newXMLNode("assigned-by","Jess Rowland")
        ))
        
        a <- addChildren(a, d)

    ##Distribution
    d = newXMLNode("distribution",attrs=list(summary="Ecosystem type described for El Salvador, description needs to be homologued with neighboring countries"),
        children=c(
            newXMLNode("description",ES.info[g,"Distribution"]),
            newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="006/NT")),
            newXMLNode("continent","America",attrs=list(id="Am")),
            newXMLNode("country","El Salvador",attrs=list(id="SLV"))
        ))
    a <- addChildren(a, d)

    ##Delimitation
    d = newXMLNode("delimitation",attrs=list(summary="Delimitation is based on supervised classification of remote sensed imagery with ground verification"),
        children=c(
            newXMLNode("methods","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. ... Historic distri- bution for ecosystems refers to their potential distributions, based on a combination of criteria for current physiognomy at different elevations obtained from a digital elevation model (DEM) generated from a mosaic of the 2011 ASTER tiles.. (pag. 493)"),
            newXMLNode("input-data","ASTER tiles"),
            newXMLNode("input-data","Digital Elevation Model"),
            newXMLNode("data-source","Ministry of Environment and Natural Resources (El Salvador)"),
            newXMLNode("validation","Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups.")

        ))
    a <- addChildren(a, d)

}

cat( saveXML(doc,file=sprintf("%s/ecosystems_ElSalvador.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
