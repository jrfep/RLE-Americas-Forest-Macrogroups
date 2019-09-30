doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

##"En el caso de Costa Rica


for (k in 1:nrow(costa.rica)) {
    mid <- costa.rica[k,"ecosystem_ID"]
    g <- match(mid,CR.info$ecosystem_ID)
    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", costa.rica$EcoName[k],attrs=list(lang="es")),
              newXMLNode("system",costa.rica$Realm[k] ),
              newXMLNode("description-source", "published national classification"),
              
              newXMLNode("comments", "Original information from Zamora (2008) updated and improved in 2014.")
              ))

    cc1 <- c(newXMLNode("description-based-on", "vegetation type"))
    cc2 <- c()
    if (!is.na(CR.info[g,"geología"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "geology"))
        cc2 <- c(cc2,newXMLNode("Geology", CR.info[g,"geología"],attrs=list(lang="es")))
    }
    if (!is.na(CR.info[g,"Biota característica"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "characteristic biota"))
        cc2 <- c(cc2,newXMLNode("Characteristic-biota", CR.info[g,"Biota característica"],attrs=list(lang="es")))
   }
    if (!is.na(CR.info[g,"topografía"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "topography"))
        cc2 <- c(cc2,newXMLNode("Topography", CR.info[g,"topografía"],attrs=list(lang="es")))
   }
    if (!is.na(CR.info[g,"clima"])) {
        cc1 <- c(cc1,newXMLNode("description-based-on", "climate"))
        cc2 <- c(cc2,newXMLNode("Climate", CR.info[g,"clima"],attrs=list(lang="es")))
  
    }
    if (!is.na(CR.info[g,"DESCRIPCION DIAGNÓSTICA"])) {
        cc2 <- c(cc2,newXMLNode("Description-notes", CR.info[g,"DESCRIPCION DIAGNÓSTICA"],attrs=list(lang="es")))
        
    }
    a <- addChildren(a, cc1)
    if (!is.null(cc2))
        a <- addChildren(a, cc2)

    b = newXMLNode("ecosystem-classification",attrs=list(id="Zamora",version="2008",selected="yes"),
        children=c(
            newXMLNode("classification-system","Unidades Fitogeográficas de Costa Rica"),
            newXMLNode("level1",CR.info[g,"SUBUNIDAD"],attrs=list(name="Subunidad",code=CR.info[g,"cdg_subunidad"])),
            newXMLNode("level0",CR.info[g,"Unidad"],attrs=list(name="Unidad",code=CR.info[g,"cdg_unidad"])), 
            newXMLNode("scale","national"),
            newXMLNode("reference","Zamora (2008)"),
            newXMLNode("assigned-by","B Herrera-F")
        ))
    a <- addChildren(a, b)

    
    d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="?",selected="no"),
        children=c(
            newXMLNode("classification-system","IUCN"),
            newXMLNode("level1",CR.info[g,"IUCN.habitat.Subcategory"],attrs=list(name="level 1")),
            newXMLNode("level0",CR.info[g,"IUCN.habitat"],attrs=list(name="level 0")),
            newXMLNode("scale","global"),
            newXMLNode("reference",""),
            newXMLNode("url",""),
            newXMLNode("assigned-by","JRFP")
        ))
        
        a <- addChildren(a, d)

    ##Distribution
    d = newXMLNode("distribution",attrs=list(summary="Ecosystem type described for Costa Rica, description needs to be homologued with neighboring countries"),
        children=c(
            newXMLNode("description",CR.info[g,"Distribution"]),
            newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="006/NT")),
            newXMLNode("continent","America",attrs=list(id="Am")),
            newXMLNode("country","Costa Rica",attrs=list(id="CR"))
        ))
    a <- addChildren(a, d)

}

cat( saveXML(doc,file=sprintf("%s/ecosystems_CostaRica.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
