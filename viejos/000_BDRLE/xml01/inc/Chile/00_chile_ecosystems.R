doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

##"En el caso de Chile, se ha utilizado la clasificación de pisos de vegetación (Luebert & Pliscoff, 2006) para realizar este primer ejercicio, ya que cuenta con una cobertura a escala nacional. Es decir, caracterizó todo Chile Continental en unidades ecosistémicas según un sistema de clasificación único, a una escala aproximada 1:100.000. Estas unidades están basadas en elementos físicos como el clima, el relieve y la composición botánica, lo que permite la identificación de distintas unidades con límites definidos. (pág 3)"

##for (k in 1:nrow(chile@data)) {
for (k in 1:nrow(chile@data)) {
    mid <- chile.P$ecosystem_ID[match(chile@data$Pisos[k],chile.P$EcoName)]
    ## ecosystem description
    HQ.ss <- subset(HQ.DB,Ecosystem_ID_bis %in% mid)
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", chile@data$Pisos[k],attrs=list(lang="es")),
              newXMLNode("system", "Terrestrial"),
              newXMLNode("description-based-on", "climate"),
              newXMLNode("description-based-on", "topography"),##relieve
              newXMLNode("description-based-on", "characteristic biota"),
              newXMLNode("description-source", "published national classification"),
              
              newXMLNode("comments", "All information from Luebert and Pliscoff")
              ))

    if (mid %in% chile.A$ecoid) {
        newXMLNode("name", subset(chile.A,ecoid %in% mid)$EcoName,attrs=list(lang="en"),parent=a)
    }

    ##Ecosystem classification
    b = newXMLNode("ecosystem-classification",attrs=list(id="LP",version="2006",selected="yes"),
        children=c(
            newXMLNode("classification-system","Vegetación de Chile"),
            newXMLNode("level1",chile@data$Formacion[k],attrs=list(name="Formación"), children=c(newXMLNode("level0",chile@data$Pisos[k],attrs=list(name="Piso")))), 
            newXMLNode("scale","national"),
            newXMLNode("reference","Luebert and Pliscoff (2006)"),
            newXMLNode("assigned-by","P Pliscoff")
        ))
    a <- addChildren(a, b)

    ## Macrogroups...
    lk <- ChileMGs[agrep(chile@data$Pisos[k],ChileMGs$PISO),]
    if (nrow(lk)==1) {
        b = newXMLNode("ecosystem-classification",attrs=list(id="IVC",version="2014",selected="no"),
            children=c(
                newXMLNode("classification-system","International Vegetation Classification"),
                newXMLNode("level5",lk$Formation,attrs=list(name="Class"),
                           children=c(
                               newXMLNode("level4",lk$Formation,attrs=list(name="Subclass"),
                                          children=c(
                                              newXMLNode("level3",lk$Formation,attrs=list(name="Formation"),
                                                         children=c(
                                                             newXMLNode("level2",sprintf("%s %s",lk$Division.code,lk$Division.Name),attrs=list(name="Division"),
                                                                        children=c(
                                                                            newXMLNode("level1",sprintf("%s %s",lk$Macrogroup.code,lk$MG.Name),attrs=list(name="Macrogroup"),
                                                                                       children=c(
                                                                                           newXMLNode("level0",lk$Ecological.System,attrs=list(name="Ecological System")))))))))))), 
                               newXMLNode("scale","global"),
                               newXMLNode("reference",""),
                               newXMLNode("assigned-by","NatureServe")))
        a <- addChildren(a, b)
    }

    ##match IUCN habitat  de HQ.ss...
    if (grepl("Bosque",chile@data$Formacion[k])) {
        d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="3.1",selected="no"),
        children=c(
            newXMLNode("classification-system","IUCN Habitats Classification Scheme"),
            newXMLNode("level0","1. Forest",attrs=list(name="level")),
            newXMLNode("scale","global"),
            newXMLNode("reference","IUCN Habitats Classification Scheme"),
            newXMLNode("version","3.1"),
            newXMLNode("url","http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3"),
            newXMLNode("assigned-by","JRFP")
        ))
        a <- addChildren(a, d)
    }
    
##Distribution
    d = newXMLNode("distribution",attrs=list(summary="Ecosystem type described for Chile, description needs to be homologued with neighboring countries"),
        children=c(
            newXMLNode("description",""),
            newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="006/NT")),
            newXMLNode("continent","America",attrs=list(id="Am")),
            newXMLNode("country","Chile",attrs=list(id="CL"))
        ))
    a <- addChildren(a, d)
}

cat( saveXML(doc,file=sprintf("%s/ecosystems_Chile.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
