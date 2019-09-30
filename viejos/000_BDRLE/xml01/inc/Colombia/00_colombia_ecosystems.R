doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

##"Etter_RLE_Colombia_2015"
lista <- subset(RLEDB[[2]],reference_code %in% "Etter_RLE_Colombia_2015")

lista[grep(colombia@data$COD[2],lista$EcoName),]
colombia@data[2,]

##for (k in 1:nrow(chile@data)) {
for (k in 1:nrow(colombia@data)) {
    lk <- lista[grep(colombia@data$COD[k],lista$EcoName),]
 
    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=lk$ecosystem_ID),
        parent=top,
        children = c(newXMLNode("name", lk$EcoName,attrs=list(lang="es")),
            newXMLNode("system", lk$Realm),
            newXMLNode("description", "adopted from Etter ???"),
            newXMLNode("native-biota", "adopted from Etter ???"),
            newXMLNode("abiotic-environment", "adopted from Etter ???"),
            newXMLNode("processes-interactions", "adopted from Etter ???"),
            newXMLNode("ecosystem-services", "adopted from Etter ???"),
            newXMLNode("comments", "")
                     ))
    

    ##Ecosystem classification
    b = newXMLNode("ecosystem-classification",attrs=list(name="Etter ???",version="2006",selected="yes"),
        children=c(
        newXMLNode("scheme",sprintf("Formaci&oacute;n %s; Piso %s",chile@data$Formacion[k],chile@data$Pisos[k])),
        newXMLNode("name",chile@data$Pisos[k]),
        newXMLNode("scale","national"),
        newXMLNode("reference","Etter ??? (2006)"),
        newXMLNode("assigned-by","A Etter")
             ))
    a <- addChildren(a, b)
    if (grepl("Bosque",chile@data$Formacion[k])) {
        d = newXMLNode("ecosystem-classification",attrs=list(name="IUCN",version="?",selected="no"),
        children=c(
        newXMLNode("scheme","1. Forest"),
        newXMLNode("name",chile@data$Pisos[k]),
        newXMLNode("scale","global"),
        newXMLNode("reference",""),
        newXMLNode("url",""),
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

cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/ecosystems_chile_prueba.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
