doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

## very basic information on each ecosystem in the list:
lista.eco <- RLEDB[[2]]

lista.eco <- subset(lista.eco,!reference_code %in% c("Alaniz_Chile_CentralChileHotspot_2016","MAC_RLE_Chile_2015","OliveiraMiranda_RLE_Venezuela_2010"))

##for (k in 1:nrow(lista.eco)) {
for (k in 1:nrow(lista.eco)) {
    mid <- lista.eco$ecosystem_ID[k]
    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", lista.eco$EcoName[k],attrs=list(lang="??")),
              newXMLNode("system", lista.eco$Realm[k]),
              newXMLNode("comments", "")
              ))

    ##Ecosystem classification
    
    if (!is.na(lista.eco$IUCN_Habitat_level_1[k])) {
        d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="?",selected="no"),
            children=c(
                newXMLNode("classification-system","IUCN"),
                newXMLNode("level1",lista.eco$IUCN_Habitat_level_1[k],
                           attrs=list(name="level")),
                newXMLNode("assigned-by","JRFP; Verónica Ruíz; Jessica Rowland")
            ))
        if (!is.na(lista.eco$IUCN_Habitat_level_2[k])) {
            newXMLNode("level2",lista.eco$IUCN_Habitat_level_2[k],
                       attrs=list(name="level"),parent=d)
        }
        a <- addChildren(a, d)
    }

    
##Distribution

    if (!is.na(lista.eco$Longitude[k])) {
        
        d <- newXMLNode("distribution",
                        children=c(
                            newXMLNode("latitude",lista.eco$Latitude[k]),
                            newXMLNode("longitude",lista.eco$Longitude[k])),
                        parent=a)
    }
        
    ##            newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="006/NT")),
    ##            newXMLNode("continent","America",attrs=list(id="Am")),
    ##            newXMLNode("country","Chile",attrs=list(id="CL"))
    ##        ))
    ##    a <- addChildren(a, d)
}

cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/initial_ecosystem_list.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
