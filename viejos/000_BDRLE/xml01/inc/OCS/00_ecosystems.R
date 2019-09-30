doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

a = newXMLNode("ecosystem",attrs=list(id="Rodriguez_RLE_Examples_2007_1"),parent=top,
    children = c(newXMLNode("name", "Kalimantan lowland forests",attrs=list(lang="en")),
        newXMLNode("system", "Terrestrial"),
        ##Ecosystem classification
        newXMLNode("ecosystem-classification",attrs=list(name="IUCN",version="?",selected="no"),
                   children=c(
                       newXMLNode("scheme","1. Forest"),
                       newXMLNode("scale","global"),
                       newXMLNode("reference",""),
                       newXMLNode("url",""),
                       newXMLNode("assigned-by","JRFP")
                   )),
##Distribution
        newXMLNode("distribution",attrs=list(summary="Indonesian Borneo"),
                   children=c(
                       newXMLNode("description",""),
                       newXMLNode("biogeographic-realm","Paleotropic",attrs=list(id="00?/PT")),
                       newXMLNode("continent","Asia",attrs=list(id="As")),
                       newXMLNode("country","Indonesia",attrs=list(id="ID"))
                   ))))



a = newXMLNode("ecosystem",attrs=list(id="Rodriguez_RLE_Examples_2007_2"),parent=top,
    children = c(newXMLNode("name", "Brazil's Atlantic Forest",attrs=list(lang="en")),
        newXMLNode("system", "Terrestrial"),
        ##Ecosystem classification
        newXMLNode("ecosystem-classification",attrs=list(name="IUCN",version="?",selected="no"),
                   children=c(
                       newXMLNode("scheme","1. Forest"),
                       newXMLNode("scale","global"),
                       newXMLNode("reference",""),
                       newXMLNode("url",""),
                       newXMLNode("assigned-by","JRFP")
                   )),
##Distribution
        newXMLNode("distribution",attrs=list(summary="Brazil"),
                   children=c(
                       newXMLNode("description",""),
                       newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="00?/PT")),
                       newXMLNode("continent","America",attrs=list(id="Am")),
                       newXMLNode("country","Brazil",attrs=list(id="BR"))
                   ))))


cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/ecosystems_OCS_prueba.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
