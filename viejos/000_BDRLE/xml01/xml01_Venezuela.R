subset(RLEDB[[5]],reference_code %in% "OliveiraMiranda_RLE_Venezuela_2010")

subset(RLEDB[[2]],reference_code %in% "OliveiraMiranda_RLE_Venezuela_2010")

lista.eco <- unique(subset(Vero.DB.todo,Reference_code %in% "OliveiraMiranda_RLE_Venezuela_2010")[,c(1,3,5:8,15,20:21)])
lista.eco[21,"EcoName_original"] <- "Vegetación saxícola"
lista.eco[11,"EcoName_original"] <- "Arbustales ribereños"

doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

j <- 0
for (k in unique(trim(lista.eco$EcoName_original))) {
    j <- j+1
    mid <- sprintf("OliveiraMiranda_RLE_Venezuela_2010:%s",j)
    kk <- subset(lista.eco,EcoName_original %in% k)
  ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", k,attrs=list(lang="es")),
              newXMLNode("name", trim(unique(kk$EcoName)),attrs=list(lang="en")),
              newXMLNode("system", "Terrestrial",
                         newXMLNode("comments", "")
                         ))
        )
        ##Ecosystem classification
    
    d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="?",selected="no"),
        children=c(
            newXMLNode("classification-system","IUCN"),
            newXMLNode("assigned-by","Verónica Ruíz")),
        parent=a)
    
    for (hh in unique(kk$IUCN.Habitat)) {
        newXMLNode("level1",hh,
                   attrs=list(name="level"),parent=d)
    }
    for (hh in unique(kk$IUCN.Habitat.Sub.category)) {
        if (hh != "-")
            newXMLNode("level2",hh,
                       attrs=list(name="level"),parent=d)
    }
}
    
cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/ecosystem_Venezuela.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
