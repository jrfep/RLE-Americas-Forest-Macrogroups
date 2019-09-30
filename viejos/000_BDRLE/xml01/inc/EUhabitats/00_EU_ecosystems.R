
doc = newXMLDoc()
top = newXMLNode("ecosystems",doc=doc)

for (k in 1:nrow(EU.RLE)) {    
    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=sprintf("%s_%s","EU_RLE",EU.RLE$Habitat.ID[k])),parent=top, children = c(newXMLNode("name", gsub("<[/p]+>","",EU.RLE[k,"Habitat.Type.Name"]),attrs=list(lang="en")),
        newXMLNode("system", ifelse(EU.RLE$Habitat.Group %in% c("Freshwater","Marine"),as.character(EU.RLE$Habitat.Group),"Terrestrial")[k]),
                                                                                                             newXMLNode("description", as.character(EU.RLE$Habitat.Description[k]),
                                                                                                                        attrs=list(source="EU.RLE.database")),
        newXMLNode("native-biota", "see Characteristic species in description"),
        newXMLNode("abiotic-environment", "???"),
        newXMLNode("processes-interactions", "???"),
        newXMLNode("ecosystem-services", "???"),
        newXMLNode("comments", "")
             ))

    euclass <- gsub("  "," ",gsub("<br />","~~",
                    gsub("&lsquo;","",
                         gsub("Annex","~~ Annex",
                         gsub("Checklist"," Checklist",
                   gsub("<em>","",
                         gsub("Euro[Vv]eg[Cc ]+[he]+ck[Ll]ist","EuroVeg Checklist",
                              gsub("&rsquo;","",
                                   gsub("&nbsp;"," ",
                                        gsub(":","~~",
                                             gsub("<[/strong]+>","~~",
                                                  gsub("<[/p]+>","~~",
                                                       as.character(EU.RLE[k,"Classification"])))))))))))))
    clases <- trim(strsplit(euclass,"~~")[[1]])
    clases <- clases[!clases==""]
    
    kk <- c(grep("EUNIS",clases)[1],grep("Annex 1",clases)[1],
            grep("EUSeaMap",clases)[1],grep("MSFD",clases)[1],grep("Barcelona Convention",clases)[1],
            grep("Emerald",clases)[1],grep("MAES",clases)[1],grep("IUCN",clases)[1],
            grep("euroveg",tolower(clases))[1],
            grep("Other relationships",clases)[1],
            length(clases)+1)
    kk <- kk[!is.na(kk)]
    kk <- sort(kk)
    ## para algunos habitat no están identificados con ':' habra que agregarlos manualmente
    if (length(kk)>1) { 
        for (j in 1:(length(kk)-1)) {
            b = newXMLNode("ecosystem-classification",attrs=list(id=gsub(":","",clases[kk[j]]),selected="no"),
                children=c(
                    newXMLNode("classification-system",
                               trim(gsub("relationships","",
                                         gsub("[\\(Aa]+ll[io]anc[es\\)]+","",gsub(":","",clases[kk[j]]))))),
                    
                    newXMLNode("level1",paste(clases[ (kk[j]+1):(kk[j+1]-1)],collapse=", ")),
                    newXMLNode("assigned-by","assessment authors")
                ))
            a <- addChildren(a, b)
        }
    }

##Distribution
    d = newXMLNode("distribution",attrs=list(summary="Ecosystem type described for the EU28+ region"),
        children=c(
            newXMLNode("description",EU.RLE$Justification.Outstanding.Example.of.Biogeographical.Region[k],attrs=list(source="EU.RLE.database")),
            newXMLNode("biogeographic-realm","Palearctic",attrs=list(id="008/PA")),
            newXMLNode("continent","Europe",attrs=list(id="Eu"))##,
            ##            newXMLNode("country","Chile",attrs=list(id="CL"))
        ))
    a <- addChildren(a, d)
}

cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/ecosystems_EU_prueba.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
