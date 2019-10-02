CS.name.simple <- sprintf("%s","[CS name]")
CS.name.pref <- sprintf("%s, %s",CS.name.simple,"[Category and bounds]")
CS.id <- "[id]"
CS.name.lang <- "en"

AT.id <- newXMLNode("AT-id",AT.id.Provita$`AT-id`)
AT.descs <- newXMLNode("AT-descriptions")
AT.names <- newXMLNode("AT-names")
AT.biota <- newXMLNode("Characteristic-biota")
AT.abiotic <- newXMLNode("Abiotic-environment")
AT.biotic <- newXMLNode("Biotic-processes")
AT.services <- newXMLNode("Ecosystem-services")
AT.threats <- newXMLNode("Threats")
AT.actions <- newXMLNode("Conservation-actions")
AT.research <- newXMLNode("Research-needs")
AT.CEM <- newXMLNode("Conceptual-ecosystem-model")
AT.class <- newXMLNode("Classifications")
AT.dist <- newXMLNode("Distribution")
AT.collapse <- newXMLNode("Collapse-definition")

division.name <- subset(tipologia,Division.Code %in% rsm$IVC.division & !(division %in% ""))$division
newXMLNode("AT-description",sprintf("The '%s' is a forest macrogroup belonging to the '%s' division in the International Vegetation Classification system (Faber-Langendoen et al. 2014). It is considered here as a proxy for a forest ecosystem type in a regional assessment of forest ecosystems in the Americas.",rsm$IVC.Name, division.name), attrs=list(lang="en"),parent=AT.descs)
newXMLNode("AT-name",rsm$IVC.Name,attrs=list(lang="en"),parent=AT.names)
