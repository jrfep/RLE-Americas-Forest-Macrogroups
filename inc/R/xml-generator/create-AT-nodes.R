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
