## IN THIS VERSION we are only considering one assessment unit: the regional assessment
##ERA.AUs <- newXMLNode("Assessment-units")
##for (AU in AUS) {
##}

## First, set up all Assessment-Unit nodes

AU.subset <- newXMLNode("Ecosystem-subset")
AU.summary <- newXMLNode("RA-Summaries")
AU.ration <- newXMLNode("Rationale")
AU.overall <- newXMLNode("Overall-category")
AU.bounds <- newXMLNode("Plausible-bounds")
AU.support <- newXMLNode("Supporting-subcriteria")
AU.criteria <- newXMLNode("Criterions")

## Now, add information per node

## subset:
AU.subset
newXMLNode("Subset-descriptions",
  children=list(newXMLNode("Subset-description","This assessment refers to the entire distribution of the assessment target as delimited by the assessment authors.",attrs=list(lang="en"))),
  parent=AU.subset)
addChildren(AU.subset,kids=list(Countries.node))

 ##
AU.summary
##
AU.ration
##
AU.overall
##
AU.bounds
##
AU.support
##
AU.criteria

newXMLNode("Assessment-unit",
  children=list(AU.subset, AU.summary, AU.ration, AU.overall, AU.bounds, AU.support, AU.criteria),
  parent=ERA.AUs)
