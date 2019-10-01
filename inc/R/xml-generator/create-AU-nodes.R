

ERA.AUs <- newXMLNode("Assessment-units")

for (AU in AUS) {
  AU.subset <- newXMLNode("")
  AU.summary <- newXMLNode("")
  AU.ration <- newXMLNode("")
  AU.overall <- newXMLNode("")
  AU.bounds <- newXMLNode("")
  AU.support <- newXMLNode("")
  AU.criteria <- newXMLNode("")

  newXMLNode("Assessment-unit",
    children=list(AU.subset,AU.summary,AU.ration,AU.overall,AU.bounds,AU.support,AU.criteria),
    parent=ERA.AUs)
}
