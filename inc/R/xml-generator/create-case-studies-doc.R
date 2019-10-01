## set-up auto-mode variables
today <- "2019-09-30"
auto.status <-"auto-generated-test"
auto.agent <- "Rsaurio"

## initialize document
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

## A case study requires six nodes: Names, Scope, Info, Target, Assessment and Curation

## These nodes are exactly the same for all case studies in this publication:

## Scope
Scope.description = newXMLNode("Scope-description","The risk assessment covered the entire distribution of the assessment target as described by the classification scheme and distribution maps available.")
Scope.classification = newXMLNode("Scope-classification", attrs=list(id="IUCN RLE", version="1.0", selected="yes", assigned-by="JRFEP"), children=list(newXMLNode("Scope-classification-element","1. Global", attrs=list(level=1))))

## Info
Info.ref <- newXMLNode("ref-label")
Info.update <- newXMLNode("Assessment-updated")
Info.authors <- newXMLNode("Assessment-authors","JR Ferrer-Paris")##Irene Zager

Info.colls <- newXMLNode("Assessment-collaborators") ##Collaborator Tina, Carmen, Erika etc...

Info.review <- newXMLNode("Assessment-reviewers")
Info.kwd <- newXMLNode("Assessment-Keywords")

## Curations
Curation.generation <- newXMLNode("content-curation",
  attrs=list(date=today, reviewer=auto.agent, status=auto.status), children=list(newXMLNode("edit-description","XML document generation through R-scripts")))

## Ecosystem risk assessment
  ERA.version <- newXMLNode("Assessment-version","2.2")
  ERA.protocol <- newXMLNode("Risk-assessment-protocol","IUCN RLE")

## set counter to 1
  CS.counter <- 1
