## initialize document
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

## A case study requires six nodes: Names, Scope, Info, Target, Assessment and Curation

## These nodes are exactly the same for all case studies in this publication:

## Scope
Scope.description = newXMLNode("Scope-description","The risk assessment covered the entire distribution of the assessment target as described by the classification scheme and distribution maps available.")
Scope.classification = newXMLNode("Scope-classification", attrs=list(id="IUCN RLE", version="1.0", selected="yes", `assigned-by`="JRFEP"), children=list(newXMLNode("Scope-classification-element","1. Global", attrs=list(level=1))))

## Info
Info.ref <- newXMLNode("ref-label", mi.reflabel)
Info.update <- newXMLNode("Assessment-updated","")
Info.authors <- newXMLNode("Assessment-authors",
  children=list(newXMLNode("Author","JR Ferrer-Paris"),
  newXMLNode("Author","I Zager")))

## TO DO: change this to a for loop with addChildren
Info.colls <- newXMLNode("Assessment-collaborators",
  children=list(newXMLNode("Collaborator","C. Josse"),
  newXMLNode("Collaborator","M. A. Oliveira‐Miranda"),
  newXMLNode("Collaborator","M. González‐Gil"),
  newXMLNode("Collaborator","T Oliveira"),
  newXMLNode("Collaborator","M. González‐Gil"),
  newXMLNode("Collaborator","D. Velarde"),
  newXMLNode("Collaborator","G. C. De La Cruz-Melo"),
  newXMLNode("Collaborator","J.C. Amilibia"),
  newXMLNode("Collaborator","E. Primiciero")))

## TO DO: change this to a for loop with addChildren
Info.review <- newXMLNode("Assessment-reviewers")
Info.kwd <- newXMLNode("Keywords",
  children=list(newXMLNode("Keyword","Woodlands"),
    newXMLNode("Keyword","Climate change"),
      newXMLNode("Keyword","Defaunation"),
        newXMLNode("Keyword","Anthromes"),
          newXMLNode("Keyword","Surface water")))

## Curations
Curation.generation <- newXMLNode("content-curation",
  attrs=list(date=today, reviewer=auto.agent, status=auto.status), children=list(newXMLNode("edit-description","XML document generation through R-scripts")))

## Ecosystem risk assessment
  ERA.version <- newXMLNode("Assessment-version","2.2")
  ERA.protocol <- newXMLNode("Risk-assessment-protocol","IUCN RLE")
