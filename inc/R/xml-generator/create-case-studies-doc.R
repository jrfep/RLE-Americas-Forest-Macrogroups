## initialize document
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

## A case study requires six nodes: Names, Scope, Info, Target, Assessment and Curation

## These nodes are exactly the same for all case studies in this publication:

## Scope
Scope.description = newXMLNode("Scope-description",attrs=list(lang="en"),"The risk assessment covered the entire distribution of the assessment target as described by the classification scheme and distribution maps available.")
Scope.classification = newXMLNode("Scope-classification", attrs=list(id="IUCN Scope", version="1.0", selected="no", `assigned-by`="JRFEP"), children=list(newXMLNode("Scope-classification-element","1. Global", attrs=list(level=1))))

## Info
Info.ref <- newXMLNode("ref-label", mi.reflabel)
Info.update <- newXMLNode("Assessment-date","2018-10-15")
Info.authors <- newXMLNode("Assessment-authors",
  children=list(newXMLNode("Author","JR Ferrer-Paris"),
  newXMLNode("Author","I Zager")))

## TO DO: change this to a for loop with addChildren
Info.colls <- newXMLNode("Assessment-collaborators",
  children=list(newXMLNode("Collaborator","C. Josse"), ## original description
  newXMLNode("Collaborator","M. A. Oliveira‐Miranda"), ## first assessments
  newXMLNode("Collaborator","M. González‐Gil"), ## initial data management
  newXMLNode("Collaborator","D. Velarde"), ## compiled descriptive files
  newXMLNode("Collaborator","G. C. De La Cruz-Melo"), ## compiled desc. files
  ##newXMLNode("Collaborator","J.C. Amilibia"), ## not sure which role...
  newXMLNode("Collaborator","E. Primiciero"))) ## compiled descriptive files

## TO DO: change this to a for loop with addChildren
Info.review <- newXMLNode("Assessment-reviewers",children=list(newXMLNode("Reviewer")))
Info.kwd <- newXMLNode("Assessment-Keywords",
  children=list(newXMLNode("Keyword","Woodlands"),
    newXMLNode("Keyword","Climate change"),
      newXMLNode("Keyword","Defaunation"),
        newXMLNode("Keyword","Anthromes"),
          newXMLNode("Keyword","Surface water")))

## Curations
Curation.generation <- newXMLNode("content-curation",
  attrs=list(date=today, `updated-by`=auto.agent, status=auto.status), children=list(newXMLNode("edit-description","XML document generation through R-scripts")))

## Ecosystem risk assessment
  ERA.version <- newXMLNode("Assessment-version","2.2")
  ERA.protocol <- newXMLNode("Risk-assessment-protocol","IUCN RLE")
