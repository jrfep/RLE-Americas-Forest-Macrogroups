## check if document root exists
if (!exists("top"))
  stop("Please start with a document root.. (create-case-studies-doc.R)")

## A case study requires six nodes: Names, Scope, Info, Target, Assessment and Curation

## These nodes are exactly the same for all case studies in this publication, they are handled in create-case-studies-doc.R

##Scope
Scope = newXMLNode("Scope",
  children=list(newXMLNode("Scope-descriptions", children=list(Scope.description)),Scope.classification))


##Info, set load to false after the first case study
do.load <- ifelse(CS.counter==1,"true","false")
Info = newXMLNode("Assessment-Information",
  attrs=list(date=today, reviewer=auto.agent, status=auto.status, load=do.load),
  children=list(Info.ref,Info.update,Info.authors,Info.colls,Info.review,Info.kwd))

## Curation
Curation = newXMLNode("Content-Curations",
  children=list(Curation.generation))

## The following nodes summarize information for each ecosystem type:
## run create-AT-nodes.R
Target = newXMLNode("Assessment-Target",
  attrs=list(date=today,`updated-by`=auto.agent,status=auto.status),
  children=list(AT.id,AT.descs,AT.names,AT.biota,AT.abiotic,AT.biotic,AT.services,AT.threats,AT.actions,AT.research,AT.CEM,AT.class,AT.dist,AT.collapse))
Names = newXMLNode("Case-Study-Names", children=list(newXMLNode("Case-Study-Name",CS.name,attrs=list(lang="en"))))

## The following node summarizes information for each assessment unit
## run create-AU-nodes.R

Assessment = newXMLNode("Ecosystem-Risk-Assessment",
  attrs=list(date=today,`updated-by`=auto.agent,status=auto.status),
  children=list(ERA.version,ERA.protocol,ERA.AUs))

## Wrapping up, and writing node to root node:

CS.attr.list <- list(name=CS.name,id=CS.id)
CS.chld.list <- list(Names,
  Scope,newXMLNode("Assessment-type","Systematic"),Info,Target,Assessment,Curation)

CS = newXMLNode("Case-Study",
  attrs=CS.attr.list,
  parent=top,
  children=CS.chld.list)
