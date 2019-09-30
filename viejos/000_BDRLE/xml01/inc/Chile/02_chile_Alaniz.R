
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)
for (k in 1:nrow(chileB)) {

    ##Case Study


CS = newXMLNode("Case-Study",attrs=list(id=sprintf("%s",chileB$ID[k],name=sprintf("Input data quality assessment: '%s (%s)'", chileB$EcoName[k], chileB$RiskCat[k]),summary="")),
    children=c(newXMLNode("keywords","Chile,Sub-national"),
        newXMLNode("ref-label","Alaniz_Chile_CentralChileHotspot_2016"),
        newXMLNode("assessment-author","Alaniz et al."),
        newXMLNode("assssment-date","2016"),
        newXMLNode("data-entry",
                   children=c(
                       newXMLNode("summary","This assessment is not linked to previous assessment of the same ecosystems in Chile. Data on criteria and categories extracted from original article and online-appendix. In some cases it remains unclear which criteria were evaluated or lead to lower risk categories than the overall category."),
                       newXMLNode("responsible","JRFP"),
                       newXMLNode("responsible","Jess Rowland"),
                       newXMLNode("source-document","Original publication (pdf-document, author Alaniz et al.)")))
               ),
    parent=top)

######
## assessment-unit
##cambiar distribution por "assessment-unit"
## ecosystem-id
## ecosystem name
## distribution
## unit delimitation: wall-to-wall vs modeled, predicted

AT <- newXMLNode("assessment-target",
                 children = c(newXMLNode("ecosystem-id", gsub(":","_",chile.A$ecoid[match(chileB$ID[k],chile.A$ecosystem_ID)]))),
                     parent=CS)

AU <- newXMLNode("assessment-unit",
           children = c(
               newXMLNode("ecosystem-delimitation","national, not homologued"),
               newXMLNode("ecosystem-subset","national"),
               newXMLNode("distribution",
                          children=c(
                              newXMLNode("summary","Subnational assessment of Chilean ecosystems"),
                              newXMLNode("scope","Regional"),
                              newXMLNode("level","Sub-national"),
                              newXMLNode("country","Chile")))),
               parent=AT)



CEM <- newXMLNode("conceptual-ecosystem-model",
           children = c(
               newXMLNode("summary","No conceptual model is described in the available documents."),
               newXMLNode("CEM-approach","implicit"),
               newXMLNode("CEM-type","none"),
               newXMLNode("CEM-source","not specified")),
               parent=AT)

##Risk assessment


if (chileB$RiskCat[k]=="LC") {
    oa.cats <- ""
} else {
    oa.cats <- unique(gsub("_[A-Z]+","",colnames(chileB)[chileB[k,]==chileB$RiskCat[k]]))
    oa.cats <- oa.cats[!oa.cats %in% c("RiskCat","A","B","C","D",NA)]
}

RA = newXMLNode("Risk-Assessment",parent=AU,
    children=c(
        newXMLNode("risk-assessment-protocol","RLE 2.0 (Bland et al. 2016) + spatio-temporal QA (Alaniz et al. 2016)"),
        newXMLNode("assessment-version","I.0"),
        newXMLNode("overall-category",chileB$RiskCat[k],attrs=list(criterion= paste(oa.cats,collapse=", ")))))

critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="A"),parent=RA)
            
for (j in c("1","2a","2b","3")) {
    if (!chileB[k,sprintf("A%s",j)] %in% c("NP","DD")) {
        addChildren(critA,newXMLNode("subcriterion",attrs=list(name=j),
                                     children=c(newXMLNode("category",chileB[k,sprintf("A%s",j)],attrs=list(criterion="A",subcriterion=j,reported="yes")))))
    }
}
critB =  ## Criterion B
    newXMLNode("criterion",attrs=list(name="B"),parent=RA)

for (j in c("1","2","3")) {
    if (!chileB[k,sprintf("B%s",j)] %in% c("NP","DD")) {
        addChildren(critB,newXMLNode("subcriterion",attrs=list(name=j),
                                     children=c(newXMLNode("category",chileB[k,sprintf("B%s",j)],attrs=list(criterion="B",subcriterion=j,reported="yes")))))
    }
}

    
critC =  ## Criterion C
    newXMLNode("criterion",attrs=list(name="C"),parent=RA)

for (j in c("1","2a","2b","3")) {
    if (!chileB[k,sprintf("C%s",j)] %in% c("NP","DD")) {
        addChildren(critC,newXMLNode("subcriterion",attrs=list(name=j),
                                     children=c(newXMLNode("category",chileB[k,sprintf("C%s",j)],attrs=list(criterion="C",subcriterion=j,reported="yes")))))
    }
}

critD =  ## Criterion D
    newXMLNode("criterion",attrs=list(name="D"),parent=RA)

for (j in c("1","2a","2b","3")) {
    if (!chileB[k,sprintf("D%s",j)] %in% c("NP","DD")) {
        addChildren(critD,newXMLNode("subcriterion",attrs=list(name=j),
                                     children=c(newXMLNode("category",chileB[k,sprintf("D%s",j)],attrs=list(criterion="D",subcriterion=j,reported="yes")))))
    }
}



## all criteria ready!

    
          

}
 
cat( saveXML(doc,file=sprintf("%s/RA_chile2.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
