        ##Case Study
        CS = newXMLNode("Case-Study",attrs=list(id=k,name=sprintf(" '%s %s'", name.o, final.cat,summary="")),
            children=c(
                newXMLNode("ref-label",ref.lab),
                newXMLNode("assessment-author",unique(kk$Assesor)),
                newXMLNode("assessment-date",unique(kk$Year)),
                newXMLNode("data-entry",
                           children=c(
                               newXMLNode("summary","Text and data was extracted from published materials (manuscript, on-line appendix) and/or material provided by assessment authors. Text describing methods or summarizing results is copied verbatim or slightly edited, and a reference to page number in the report has been added. "),
                               newXMLNode("responsible","Jess Rowland"),
                               newXMLNode("responsible","JRFP"),
                               newXMLNode("source-document",unique(kk$Reference_code)),
                               newXMLNode("source-document","Please fill for other sources"))),
                newXMLNode("date-received",""),
                newXMLNode("date-accepted",""),
                newXMLNode("date-webpublished",""),
                newXMLNode("date-published","")
            ),
            parent=top)
        
        AT <- newXMLNode("assessment-target",
                         children = c(newXMLNode("ecosystem-id", k),
                             newXMLNode("name", name.e),
                             newXMLNode("original-name", name.o)
                                      ),
                         parent=CS)
        
    AU <- newXMLNode("assessment-unit",
                     children = c(
                         newXMLNode("ecosystem-subset","whole"),
                         newXMLNode("distribution",
                                    children=c(
                                        newXMLNode("summary",unique(kk$Ecosystem.distribution))##,
                                    )),
                         
                         
                         newXMLNode("type",unique(kk$Assessment.type)),
                         newXMLNode("level",unique(kk$Assessment.type.level.)),
                         newXMLNode("countries",unique(kk$Country))),
                         parent=AT)

    ##Threats
threats=newXMLNode("threats",parent=CS)
for (tt in trim(gsub("\\. "," ",strsplit(unique(kk$IUCN.Threat.),"\n")[[1]])))
    newXMLNode("IUCN-threat",tt,parent=threats)
                   
 
    
     
    RA = newXMLNode("Risk-Assessment",parent=AU,
        children=c(
            newXMLNode("assessment-version","1.0"),
            newXMLNode("overall-category",final.cat,attrs=list(criterion= paste(oa.cats,collapse=", ")))))
    
    for (cc in c("A","B")) {
        crit =  newXMLNode("criterion",attrs=list(name=cc),parent=RA)                     
        
        for (sc in c("1","2","2a","2b","3","4")) {
            if (sum(kk$RLE_criterion %in% sprintf("%s%s",cc,sc))==1) {
                with(subset(kk,RLE_criterion %in% sprintf("%s%s",cc,sc)),
                     newXMLNode("subcriterion",attrs=list(name=sprintf("%s%s",cc,sc),version=RLE.version.),
                                children=c(newXMLNode("category",ifelse(RLE.bounds=="",Overall,sprintf("%s (%s)",Overall,RLE.bounds)),
                                    attrs=list(criterion=cc,subcriterion=sc)),
                                    newXMLNode("parameter",RLE.parameter),
                                    newXMLNode("value",RLE_parameter_value),
                                    newXMLNode("spatial-data",Spatial.data..criteria.A...B.),
                                    newXMLNode("summary",sprintf("Assessment based on %s (%s). %s",RLE.parameter,RLE_parameter_value,Notes))),
                                parent=crit))
                
            }
        }
    }


    for (cc in c("C","D")) {
        crit =  newXMLNode("criterion",attrs=list(name=cc),parent=RA)                     
        
        for (sc in c("1","2","2a","2b","3","4")) {
            if (sum(kk$RLE_criterion %in% sprintf("%s%s",cc,sc))==1) {
                with(subset(kk,RLE_criterion %in% sprintf("%s%s",cc,sc)),
                     newXMLNode("subcriterion",attrs=list(name=sprintf("%s%s",cc,sc),version=RLE.version.),
                                children=c(newXMLNode("category",ifelse(RLE.bounds=="",Overall,sprintf("%s (%s)",Overall,RLE.bounds)),
                                    attrs=list(criterion=cc,subcriterion=sc)),
                                    newXMLNode("indicator",Indicator.criteria.C.D),
                                    newXMLNode("parameter",RLE.parameter),
                                    newXMLNode("value",RLE_parameter_value),
                                    newXMLNode("summary",sprintf("Assessment based on %s (%s). %s",RLE.parameter,RLE_parameter_value,Notes))),
                                parent=crit))
                
            }
        }
    }

        ##E

cc <- "E"
if (sum(kk$RLE_criterion %in% sprintf("%s",cc))==1) {
    crit =  
        with(subset(kk,RLE_criterion %in% sprintf("%s",cc)),
             newXMLNode("criterion",attrs=list(name=cc,version=RLE.version.),
                        children=c(newXMLNode("category",ifelse(RLE.bounds=="",Overall,sprintf("%s (%s)",Overall,RLE.bounds)),
                 attrs=list(criterion=cc)),
                 newXMLNode("parameter",RLE.parameter),
                 newXMLNode("value",RLE_parameter_value),
                 newXMLNode("summary",sprintf("Assessment based on %s (%s). %s",RLE.parameter,RLE_parameter_value,Extra.information))),
             parent=RA))
}
## all criteria ready!
