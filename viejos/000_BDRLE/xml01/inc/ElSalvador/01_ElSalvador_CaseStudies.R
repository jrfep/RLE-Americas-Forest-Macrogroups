
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

for (k in 1:nrow(el.salvador)) {
   mid <- el.salvador[k,"ecosystem_ID"]
   g <- match(mid,ES.info$ecosystem_ID)
    ss <- subset(ES.2,Ecosystem_ID_bis %in% sub("Crespin_RLE_ElSalvador_2015","Crespin_ElSalvador_SLV_2015",mid))

   final.cat <- subset(ss,trim(RLE_criterion) %in% "Overall")$Overall
    ##Case Study
   CS = newXMLNode("Case-Study",attrs=list(id=mid,
                                    name=sprintf("Predicting ecosystem collapse in El Salvador: '%s (%s)'",    ES.info[g,"Ecosystem"],final.cat),
                                    summary=""),
       children=c(newXMLNode("keywords","El Salvador, Nacional"),
           newXMLNode("ref-label","Crespin_RLE_ElSalvador_2015"),
           newXMLNode("assessment-author","Silvio J. Crespin"),
           newXMLNode("assessment-author","Javier A. Simonetti"),
           newXMLNode("assessment-date","2015"),
           newXMLNode("data-entry",
                      children=c(
                          newXMLNode("summary","."),
                          newXMLNode("responsible","JRFP"),
                          newXMLNode("responsible","Jess Rowland"),
                          newXMLNode("source-document","Peer reviewed publication (pdf-document and Supporting Information, author S.J. Crespin and J.A. Simonetti)"))),
           newXMLNode("date-received",""),
           newXMLNode("date-accepted",""),
           newXMLNode("date-webpublished",""),
           newXMLNode("date-published","")),
    parent=top)

######
## assessment-unit
##cambiar distribution por "assessment-unit"
## ecosystem-id
## ecosystem name
## distribution
## unit delimitation: wall-to-wall vs modeled, predicted


AT <- newXMLNode("assessment-target",
                 children = c(newXMLNode("ecosystem-id", mid)),
                 parent=CS)

AU <- newXMLNode("assessment-unit",
                 children = c(
                     newXMLNode("ecosystem-delimitation","national, not homologued"),
                     newXMLNode("ecosystem-subset","national"),
                     newXMLNode("distribution",
                                children=c(
                                    newXMLNode("summary","Assessment of El Salvador's ecosystems"),
                                    newXMLNode("scope","Regional"),
                                    newXMLNode("level","National"),
                                    newXMLNode("country","El Salvador")))),
                 parent=AT)


CEM <- newXMLNode("conceptual-ecosystem-model",
                  children = c(
               newXMLNode("summary","No conceptual model is described in the available documents."),
               newXMLNode("CEM-approach","implicit"),
               newXMLNode("CEM-type","none"),
               newXMLNode("CEM-source","not specified")),
               parent=AT)

    
##Threats
    tts <- c()
   
   if (!is.na(ES.info[g,"Threats"])) {
       for (kk in trim(strsplit(ES.info[g,"Threats"],";")[[1]])) {
           tts <- c(tts,newXMLNode("threatening-process",kk))
       }
       
   }
   if (any(!is.na(ss$IUCN.Threat.Sub.category))) {
       uu <- unique(ss[,c("IUCN.Threat.","IUCN.Threat.Sub.category")])
       for (kk in 1:nrow(uu)) {
           tts <- c(tts,newXMLNode("IUCN-threat",uu[kk,1]))
           tts <- c(tts,newXMLNode("IUCN-threat-subcategory",uu[kk,2]))
       }

   }
    
    threats=newXMLNode("threats",
        children=tts,parent=AT)

##Threats
##    threats=newXMLNode("threats",
##        children=c(newXMLNode("threatening-processes",""),
##            newXMLNode("collapse-definition",""),
##            newXMLNode("comments","Threat information not explicitly given in original publication"),
##            newXMLNode("threat","",attrs=list(id=""),
##                       children=c(newXMLNode("type","minor"),
##                           newXMLNode("timing","ongoing"),
##                           newXMLNode("scope","whole"),
##                           newXMLNode("severity","causing or likely to cause declines"),
##                           newXMLNode("impact","medium impact"),
##                           newXMLNode("stresses","minor")))),
##        parent=AT)
##Conservation Actions
##    conservation=c()
##RA_IUCN_Conservation Action_ID_1
##RA_IUCN_Conservation Action_1
##RA_Protected Area_present
##RA_Protected Area_name
##WCMC_Protected Area_Classification_Scheme
##RA_Conservation Action_Summary
##RA_Conservation Action_References

##Research Actions
##    research=c()
##RA_Research Action_ID_1
##RA_Research Action_1
##RA_Research_Action_Summary
##RA_Research_Action_References

    ##Risk assessment
               
               
               if (final.cat=="LC") {
                   oa.cats <- ""
               } else {
                   oa.cats <- trim(ss$RLE_criterion[ss$Overall %in% final.cat])
                   oa.cats <- oa.cats[!oa.cats %in% "Overall"]
               }
               
               RA = newXMLNode("Risk-Assessment",parent=AU,
                   children=c(
                       newXMLNode("risk-assessment-protocol","RLE 2.0 (Keith et al. 2013)"),
                               newXMLNode("assessment-version","1.0"),
                               newXMLNode("overall-category",final.cat,attrs=list(criterion= paste(oa.cats,collapse=", ")))))


###########
   ## criteria
   
   critA =  ## Criterion A
       newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
                  children=c(
                      newXMLNode("key-indicator-variable","mapped ecosystem distribution"),
                      newXMLNode("key-indicator-variable","mapped potential ecosystem distribution"),
                      
                      newXMLNode("indicator-data","digital elevation model"),
                      newXMLNode("indicator-data","remote sensors"),##Landsat
                      newXMLNode("indicator-data","ground verification"),
                      newXMLNode("data-source","Ministry of Environment and Natural Resources (El Salvador)"),
                      newXMLNode("method-of-measurement","Spatio-temporal analysis")),parent=RA)
                      
   ##newXMLNode("collapse-definition",""),

critB = newXMLNode("criterion",attrs=list(name="B"), ## criterion B
    children=c(
        newXMLNode("key-indicator-variable","mapped ecosystem distribution"),
                      newXMLNode("indicator-data","digital elevation model"),
                      newXMLNode("indicator-data","remote sensors"),##Landsat
                      newXMLNode("indicator-data","ground verification"),
                      newXMLNode("data-source","Ministry of Environment and Natural Resources (El Salvador)"),
        
        newXMLNode("method-of-measurement","Spatial analysis")),

        ##newXMLNode("collapse-definition","Para efectos de esta evaluación en lo que a los Criterios A y B se refiere, para todas las UF evaluadas se asume que llegan al colapso cuando la distribución de la vegetación en el mapa es cero. (pág. 67)"),
        parent=RA)


   
   ## subcriteria
   newXMLNode("subcriterion",attrs=list(name="1"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "A1")$Overall,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                  newXMLNode("summary", " We based our assess- ment on reduction in geographic distribution (criteria A) by using the difference between current distributions (2011) and a preterit state (1998) to estimate rate of change and project its decline over the last 50 years (1961–2011; criteria A1) ... assuming a constant rate of decline in the observed time lapse for a linear loss of surface area.(pag. 494)"),
                  newXMLNode("period-begins","1961"),
                  newXMLNode("period-ends","2011"),
                  
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("distribution-area","not reported",attrs=c(type="projected")),
                                 newXMLNode("year","1961"),
                                 newXMLNode("method","Rate of change estimated from measured values and projected to reference date, assuming a constant rate of decline in the observed time lapse for a linear loss of surface area. (pag. 494)"))),
                 newXMLNode("parameter-value",
                            children=c(newXMLNode("distribution-area",ES.info[k,"Raw surface area (km2) 1998"],attrs=c(units="km2",type="measured")),
                                newXMLNode("year","1998"),
                                newXMLNode("method","Ecosystem distribution data for 1998 were generated by bands 4, 5 and 3 from Landsat 7 scenes at a 30 × 30 m resolution by a combination of paths 18 and 19 with rows 50 and 51. (pag. 493)"))),
                    newXMLNode("parameter-value",
               
                             children=c(newXMLNode("distribution-area",
                                 ES.info[k,"Raw surface area (km2) 2011"],
                                 attrs=c(units="km2",type="measured")),
                                 newXMLNode("year","2011"),
                                 newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
              parent=critA)


   newXMLNode("subcriterion",attrs=list(name="2a"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "A2a")$Overall,attrs=list(criterion="A",subcriterion="2a",reported="yes")),
                  newXMLNode("summary", " We based our assess- ment on reduction in geographic distribution (criteria A) by using the difference between current distributions (2011) and a preterit state (1998) to estimate rate of change and project its decline ...within the next 50 years (2011–2061; criteria A2a) ... assuming a constant rate of decline in the observed time lapse for a linear loss of surface area.(pag. 494)"),
                  newXMLNode("period-begins","2011"),
                  newXMLNode("period-ends","2061"),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("distribution-area","not reported",attrs=c(type="projected")),
                                 newXMLNode("year","2061"),
                                 newXMLNode("method","Rate of change estimated from measured values and projected to reference date, assuming a constant rate of decline in the observed time lapse for a linear loss of surface area. (pag. 494)"))),
                 newXMLNode("parameter-value",
                            children=c(newXMLNode("distribution-area",ES.info[k,"Raw surface area (km2) 1998"],attrs=c(units="km2",type="measured")),
                                newXMLNode("year","1998"),
                                newXMLNode("method","Ecosystem distribution data for 1998 were generated by bands 4, 5 and 3 from Landsat 7 scenes at a 30 × 30 m resolution by a combination of paths 18 and 19 with rows 50 and 51. (pag. 493)"))),
                    newXMLNode("parameter-value",
               
                             children=c(newXMLNode("distribution-area",
                                 ES.info[k,"Raw surface area (km2) 2011"],
                                 attrs=c(units="km2",type="measured")),
                                 newXMLNode("year","2011"),
                                 newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
                  
              parent=critA)


      newXMLNode("subcriterion",attrs=list(name="2b"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "A2b")$Overall,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                  newXMLNode("summary", " We based our assess- ment on reduction in geographic distribution (criteria A) by using the difference between current distributions (2011) and a preterit state (1998) to estimate rate of change and project its decline ...over a period of 50 years including both past and present (1998–2048; criteria A2b) ... assuming a constant rate of decline in the observed time lapse for a linear loss of surface area.(pag. 494)"),
                  newXMLNode("period-begins","1998"),
                  newXMLNode("period-ends","2048"),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("distribution-area","not reported",attrs=c(type="projected")),
                                 newXMLNode("year","2048"),
                                 newXMLNode("method","Rate of change estimated from measured values and projected to reference date, assuming a constant rate of decline in the observed time lapse for a linear loss of surface area. (pag. 494)"))),
                 newXMLNode("parameter-value",
                            children=c(newXMLNode("distribution-area",ES.info[k,"Raw surface area (km2) 1998"],attrs=c(units="km2",type="measured")),
                                newXMLNode("year","1998"),
                                newXMLNode("method","Ecosystem distribution data for 1998 were generated by bands 4, 5 and 3 from Landsat 7 scenes at a 30 × 30 m resolution by a combination of paths 18 and 19 with rows 50 and 51. (pag. 493)"))),
                    newXMLNode("parameter-value",
               
                             children=c(newXMLNode("distribution-area",
                                 ES.info[k,"Raw surface area (km2) 2011"],
                                 attrs=c(units="km2",type="measured")),
                                 newXMLNode("year","2011"),
                                 newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
                  
              parent=critA)


   newXMLNode("subcriterion",attrs=list(name="3"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "A3")$Overall,attrs=list(criterion="A",subcriterion="3",reported="yes")),
                  newXMLNode("summary", " We used the difference between historic and current distributions to estimate historical decline since 1750 (crite- ria A3). Given a dearth of maps ranging from 1750s, the potential distribution of vegetation is the best approximation we have.(pag. 494)"),
                  newXMLNode("period-begins","uncertain"),
                  newXMLNode("period-ends","2011"),
                  newXMLNode("parameter-value",
                             children=c(
                                 newXMLNode("distribution-area",ES.info[k,"Raw surface area (km2) ca.1750"],attrs=c(units="km2",type="potential")),
                                 newXMLNode("year","uncertain"),
                                 newXMLNode("spatial-data","digital elevation model",attrs=c(resolution="15x15m")),
                                 newXMLNode("spatial-data","ASTER tiles",attrs=c(resolution="15x15m")),
                                 newXMLNode("method","Historic distri- bution for ecosystems refers to their potential distributions, based on a combination of criteria for current physiognomy at different elevations obtained from a digital elevation model (DEM) generated from a mosaic of the 2011 ASTER tiles. (pag. 493)"))),
                  
                  newXMLNode("parameter-value",                  
                             children=c(
                                 newXMLNode("distribution-area",
                                            ES.info[k,"Raw surface area (km2) 2011"],
                                            attrs=c(units="km2",type="measured")),
                                 newXMLNode("year","2011"),
                                 
                                 newXMLNode("spatial-data","ASTER tiles",attrs=c(resolution="15x15m")),
                                 newXMLNode("ground-thruthing","yes"),
                                 
                                 newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
              parent=critA)


               

    

        ## subcriteria
         newXMLNode("subcriterion",attrs=list(name="1"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "B1")$Overall,attrs=list(criterion="B",subcriterion="1",reported="yes")),
                  newXMLNode("summary", "... using current distri- bution to calculate the extent of occurrence (extent of minimum convex polygon enclosing all occurrences, criteria B1) and area of occupancy (number of 10 × 10 km grid cells occupied by at least 1 km2, criteria B2) for each ecosystem (Fig. 1), specifically by observed or inferred continuing decline (criteria B1a + 2a), threatening processes likely to produce continuing decline (criteria B1b + 2b) and low number of locations (criteria B1c + 2c) (see Keith et al. 2013). For qualitative criteria such as ongoing threatening processes, we consulted each ecosystem definition in Vreugdenhil et al. (2012), which provided a description of anthropogenic activities likely to cause surface area change. (pag. 494)"),
                  newXMLNode("period","2011"),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("continuing-decline","not reported"))),
                 newXMLNode("parameter-value",
                             children=c(newXMLNode("threatening-process","not reported"))),
                 newXMLNode("parameter-value",
                             children=c(newXMLNode("number-of-locations","not reported"))),
                 newXMLNode("parameter-value",
                             children=c(newXMLNode("extent-of-occurrence",ES.info[k,"Extent of occurence (km2) ca. 2011"],attrs=c(units="km2",type="measured")),
                  newXMLNode("year","2011"),
                  
                  newXMLNode("spatial-data","ASTER tiles",attrs=c(resolution="15x15m")),
                  newXMLNode("ground-thruthing","yes"),
                  
                  newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
                  
                    parent=critB)

            newXMLNode("subcriterion",attrs=list(name="2"),
              children=c(
                  newXMLNode("category",subset(ss,RLE_criterion %in% "B2")$Overall,attrs=list(criterion="B",subcriterion="2",reported="yes")),
                  newXMLNode("summary", "... using current distri- bution to calculate the extent of occurrence (extent of minimum convex polygon enclosing all occurrences, criteria B1) and area of occupancy (number of 10 × 10 km grid cells occupied by at least 1 km2, criteria B2) for each ecosystem (Fig. 1), specifically by observed or inferred continuing decline (criteria B1a + 2a), threatening processes likely to produce continuing decline (criteria B1b + 2b) and low number of locations (criteria B1c + 2c) (see Keith et al. 2013). For qualitative criteria such as ongoing threatening processes, we consulted each ecosystem definition in Vreugdenhil et al. (2012), which provided a description of anthropogenic activities likely to cause surface area change. (pag. 494)"),
                  newXMLNode("period","2011"),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("continuing-decline","not reported"))),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("threatening-process","not reported"))),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("number-of-locations","not reported"))),
                  newXMLNode("parameter-value",
                             children=c(newXMLNode("area-of-occupancy",ES.info[k,"Area of occupancy Cells occupied"],attrs=c(units="10x10km cells",subset="occupied>1km2",type="measured")),
                                 newXMLNode("year","2011"),
                                 
                                 newXMLNode("spatial-data","ASTER tiles",attrs=c(resolution="15x15m")),
                                 newXMLNode("ground-thruthing","yes"),
                                 
                                 newXMLNode("method","Data for 2011 were gener- ated at a 15 × 15 m resolution from multiple ASTER tiles. Ground verification during 2011 allowed for independent corroboration of distinct vegetation groups. (pag. 493)")))),
              parent=critB)



   ## not evaluated


   critC = newXMLNode("criterion",attrs=list(name="C"),##criterion C
       children=c(
           ## subcriteria
           newXMLNode("subcriterion",attrs=list(name="1"),
                      children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="1",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="2a"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="2a",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="2b"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="2b",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="3"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="3",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)")))),        parent=RA)

      critD = newXMLNode("criterion",attrs=list(name="D"),##criterion D
       children=c(
           ## subcriteria
           newXMLNode("subcriterion",attrs=list(name="1"),
                      children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="1",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="2a"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="2a",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="2b"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="2b",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)"))),
                       newXMLNode("subcriterion",attrs=list(name="3"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="3",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)")))),        parent=RA)
        
   critE = newXMLNode("criterion",attrs=list(name="E"),##criterion E
       children=c(newXMLNode("category","NE",attrs=list(criterion="E",reported="no")),newXMLNode("summary","Following our approach on land use change, we focused on rates of decline in distribution and the degree of geo- graphic restriction (criteria A and B). (pag 494)")),parent=RA)
                   

## all criteria ready!

}    
          

           
 
cat( saveXML(doc,file=sprintf("%s/RA_ElSalvador.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
