
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

for (k in 1:nrow(costa.rica)) {
   mid <- costa.rica[k,"ecosystem_ID"]
   g <- match(mid,CR.info$ecosystem_ID)
   ss <- subset(CR.2, ## no useful addiitonal information
           gsub("Her_forest_CRI_2015","Herrera_RLE_CostaRica_2015",
                Ecosystem_ID_bis) %in% mid)

   final.cat <- subset(ss,RLE.criterion. %in% "Overall")$Overall
    ##Case Study
   CS = newXMLNode("Case-Study",attrs=list(id=mid,
                                    name=sprintf("Lista Roja de Ecosistemas de Costa Rica: '%s (%s)'",    CR.info[g,"SUBUNIDAD"],final.cat),
                                    summary=""),
       children=c(newXMLNode("keywords","Costa Rica, Nacional"),
           newXMLNode("ref-label","Herrera_RLE_CostaRica_2015"),
           newXMLNode("assessment-author","B Herrera-F"),
           newXMLNode("assessment-date","2015"),
           newXMLNode("data-entry",
                      children=c(
                          newXMLNode("summary","Text and data was extracted from the digital version of the technical report shared by the main author of the national assessment.

Text describing methods or summarizing results is copied verbatim or slightly edited, and a reference to page number in the report has been added. For those criteria or subcriteria that do not appear in the spatial database, the corresponding category (either DD or NE) has been added according to the description in the report. Ecosystem level information (type description, threats and other details) have not been filled out, but information could be sought in other sources."),
                          newXMLNode("responsible","JRFP"),
                          newXMLNode("source-document","Tecnical report indicating the general assessment results (pdf-document, author B. Herrera-F)"))),
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
                                    newXMLNode("summary","National assessment of Costa Rica's ecosystems"),
                                    newXMLNode("scope","Regional"),
                                    newXMLNode("level","National"),
                                    newXMLNode("country","Costa Rica")))),
                 parent=AT)


CEM <- newXMLNode("conceptual-ecosystem-model",
                  children = c(
               newXMLNode("summary","No conceptual model is described in the available documents."),
               newXMLNode("CEM-approach","implicit"),
               newXMLNode("CEM-type","none"),
               newXMLNode("CEM-source","not specified")),
               parent=AT)

    


##Threats
if (!is.na(CR.info[g,"threats"])) {
    tts <- c()
    for (kk in trim(strsplit(CR.info[g,"threats"],";")[[1]])) {
        tts <- c(tts,newXMLNode("threatening-process",kk))
    }
    threats=newXMLNode("threats",
        children=tts,parent=AT)

}

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
                   oa.cats <- ss$RLE.criterion.[ss$Overall %in% final.cat]
                   oa.cats <- oa.cats[!oa.cats %in% "Overall"]
               }
               
               RA = newXMLNode("Risk-Assessment",parent=AU,
                   children=c(
                       newXMLNode("risk-assessment-protocol","RLE 2.0 (Keith et al. 2013)"),
                               newXMLNode("assessment-version","1.0"),
                               newXMLNode("overall-category",final.cat,attrs=list(criterion= paste(oa.cats,collapse=", ")))))

               critA =  ## Criterion A
                   newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
                              children=c(
                                  newXMLNode("key-indicator-variable","mapped current distribution"),
                                  newXMLNode("key-indicator-variable","land cover"),
                                  newXMLNode("key-indicator-variable","forest cover"),
                                  newXMLNode("indicator-data","aerial fotography"),
                                  newXMLNode("indicator-data","remote sensors"),##Landsat
                                  newXMLNode("indicator-data","cartography"),
                                  newXMLNode("data-source","Instituto Geográfico Nacional / FONAFIFO (Costa Rica)"),
                                  newXMLNode("data-source","INBio (Costa Rica)"),
                                  newXMLNode("data-source","CATIE (Costa Rica)"),
                                  newXMLNode("method-of-measurement","Spatio-temporal analysis"),

                                  newXMLNode("collapse-definition","Para efectos de esta evaluación en lo que a los Criterios A y B se refiere, para todas las UF evaluadas se asume que llegan al colapso cuando la distribución de la vegetación en el mapa es cero. (pág. 67)"),
                                  
                                  ## subcriteria
                                  newXMLNode("subcriterion",attrs=list(name="1"),
                                             children=c(newXMLNode("category",subset(ss,RLE.criterion. %in% "A1")$Overall,attrs=list(criterion="A",subcriterion="1",reported="yes")),newXMLNode("summary",ifelse(subset(ss,RLE.criterion. %in% "A1")$Overall=="DD","Debe notarse que para los ecosistemas de páramos, los datos de cobertura utilizados carecían de la resolución espacial y espectral suficiente para realizar comparación en el período de estudio (ie. 50 años para el Criterio A), por lo que no fue posible realizar la evaluación. ... Por lo tanto, ... estos ecosistemas fueron calificados con 'deficientes en datos'. (pág. 67)","Se utilizó la cobertura de vegetación del año 1960, la cual fue revisada y ajustada utilizando fotografías aéreas del mismo año. Esto permitió la corroboración de la presencia de bosque y ajuste de límites de las diferentes clases. Además, la evaluación se utilizó el mapa de cobertura forestal del año 2010, clasificada a partir de imágenes Landsat de 30m de resolución. (pág. 13)")),
                                                 newXMLNode("initial-year","1960",attrs=c(type="calculated")),
                                                 newXMLNode("final-year","2010",attrs=c(type="calculated")))),
                                  newXMLNode("subcriterion",attrs=list(name="2a"),
                                             children=c(newXMLNode("category","DD",attrs=list(criterion="A",subcriterion="2a",reported="no")),newXMLNode("summary","Para efectos de este informe se aplicaron los criterios los criterios A1, B1 y B2. Los demás criterios no fuero aplicados debido a la falta de datos para su estimación. (pág 13)"))),
                                  newXMLNode("subcriterion",attrs=list(name="2b"),
                                             children=c(newXMLNode("category","DD",attrs=list(criterion="A",subcriterion="2b",reported="no")),newXMLNode("summary","Para efectos de este informe se aplicaron los criterios los criterios A1, B1 y B2. Los demás criterios no fuero aplicados debido a la falta de datos para su estimación. (pág 13)"))),
                                  newXMLNode("subcriterion",attrs=list(name="3"),
                                             children=c(newXMLNode("category","DD",attrs=list(criterion="A",subcriterion="3",reported="no")),newXMLNode("summary","Para efectos de este informe se aplicaron los criterios los criterios A1, B1 y B2. Los demás criterios no fuero aplicados debido a la falta de datos para su estimación. (pág 13)")))
               ),parent=RA)

               
critB = newXMLNode("criterion",attrs=list(name="B"), ## criterion B
    children=c(
        newXMLNode("key-indicator-variable","mapped current distribution"),
        newXMLNode("key-indicator-variable","land cover"),
        newXMLNode("key-indicator-variable","expert evaluation"),
        newXMLNode("indicator-data","aerial fotography"),
        newXMLNode("indicator-data","cartography"),
        newXMLNode("data-source","Instituto Geográfico Nacional / FONAFIFO (Costa Rica)"),
        newXMLNode("data-source","INBio (Costa Rica)"),
        newXMLNode("data-source","CATIE (Costa Rica)"),
        newXMLNode("data-source","Personal de las Áreas de Conservación de Costa Rica"),
        
        newXMLNode("method-of-measurement","Spatial analysis"),

        newXMLNode("collapse-definition","Para efectos de esta evaluación en lo que a los Criterios A y B se refiere, para todas las UF evaluadas se asume que llegan al colapso cuando la distribución de la vegetación en el mapa es cero. (pág. 67)"),

        ## subcriteria
        newXMLNode("subcriterion",attrs=list(name="1"),
                   children=c(newXMLNode("category",subset(ss,RLE.criterion. %in% "B1")$Overall,attrs=list(criterion="B",subcriterion="1",reported="yes")),newXMLNode("summary",ifelse(subset(ss,RLE.criterion. %in% "B1")$Overall=="DD","Debe notarse que para los ecosistemas de páramos, los datos de cobertura utilizados carecían de la resolución espacial y espectral suficiente para realizar comparación en el período de estudio (ie. 50 años para el Criterio A), por lo que no fue posible realizar la evaluación.En el caso del Criterio B, no fue posible realizar extrapolaciones al futuro, debido a que no se cuenta con información sobre tasas de cambio o información detallada sobre amenezas que permita una modelación estadística futura. Por lo tanto, para estos dos criterios, estos ecosistemas fueron calificados con 'deficientes en datos'. (pág. 67)",""))
                       )),
        newXMLNode("subcriterion",attrs=list(name="2"),
                   children=c(newXMLNode("category",subset(ss,RLE.criterion. %in% "B2")$Overall,attrs=list(criterion="B",subcriterion="2",reported="yes")),newXMLNode("summary",ifelse(subset(ss,RLE.criterion. %in% "B2")$Overall=="DD","Debe notarse que para los ecosistemas de páramos, los datos de cobertura utilizados carecían de la resolución espacial y espectral suficiente para realizar comparación en el período de estudio (ie. 50 años para el Criterio A), por lo que no fue posible realizar la evaluación.En el caso del Criterio B, no fue posible realizar extrapolaciones al futuro, debido a que no se cuenta con información sobre tasas de cambio o información detallada sobre amenezas que permita una modelación estadística futura. Por lo tanto, para estos dos criterios, estos ecosistemas fueron calificados con 'deficientes en datos'. (pág. 67)",""))
                       )),
        newXMLNode("subcriterion",attrs=list(name="3"),
                   children=c(newXMLNode("category","DD",attrs=list(criterion="B",subcriterion="3",reported="no")),newXMLNode("summary","Para efectos de este informe se aplicaron los criterios los criterios A1, B1 y B2. Los demás criterios no fuero aplicados debido a la falta de datos para su estimación. (pág 13)")))
               ),parent=RA)



               critC = newXMLNode("criterion",attrs=list(name="C"),##criterion C
                   children=c(
                       ## subcriteria
                       newXMLNode("subcriterion",attrs=list(name="1"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="1",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="2a"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="2a",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="2b"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="2b",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="3"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="C",subcriterion="3",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)")))))

               critD = newXMLNode("criterion",attrs=list(name="D"),##criterion D
                   children=c(
                       ## subcriteria
                       newXMLNode("subcriterion",attrs=list(name="1"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="1",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="2a"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="2a",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="2b"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="2b",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)"))),
                       newXMLNode("subcriterion",attrs=list(name="3"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="3",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)")))))


        
        critE = newXMLNode("criterion",attrs=list(name="E"),##criterion E
            children=c(newXMLNode("category","NE",attrs=list(criterion="E",reported="yes")),newXMLNode("summary","Cuadro 6 (pág 68 y siguientes)")),parent=RA)
                   

## all criteria ready!

}    
          

           
 
cat( saveXML(doc,file=sprintf("%s/RA_CostaRica.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
