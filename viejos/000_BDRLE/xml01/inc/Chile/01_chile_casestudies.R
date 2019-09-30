
doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)
for (k in 1:nrow(chile@data)) {

    ## Data entry
    DE <- newXMLNode("data-entry",
                         children=c(
                       newXMLNode("summary","Text and data was extracted automagically from three source documents shared by the main author of the national assessment. Text describing methods or summarizing results is copied verbatim or slightly edited, and a reference to page number in the report has been added. For those criteria or subcriteria that do not appear in the spatial database, the corresponding category (either DD or NE) has been added according to the description in the report. Ecosystem level information (type description, threats and other details) have not been filled out, but information could be sought in other sources."),
                       newXMLNode("responsible","JRFP"),

                       newXMLNode("source-document","Tecnical report indicating the general assessment results (pdf-document, author P. Pliscoff)"),
                       newXMLNode("source-document","Spatial database with information on categories for criteria per ecosystem (shapefile with associated tables, author P. Pliscoff)"),
                       newXMLNode("source-document","Spreadsheet comparing ecosystem units with other classification systems (excel-document, author unknown)")
                             newXMLNode("data-entry-version","test version"),
                             newXMLNode("data-entry-procedure","customized R scripts by JRFP"),
                             newXMLNode("date-time",format(Sys.time(), "%Y-%m-%d %H:%M:%S "))))

        ##Case Study

CS = newXMLNode("Case-Study",attrs=list(id=sprintf("%s_%s","MAC_RLE_Chile_2015",chile@data$ID[k]),name=sprintf("Lista Roja de Ecosistemas de Chile: '%s (%s)'", chile@data$Pisos[k], chile@data$Final[k]),summary=""),
    children=c(newXMLNode("keywords","Chile,Nacional"),
        newXMLNode("ref-label","MAC_RLE_Chile_2015"),
        newXMLNode("assessment-author","P Pliscoff"),
        newXMLNode("assessment-date","2015"),
        DE,
        newXMLNode("date-received",""),
        newXMLNode("date-accepted",""),
        newXMLNode("date-webpublished",""),
        newXMLNode("date-published","")
               ),
    parent=top)

######
## assessment-unit
##cambiar distribution por "assessment-unit"
## ecosystem-id
## ecosystem name
## distribution
## unit delimitation: wall-to-wall vs modeled, predicted

spatial.data =  newXMLNode("spatial-data",
        children=c(newXMLNode("filename","Chile_IUCN_redlist.shp"),
            newXMLNode("filetype","shapefile"),
            newXMLNode("filecreator","P Pliscoff"),
            newXMLNode("dataformat","vector, polygons"),
            newXMLNode("reference",""),
            newXMLNode("public-url",""),
            newXMLNode("id-infile",chile@data$ID[k],attrs=list(column="ID"))))

AT <- newXMLNode("assessment-target",
                 children = c(newXMLNode("ecosystem-id", sprintf("%s_%s","MAC_RLE_Chile_2015",chile@data$ID[k]))),
                     parent=CS)

AU <- newXMLNode("assessment-unit",
           children = c(
               newXMLNode("ecosystem-delimitation","national, not homologued"),
               newXMLNode("ecosystem-subset","national"),
               newXMLNode("distribution",
                          children=c(
                              newXMLNode("summary","National assessment of Chilean ecosystems"),
                              spatial.data,
                              ##newXMLNode("bounding-box",)
                              newXMLNode("centroid",
                                         attrs=list(latitude=coordinates(chile[2,])[,2],longitude=coordinates(chile[2,])[,1],proj4string=as.character(chile@proj4string))),
                              
                              newXMLNode("scope","Regional"),
                              newXMLNode("level","National"),
                              newXMLNode("country","Chile")))),
               parent=AT)


CEM <- newXMLNode("conceptual-ecosystem-model",
           children = c(
               newXMLNode("summary","No conceptual model is described in the available documents."),
               newXMLNode("CEM-approach","implicit"),
               newXMLNode("CEM-type","none"),
               newXMLNode("CEM-source","not specified")),
               parent=AT)

    



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

           
           if (chile@data$Final[k]=="LC") {
               oa.cats <- ""
           } else {
               oa.cats <- unique(gsub("_[A-Z]+","",colnames(chile@data)[chile@data[k,]==chile@data$Final[k]]))
               oa.cats <- oa.cats[!oa.cats %in% "Final"]
           }

           RA = newXMLNode("Risk-Assessment",parent=AU,
                           children=c(
                               newXMLNode("risk-assessment-protocol","RLE 2.0 (Keith et al. 2013)"),
                               newXMLNode("assessment-version","1.0"),
                               newXMLNode("overall-category",chile@data$Final[k],attrs=list(criterion= paste(oa.cats,collapse=", ")))))

critA =  ## Criterion A
    newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
               children=c(
                   newXMLNode("key-indicator-variable","mapped potential distribution"),
                   newXMLNode("key-indicator-variable","land cover"),
                   newXMLNode("key-indicator-variable","land use"),
                   newXMLNode("indicator-data","remote sensor"),
                   newXMLNode("indicator-data","cartography"),
                   newXMLNode("data-source","Modis"),
                   newXMLNode("data-source","Ministerio de Medio Ambiente (Chile)"),
                   newXMLNode("data-source","INFOR (Chile)"),
                   newXMLNode("method-of-measurement","Spatio-temporal analysis"),
                   
                   ## subcriteria
                   newXMLNode("subcriterion",attrs=list(name="1"),
                              children=c(newXMLNode("category","DD",attrs=list(criterion="A",subcriterion="1",reported="no")),newXMLNode("summary","El subcriterio A1 no se evaluó debido a que no se cuenta con datos históricos recientes (pág. 9)"))),
                   newXMLNode("subcriterion",attrs=list(name="2a"),
                              children=c(newXMLNode("category","DD",attrs=list(criterion="A",subcriterion="2a",reported="no")),newXMLNode("summary","El  sub  criterio  A2a,  en  cambio,  fue  aplicado  utilizando  técnicas  de  modelamiento  de cambio  climático,  pero  la  variabilidad  de  los  resultados  obtenidos,  en  función  de  las  distintas técnicas aplicadas y de los distintos modelos de circulación global de cambio climático (GCM), no permitió establecer resultados fiables por el momento. (pág. 13)"))),
                       newXMLNode("subcriterion",attrs=list(name="2b"),
                                  children=c(newXMLNode("category",chile@data$A2b[k],attrs=list(criterion="A",subcriterion="2b",reported="yes")),newXMLNode("summary","En el caso del sub criterio A2b, se realizó una estimación basada en una proyección de la tasa de pérdida reciente calculada en los últimos 20 años (1992-­‐2012). (pág. 9)"),
                                      newXMLNode("initial-year","1992",attrs=c(type="calculated")),
                                      newXMLNode("middle-year","2012",attrs=c(type="calculated")),
                                      
                                      newXMLNode("final-year","2042",attrs=c(type="inferred")))),
                   newXMLNode("subcriterion",attrs=list(name="3"),
                              children=c(newXMLNode("category",chile@data$A3[k],attrs=list(criterion="A",subcriterion="3",reported="yes")),
                                  newXMLNode("initial-year","1750",attrs=c(type="potential")),
                                  newXMLNode("final-year","2014",attrs=c(type="current")),
                                  newXMLNode("initial-extent",chile@data$Sup_pot[k],attrs=c(units="km2")),
                                  newXMLNode("final-extent",chile@data$Sup_actual[k],attrs=c(units="km2"))))
               ),parent=RA)

critB = newXMLNode("criterion",attrs=list(name="B"), ## criterion B
    children=c(
        newXMLNode("key-indicator-variable","mapped potential distribution"),
        newXMLNode("key-indicator-variable","land cover"),
        newXMLNode("key-indicator-variable","land use"),
        newXMLNode("indicator-data","remote sensor"),
        newXMLNode("indicator-data","cartography"),
        newXMLNode("data-source","Modis"),
        newXMLNode("data-source","Ministerio de Medio Ambiente (Chile)"),
        newXMLNode("data-source","INFOR (Chile)"),
        newXMLNode("method-of-measurement","Spatial analysis"),
        newXMLNode("method-of-measurement","Spatio-temporal analysis"),
        ## subcriteria
        newXMLNode("subcriterion",attrs=list(name="1"),
                   children=c(newXMLNode("category","NE",attrs=list(criterion="B",subcriterion="1",reported="no")),newXMLNode("summary","El sub criterio B1, que se evalúa en función de la definición del polígono mínimo convexo (Extensión de la ocupación) que abarque toda la distribución del ecosistema, no fue incluido debido a que a escala nacional la distribución del conjunto de ecosistemas terrestres, presenta patrones de distribución muy dispares. La determinación del polígono mínimo convexo, sobreestimaría la distribución de unidades que a pesar de poseer una superficie restringida, se distribuyen a lo largo de un gradiente latitudinal, esto se da comúnmente en Chile, debido a su disposición geográfica. (pág. 10)"))),
            newXMLNode("subcriterion",attrs=list(name="2"),
                 children=c(newXMLNode("category",chile@data$B2[k],attrs=list(criterion="B",subcriterion="2",reported="yes")),newXMLNode("summary","El sub criterio B2 fue aplicado utilizando la distribución actual de cada piso de vegetación, la cual fue procesada en formato raster (celdas) de 10x10 Km2, realizando así un conteo de celdas para calcular los umbrales de área definidos en este sub criterio de área de ocupación del ecosistema. Una vez realizado esto, y siguiendo los pasos estipulados para el criterio B2,  para el cual es condición requerida la inclusión de la evaluación de alguno de los sub criterios condicionantes indicados en la Figura 2, se definió aplicar el sub criterio condicionante de disminución continua (a) por una medida de extensión espacial (i). En este caso, se revisó el comportamiento en el criterio de reducción espacial A2b. (pág. 10)"),
                   newXMLNode("initial-year","2014",attrs=c(type="calculated")),
                   newXMLNode("initial-extent","",attrs=c(units="10x10 cells")))),
            newXMLNode("subcriterion",attrs=list(name="3"),
                 children=c(newXMLNode("category","NE",attrs=list(criterion="B",subcriterion="3",reported="no")),
                     newXMLNode("summary","Finalmente, el criterio B3 que se evalúa en función de un número de localidades de presencia, tampoco fue incluido (pág. 10)")))),parent=RA)

critC = newXMLNode("criterion",attrs=list(name="C"),##criterion C
    children=c(
        newXMLNode("key-indicator-variable","Water stress"),
        newXMLNode("key-indicator-variable","mapped potential distribution"),
        newXMLNode("indicator-data","projected climate change"),
        newXMLNode("indicator-data","cartography"),
        newXMLNode("data-source","Technical report (Universidad de Chile)"),
        newXMLNode("method-of-measurement","Spatial analysis"),
        newXMLNode("method-of-measurement","Model prediction"),
        ## subcriteria
        newXMLNode("subcriterion",attrs=list(name="1"),
                   children=c(newXMLNode("category","DD",attrs=list(criterion="C",subcriterion="1",reported="no")),newXMLNode("summary","Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)"))),
        newXMLNode("subcriterion",attrs=list(name="2b"),
                   children=c(newXMLNode("category",chile@data$C2_EH[k],attrs=list(criterion="C",subcriterion="2b",reported="yes")),newXMLNode("summary","El criterio C fue aplicado en el sub criterio C2, referido a la estimación de la degradación ambiental durante los próximos 50 años... Para  aplicar  el  sub  criterio  C2,  se  utilizaron  los resultados  obtenidos  en  el  estudio  “Plan  de  Acción  Para  la  Protección  y  Conservación  de  la Biodiversidad, en un Contexto de Adaptación al Cambio Climático” (Santibañez et al. 2013). En el estudio señalado, se calculó un índice de estrés integrado obtenido a partir de la estimación de un estrés  hídrico  y  un  estrés  térmico. (pág. 11)"),
                       newXMLNode("initial-year","2000",attrs=c(type="calculated")),
                       newXMLNode("final-year","2050",attrs=c(type="projected")))),
        newXMLNode("subcriterion",attrs=list(name="3"),
                   children=c(newXMLNode("category",chile@data$A3[k],attrs=list(criterion="C",subcriterion="3",reported="no")),
                       newXMLNode("summary"," Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)")))),
    parent=RA)

critC = newXMLNode("criterion",attrs=list(name="C"),##criterion C
    children=c(## subcriteria
        newXMLNode("key-indicator-variable","Summer thermal stress"),
        newXMLNode("key-indicator-variable","mapped potential distribution"),
        newXMLNode("indicator-data","projected climate change"),
        newXMLNode("indicator-data","cartography"),
        newXMLNode("data-source","Technical report (Universidad de Chile)"),
        newXMLNode("method-of-measurement","Spatial analysis"),
        newXMLNode("method-of-measurement","Model prediction"),

        newXMLNode("subcriterion",attrs=list(name="1"),
                   children=c(newXMLNode("category","DD",attrs=list(criterion="C",subcriterion="1",reported="no")),newXMLNode("summary","Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)"))),
        newXMLNode("subcriterion",attrs=list(name="2b"),
                   children=c(newXMLNode("category",chile@data$C2_ETE[k],attrs=list(criterion="C",subcriterion="2b",reported="yes")),newXMLNode("summary","El criterio C fue aplicado en el sub criterio C2, referido a la estimación de la degradación ambiental durante los próximos 50 años... Para  aplicar  el  sub  criterio  C2,  se  utilizaron  los resultados  obtenidos  en  el  estudio  “Plan  de  Acción  Para  la  Protección  y  Conservación  de  la Biodiversidad, en un Contexto de Adaptación al Cambio Climático” (Santibañez et al. 2013). En el estudio señalado, se calculó un índice de estrés integrado obtenido a partir de la estimación de un estrés  hídrico  y  un  estrés  térmico. (pág. 11)"),
                       newXMLNode("initial-year","2000",attrs=c(type="calculated")),
                       newXMLNode("final-year","2050",attrs=c(type="projected")))),
        newXMLNode("subcriterion",attrs=list(name="3"),
                   children=c(newXMLNode("category",chile@data$A3[k],attrs=list(criterion="C",subcriterion="3",reported="no")),
                       newXMLNode("summary"," Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)")))),parent=RA)

    critC = newXMLNode("criterion",attrs=list(name="C"),##criterion C
        children=c(
            ##newXMLNode("key-indicator-variable","Estrés térmico invernal"),
            newXMLNode("key-indicator-variable","Winter thermal stress"),
            newXMLNode("key-indicator-variable","mapped potential distribution"),
            newXMLNode("indicator-data","projected climate change"),
            newXMLNode("indicator-data","cartography"),
            newXMLNode("data-source","Technical report (Universidad de Chile)"),
            newXMLNode("method-of-measurement","Spatial analysis"),
            newXMLNode("method-of-measurement","Model prediction"),
            ## subcriteria
            newXMLNode("subcriterion",attrs=list(name="1"),
                       children=c(newXMLNode("category","DD",attrs=list(criterion="C",subcriterion="1",reported="no")),newXMLNode("summary","Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)"))),
            newXMLNode("subcriterion",attrs=list(name="2b"),
                       children=c(newXMLNode("category",chile@data$C2_ETI[k],attrs=list(criterion="C",subcriterion="2b",reported="yes")),newXMLNode("summary","El criterio C fue aplicado en el sub criterio C2, referido a la estimación de la degradación ambiental durante los próximos 50 años... Para  aplicar  el  sub  criterio  C2,  se  utilizaron  los resultados  obtenidos  en  el  estudio  “Plan  de  Acción  Para  la  Protección  y  Conservación  de  la Biodiversidad, en un Contexto de Adaptación al Cambio Climático” (Santibañez et al. 2013). En el estudio señalado, se calculó un índice de estrés integrado obtenido a partir de la estimación de un estrés  hídrico  y  un  estrés  térmico. (pág. 11)"),
                           newXMLNode("initial-year","2000",attrs=c(type="calculated")),
                           newXMLNode("final-year","2050",attrs=c(type="projected")))),
            newXMLNode("subcriterion",attrs=list(name="3"),
                       children=c(newXMLNode("category","DD",attrs=list(criterion="C",subcriterion="3",reported="no")),
                           newXMLNode("summary"," Tanto el criterio C1, como el sub criterio C3 no fueron evaluados, debido  a  la  imposibilidad  de  contar  con  datos  de  variables  abióticas  a  escala  nacional  tanto  en términos  históricos,  como  históricos  recientes. (pág. 11)")))),parent=RA)
        
        ##D
        critD =        newXMLNode("criterion",attrs=list(name="D"),##criterion D
            children=c(## subcriteria
                newXMLNode("summary","Se considera que no existe información suficiente para el cálculo del criterio D relacionado a la alteración de procesos e interacciones bióticas, a escala nacional. (pág 11)"),
                newXMLNode("subcriterion",attrs=list(name="1"),
                           children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="1",reported="no")))),
                       newXMLNode("subcriterion",attrs=list(name="2"),
                                  children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="2",reported="no")),newXMLNode("summary",""))),
                newXMLNode("subcriterion",attrs=list(name="3"),
                           children=c(newXMLNode("category","NE",attrs=list(criterion="D",subcriterion="3",reported="no")),newXMLNode("summary","")))),parent=RA)
        ##E
        
        critE = newXMLNode("criterion",attrs=list(name="E"),##criterion E
            children=c(newXMLNode("category","NE",attrs=list(criterion="E",reported="no")),newXMLNode("summary","Finalmente el criterio E, que se define con estimaciones cuantitativas del riesgo de colapso de los ecosistemas, no fue aplicado en este ejercicio, debido a que se consideró que no existe información suficiente para poder estimar que los ecosistemas analizados se encuentran en un estado de colapso. (pág 11)")),parent=RA)
                   

## all criteria ready!

    
          

}
 
cat( saveXML(doc,file=sprintf("%s/RA_chile.xml",output.dir),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
