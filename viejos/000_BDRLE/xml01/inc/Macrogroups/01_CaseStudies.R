for (ff in c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5",
             "1.B.1","1.B.2","1.B.3")) {
    doc = newXMLDoc()
    top = newXMLNode("Case-Studies",doc=doc)
    for (k in grep(ff,confTest$Name)) {
        mid <- subset(RLEDB[[2]],EcoName %in% confTest[k,"Name"])$ecosystem_ID
        mg <- confTest[k,"Macrogroup.code"]
        tt <- subset(tipologia,macrogroup_key %in% confTest[k,"Macrogroup.code"])
        
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG1$Codigo) {
            ss <- subset(rsm.MG1,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG2$Codigo) {
            ss <- subset(rsm.MG2,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG3$Codigo) {
            ss <- subset(rsm.MG3,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG4$Codigo) {
            ss <- subset(rsm.MG4,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG5$Codigo) {
            ss <- subset(rsm.MG5,Codigo %in% mg)
        }
        ss[ss %in% "N.T."] <- NA

        paises <- c()
        for (dd in c("tabSAM","tabNAC")) {
            if (file.exists(sprintf("~/tmp/%s/Paises/%s.txt",dd,mg))) {
                pss <- read.table(sprintf("~/tmp/%s/Paises/%s.txt",dd,mg))
                paises <- c(paises,names(rev(sort(table(pss$V2)))))
            }
        }
        paises <- unique(paises[!paises %in% "*"])
        slc.paises <- subset(lista.paises,OBJECTID %in% paises)
        
        countries <- c()
        for (p in 1:nrow(slc.paises)) {
            countries <- c(countries,newXMLNode("country", iconv(slc.paises$LOCAL[p],"latin1","utf8"),attrs=list(id=slc.paises$ISO2[p])))
        }
        
        

        DE <- newXMLNode("data-entry",
                         children=c(
                             newXMLNode("summary","Text and data were extracted directly from several documents and tables generated during the assessment procedure."),
                             newXMLNode("source-document","IVC Typology (spread-sheet, author NatureServe)"),
                             newXMLNode("data-entry-version","test version"),
                             newXMLNode("data-entry-procedure","customized R scripts by JRFP"),
                             newXMLNode("date-time",format(Sys.time(), "%Y-%m-%d %H:%M:%S "))))

        for (kk in c("JRFP",unique(ss$Realizado.por))) {
            addChildren(DE,newXMLNode("responsible",kk))
        }
        ##final.cat <- subset(ss,RLE.criterion. %in% "Overall")$Overall
        final.cat <- "NE"
        ##Case Study
        CS = newXMLNode("Case-Study",attrs=list(id=mid,
                                         name=sprintf("Forest Macrogroups of the Americas: '%s (%s)'",   confTest[k,"Name"] ,final.cat),
                                         summary=""),
       children=c(newXMLNode("keywords","Continental"),
           newXMLNode("ref-label","FerrerParis_Continental_ForestMacrogroup_2017"),
           newXMLNode("assessment-author","JR Ferrer-Paris"),
           newXMLNode("assessment-date","2017"),
           DE,
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
                     newXMLNode("ecosystem-delimitation","global"),
                     newXMLNode("ecosystem-subset","global"),
                     newXMLNode("distribution",
                                children=c(
                                    newXMLNode("summary",sprintf("Global assessment of the %s Macrogroup",confTest[k,"Name"])),
                                    newXMLNode("scope","Global"),
                                    newXMLNode("level","Global"),
                                    countries
                                ))),
                 parent=AT)


CEM <- newXMLNode("conceptual-ecosystem-model",
                  children = c(
               newXMLNode("summary","No conceptual model is described in the available documents."),
               newXMLNode("CEM-approach","descriptive"),
               newXMLNode("CEM-type","descriptive"),
               newXMLNode("CEM-source","NatureServe")),
               parent=AT)

    


##Threats
##if (!is.na(CR.info[g,"threats"])) {
##    tts <- c()
##    for (kk in trim(strsplit(CR.info[g,"threats"],";")[[1]])) {
##        tts <- c(tts,newXMLNode("threatening-process",kk))
##    }
##    threats=newXMLNode("threats",
##        children=tts,parent=AT)
##}

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
               
               
##               if (final.cat=="LC") {
##                   oa.cats <- ""
##               } else {
##                   oa.cats <- ss$RLE.criterion.[ss$Overall %in% final.cat]
##                   oa.cats <- oa.cats[!oa.cats %in% "Overall"]
##               }
               oa.cats <- ""
               RA = newXMLNode("Risk-Assessment",parent=AU,
                   children=c(
                       newXMLNode("risk-assessment-protocol","RLE 2.0 (Bland et al. 2017)"),
                               newXMLNode("assessment-version","1.0"),
                               newXMLNode("overall-category",final.cat,attrs=list(criterion= paste(oa.cats,collapse=", ")))))

        for (ll in LETTERS[1:5]) {
            crits = newXMLNode("criterion",attrs=list(name=ll),##criterion E
                children=c(newXMLNode("category","NE",attrs=list(criterion=ll,reported="yes")),newXMLNode("summary","This criterion has not been evaluated yet")),parent=RA)
        }
    }    
           
   cat( saveXML(doc,file=sprintf("%s/RA_Forest_Macrogroups_%s.xml",output.dir,ff),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
}

