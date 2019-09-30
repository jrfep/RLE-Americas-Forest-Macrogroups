doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)

### "Indonesian Borneo’s lowland tropical forests"
##Case Study
CS = newXMLNode("Case-Study",
    attrs=list(id="",
        name="Extinction risk for Indonesian Borneo’s lowland tropical forests"), 
    children=c(newXMLNode("keywords",""),
        newXMLNode("citation",""),
        newXMLNode("assessment-author","Jon Paul Rodríguez"),
        newXMLNode("assessment-author","Jennifer K. Balch"),
        newXMLNode("assessment-author","Kathryn M. Rodríguez-Clark"),
        newXMLNode("assessment-date","2007"),
        newXMLNode("data-entry-responsible","JRFP"),
        newXMLNode("date-published","27 October 2006")
               ),
    parent=top)

######
## assessment-unit
newXMLNode("assessment-unit",
           children = c(newXMLNode("ecosystem-id", ""),
               newXMLNode("distribution",
                          children=c(
                              newXMLNode("summary","Assessment based on data from one of the three major islands of Indonesia, which represents roughly 30% of the original extent (by 1985) of this ecosystem in the country."),
                              newXMLNode("scope","Regional"),
                              newXMLNode("level","Regional"),
                              newXMLNode("countries","Indonesia")))),
                          parent=CS)
##Threats
    threats=newXMLNode("threats",
        children=c(newXMLNode("threatening-processes","logging, land cover conversion,fire"),
            newXMLNode("collapse-definition",""),
            newXMLNode("comments","The threat to Indonesian Borneo’s (Kalimantan) lowland tropical forests by logging,conversion, and fire has been well-documented in research that carefully defines the extent, composition, and natural dynamics of this ecosystem (Curran et al. 1999;
Siegert et al. 2001; Holmes 2002; Wikramanayake et al. 2002; Curran et al. 2004)."),
            newXMLNode("threat","logging",attrs=list(id=""),
                       children=c(
                           newXMLNode("timing","ongoing"),
                           newXMLNode("severity","causing or likely to cause declines")))),
        parent=CS)

    ##Risk assessment
    RA = newXMLNode("Risk-Assessment",parent=CS,
        children=c(
            newXMLNode("summary","The Indonesian Borneo’s low-land tropical forests was designated as Critically Endangered. This example demonstrates how a more refined regional-level analysis within the context of a coarser national-level study can contribute to understanding ecosystem extinction risk across scales."),
            newXMLNode("version","1.0"),
            newXMLNode("overall-category","CR",attrs=list(criterion="B"))))

    critA =  ## Criterion A
        newXMLNode("criterion",attrs=list(name="A",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","VU",attrs=list(criterion="A",reported="yes")),
                       newXMLNode("summary",""),
                       newXMLNode("area",attrs=c(type="calculated"),
                                  children=c(newXMLNode("year","1986"),
                                      newXMLNode("value","52000",units="km2"))),
                       newXMLNode("area",attrs=c(type="calculated"),
                                  children=c(newXMLNode("year","2001"),
                                      newXMLNode("value","23000",units="km2"))),
                       newXMLNode("area-loss","56", attrs=c(type="calculated",units="%"),
                                  children=c(newXMLNode("initial-year","1986"),
                                      newXMLNode("final-year","2001"))),
                       newXMLNode("area-loss",">100", attrs=c(type="projected",units="%"),
                                  children=c(newXMLNode("initial-year","1986"),
                                      newXMLNode("final-year","2016"))),
                       
                       newXMLNode("loss-rate","1930", attrs=c(type="calculated",units="km2/y"),
                                  children=c(newXMLNode("initial-year","1986"),
                                      newXMLNode("final-year","2001"))),
                       newXMLNode("trend-in-threat","Logging within protected areas expected to increase")

                              ),parent=RA)



    critB =  ## Criterion B
        newXMLNode("criterion",attrs=list(name="B",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","CR",
                       attrs=list(criterion="B",reported="yes")),
                       newXMLNode("summary",""),
                       newXMLNode("area-loss",">100", attrs=c(type="projected",units="%"),
                                  children=c(newXMLNode("initial-year","1986"),
                                      newXMLNode("final-year","2016"))),
                       
                       newXMLNode("loss-rate","1930", attrs=c(type="calculated",units="km2/y"),
                                  children=c(newXMLNode("initial-year","1986"),
                                      newXMLNode("final-year","2001")))),
                  parent=RA)


    critC =  ## Criterion C
        newXMLNode("criterion",attrs=list(name="C",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","DD",
                       attrs=list(criterion="C",reported="yes")),
                       newXMLNode("fragmentation",
                                  children=c(newXMLNode("small-fragments","n/a",attrs=list(units="%",threshold="<10km2")),
                                             newXMLNode("isolation","n/a",attrs=list(units="%",threshold=">1km")))),
                       newXMLNode("summary","Data concerning the area and isolation of fragments was insufficient to apply Criterion C as specified")),
                   parent=RA)

    critD =  ## Criterion D
        newXMLNode("criterion",attrs=list(name="D",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","LC",
                       attrs=list(criterion="D",reported="no")),
                       newXMLNode("summary",""),
                       newXMLNode("comments","Reported as 'n/a' in table, but should be 'Least Concern' due to current extent > 10km2")),
                  parent=RA)



###"Brazil’s Atlantic rainforests"
##Case Study
CS = newXMLNode("Case-Study",
    attrs=list(id="",
        name="Extinction risk for Brazil's Atlantic rainforests"), 
    children=c(newXMLNode("keywords",""),
        newXMLNode("citation",""),
        newXMLNode("assessment-author","Jon Paul Rodríguez"),
        newXMLNode("assessment-author","Jennifer K. Balch"),
        newXMLNode("assessment-author","Kathryn M. Rodríguez-Clark"),
        newXMLNode("assessment-date","2007"),
        newXMLNode("data-entry-responsible","JRFP"),
        newXMLNode("date-published","27 October 2006")
               ),
    parent=top)

######
## assessment-unit
newXMLNode("assessment-unit",
           children = c(newXMLNode("ecosystem-id", ""),
               newXMLNode("distribution",
                          children=c(
                              newXMLNode("summary","Synthesis of data along the whole distribution of Brazil's Atlantic rainforests, therefore the assessment is considered of global scope."),
                              newXMLNode("scope","Global"),
                              ##newXMLNode("level","National"),
                              newXMLNode("countries","Brazil")))),
                          parent=CS)
##Threats
    threats=newXMLNode("threats",
        children=c(newXMLNode("threatening-processes","deforestation and forest fragmentation"),
            newXMLNode("collapse-definition",""),
            newXMLNode("comments",""),
            newXMLNode("threat","deforestation",attrs=list(id=""),
                       children=c(
                           newXMLNode("timing","ongoing"),
                           newXMLNode("severity","causing or likely to cause declines")))),
        parent=CS)

    ##Risk assessment
    RA = newXMLNode("Risk-Assessment",parent=CS,
        children=c(
            newXMLNode("version","1.0"),
            newXMLNode("summary","Brazil’s Atlantic rainforests is classified
as Critically Endangered using Criterion A."),
            newXMLNode("overall-category","CR",attrs=list(criterion="A"))))

    critA =  ## Criterion A
        newXMLNode("criterion",attrs=list(name="A",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","VU",attrs=list(criterion="A",reported="yes")),
                       newXMLNode("summary",""),
                       newXMLNode("area",attrs=c(type="estimated"),
                                  children=c(newXMLNode("year","1600"),
                                      newXMLNode("value","1000000-15000000",units="km2"))),
                       newXMLNode("area",attrs=c(type="calculated"),
                                  children=c(newXMLNode("year","1990-1998"),
                                      newXMLNode("value","n/a",units="km2"))),
                       newXMLNode("area-loss","88-99", attrs=c(type="estimated",units="%"),
                                  children=c(newXMLNode("initial-year","1600"),
                                      newXMLNode("final-year","1990-1998"))),
                       newXMLNode("area-loss","n/a", attrs=c(type="projected",units="%"),
                                  children=c(newXMLNode("initial-year","2007"),
                                      newXMLNode("final-year","2037"))),
                       
                       newXMLNode("loss-rate","n/a", attrs=c(type="calculated",units="km2/y"),
                                  children=c(newXMLNode("initial-year","1600"),
                                      newXMLNode("final-year","1990-1998"))),
                       newXMLNode("trend-in-threat","Forest continues to be converted to pastures")

                              ),parent=RA)



    critB =  ## Criterion B
        newXMLNode("criterion",attrs=list(name="B",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","DD",
                       attrs=list(criterion="B",reported="no")),
                       newXMLNode("summary",""),
                       newXMLNode("comment","reported as 'n/a' in table"),
                       newXMLNode("area-loss","n/a", attrs=c(type="projected",units="%"),
                                  children=c(newXMLNode("initial-year","2007"),
                                      newXMLNode("final-year","2037"))),
                       
                       newXMLNode("loss-rate","n/a", attrs=c(type="calculated",units="km2/y"),
                                  children=c(newXMLNode("initial-year","1600"),
                                      newXMLNode("final-year","1990-1998")))),
                  parent=RA)


    critC =  ## Criterion C
        newXMLNode("criterion",attrs=list(name="C",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","DD",
                       attrs=list(criterion="C",reported="yes")),
                       newXMLNode("summary","There is conflicting evidence on the isolation of remaining fragments from different regions. The analysis of connectivity has not yet been synthesized. "),
                       newXMLNode("fragmentation",
                                  children=c(newXMLNode("small-fragments","n/a",attrs=list(units="%",threshold="<10km2")),
                                             newXMLNode("isolation","n/a",attrs=list(units="%",threshold=">1km"))))),
                   parent=RA)

    critD =  ## Criterion D
        newXMLNode("criterion",attrs=list(name="D",ra-version="Rodriguez et al. 2007"),
                   children=c(newXMLNode("category","LC",
                       attrs=list(criterion="D",reported="no")),
                       newXMLNode("summary",""),
                       newXMLNode("comments","Reported as 'n/a' in table, but should be 'Least Concern' due to current extent > 10km2")),
                  parent=RA)


## all criteria ready!

    
          

}
 
cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/RA_RBR07.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
