doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)
for (k in 1:nrow(EU.RLE)) {

    ##Case Study
CS = newXMLNode("Case-Study",attrs=list(id=sprintf("%s_%s","EU_RLE",EU.RLE$Habitat.ID[k],name=sprintf("EU Red list of habitats : '%s (%s)'", EU.RLE$Habitat.Type.Name[k], EU.RLE$Overall.Category.EU28[k]),summary="")),
    children=c(newXMLNode("keywords","EU,Continental"),
        newXMLNode("ref-label",ifelse(EU.RLE$Habitat.Group[k] %in% "Marine","EU_RLH_Marine_2017","EU_RLH_Terrestrial_2017")),
        newXMLNode("assessment-author",gsub("<[/p]+>","",as.character(EU.RLE$Assessors[k]))),
        newXMLNode("assessment-contributors",gsub("<[/p]+>","",as.character(EU.RLE$Contributors[k]))),
        newXMLNode("assessment-reviewer",gsub("<[/p]+>","",as.character(EU.RLE$Reviewers[k]))),
        newXMLNode("data-entry",
                   children=c(
                       newXMLNode("summary","All text and data was extracted automagically from a public database. No further details were added. Slight editing of values were performed to conform with the RLE format. Application of a combined C/D criterion is recorded, but does not conform to RLE guidelines."),
                       newXMLNode("responsible","JRFP"),

                       newXMLNode("source-document","Database (MS Access, public access)")
                   )),


        newXMLNode("date-received",as.character(EU.RLE$Date.of.Assessment[k])),
        newXMLNode("date-accepted",as.character(EU.RLE$Date.of.Review[k])),
        newXMLNode("date-webpublished",""),
        newXMLNode("date-published","")),
    parent=top)

## one assessment target (ecosystem-type), but two assessment units (EU and EUplus)
AT <- newXMLNode("assessment-target",
                 children = c(        newXMLNode("ecosystem-id", sprintf("%s_%s","EU_RLE",EU.RLE$Habitat.ID[k]))),
                     parent=CS)

    ##Threats
threats=newXMLNode("threats",
    children=c(newXMLNode("threatening-processes",gsub("<[/p]+>","",as.character(EU.RLE$Pressures.and.Threats[k])))),
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
##        parent=CS)
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


######
## Assessment unit EU28

AU <- newXMLNode("assessment-unit",
           children = c(
               newXMLNode("ecosystem-subset","administrative subset"),
               newXMLNode("distribution","Continental assessment of the Red List status of all natural and semi-natural habitat within the EU28 countries",attrs=list(latitude="",longitude="",proj4string="",summary="Continental assessment in EU28 countries")),
               newXMLNode("total-area", EU.RLE[k,"Current.Estimated.Total.Area..EU28..in.Km2"],attrs=list(units="km2")),                     
               newXMLNode("scope","Regional"),
               newXMLNode("level","Continental")),
                 parent=AT)


    ##Risk assessment
    RA = newXMLNode("Risk-Assessment",parent=AU,
        children=c(newXMLNode("assessment-version","1.0"),
            newXMLNode("overall-category",EU.RLE[k,"Overall.Category.EU28"],attrs=list(criterion=EU.RLE[k,"Overall.Criteria.EU28"]))))

##    spatial.data =  newXMLNode("spatial-data",
##        children=c(newXMLNode("filename","Chile_IUCN_redlist.shp"),
##            newXMLNode("filetype","shapefile"),
##            newXMLNode("filecreator","P Pliscoff"),
##            newXMLNode("dataformat","vector, polygons"),
##            newXMLNode("reference",""),
##            newXMLNode("public-url",""),
##            newXMLNode("id-infile",chile@data$ID[k],attrs=list(column="ID"))))


#############
## criterion A
##############

critA <- newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
                    children=c(newXMLNode("summary",sprintf("%s %s",EU.RLE[k,"Criterion.A.Text"],EU.RLE[k,"Trends.in.Quantity"]))),
                    parent=RA
                    )

a <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A1.EU28"],attrs=list(criterion="A",subcriterion="1",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A1..EU28."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A1..EU28."])))),
                parent=critA)

a <- newXMLNode("subcriterion",attrs=list(name="2a"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A2a.EU28"],attrs=list(criterion="A",subcriterion="2a",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A2a..EU.28."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A2a..EU.28."])))),
                parent=critA)

a <- newXMLNode("subcriterion",attrs=list(name="2b"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A2b.EU28"],attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A2b..EU.28."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A2b..EU.28."])))),
                parent=critA)


a <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A3.EU28"],attrs=list(criterion="A",subcriterion="3",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A3..EU28."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A3..EU28."])))),
                parent=critA)

#############
## criterion B
##############

critB <- newXMLNode("criterion",attrs=list(name="B",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.B.Text"]))),
                        newXMLNode("comment",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Comment.EU28"])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B1.EU28"],attrs=list(criterion="B",subcriterion="1",reported="yes")),
                    newXMLNode("EOO-area",EU.RLE[k,"EOO..EU28..in.Km2"],attrs=list(units="km2")),                 
                    newXMLNode("EOO",EU.RLE[k,"Criterion.B1...EOO..EU28..in.Km2"]),
                    newXMLNode("literal-a",EU.RLE[k,"Criterion.B1a..EU28."]),
                    newXMLNode("literal-b",EU.RLE[k,"Criterion.B1b..EU28."]),
                    newXMLNode("literal-c",EU.RLE[k,"Criterion.B1c..EU28."])),
                parent=critB)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B2.EU28"],attrs=list(criterion="B",subcriterion="2",reported="yes")),
                    newXMLNode("AOO-area",EU.RLE[k,"AOO..EU28..in.nb.of.10x10.Km.grid.cells"],attrs=list(units="10x10cells")),                 
                    newXMLNode("AOO",EU.RLE[k,"Criterion.B2...AOO..EU28..in.nb.of.10x10.Km.grid.cells"]),
                    newXMLNode("literal-a",EU.RLE[k,"Criterion.B2a..EU28."]),
                    newXMLNode("literal-b",EU.RLE[k,"Criterion.B2b..EU28."]),
                    newXMLNode("literal-c",EU.RLE[k,"Criterion.B2c..EU28."])),
                parent=critB)


b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B3.EU28"],attrs=list(criterion="B",subcriterion="3",reported="yes")),
                    newXMLNode("localities",EU.RLE[k,"Criterion.B3..EU28."])),
                parent=critB)





#############
## criterion C
##############

critC <- newXMLNode("criterion",attrs=list(name="C",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C1.EU28"],attrs=list(criterion="C",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C1.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C1.Severity..EU28."],attrs=list(units="%"))),
                parent=critC)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C2.EU28"],attrs=list(criterion="C",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C2.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C2.Severity..EU28."],attrs=list(units="%"))),
                parent=critC)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C3.EU28"],attrs=list(criterion="C",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C3.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C3.Severity..EU28."],attrs=list(units="%"))),
                parent=critC)



#############
## criterion D
##############

critD <- newXMLNode("criterion",attrs=list(name="D",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D1.EU28"],attrs=list(criterion="D",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D1.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D1.Severity..EU28."],attrs=list(units="%"))),
                parent=critD)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D2.EU28"],attrs=list(criterion="D",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D2.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D2.Severity..EU28."],attrs=list(units="%"))),
                parent=critD)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D3.EU28"],attrs=list(criterion="D",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D3.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D3.Severity..EU28."],attrs=list(units="%"))),
                parent=critD)



#############
## criterion C+D
##############

critCD <- newXMLNode("criterion",attrs=list(name="C/D",version="EU-RLE"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"]))),
                        newXMLNode("comment","Combination of C and D criteria does not conform with RLE Guidelines (version 1.1) and is strongly discouraged.")),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D1.EU28"],attrs=list(criterion="C/D",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D1.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D1.Severity..EU28."],attrs=list(units="%"))),
                parent=critCD)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D2.EU28"],attrs=list(criterion="C/D",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D2.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D2.Severity..EU28."],attrs=list(units="%"))),
                parent=critCD)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D3.EU28"],attrs=list(criterion="C/D",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D3.Extent..EU28."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D3.Severity..EU28."],attrs=list(units="%"))),
                parent=critCD)


#############
## criterion E
##############


critE <- newXMLNode("criterion",attrs=list(name="E",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.E.Text"]))),
                        newXMLNode("category",EU.RLE[k,"Category.E.EU28"],attrs=list(criterion="E",reported="yes")),
                        newXMLNode("risk-of-collapse",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.E..EU28."])))),
                    parent=RA
                    )

######
## ahora viene el Assessment unit EU28+


AU <- newXMLNode("assessment-unit",
           children = c(
               newXMLNode("ecosystem-subset","administrative subset"),
               newXMLNode("distribution","Continental assessment of the Red List status of all natural and semi-natural habitat at the geographic level of the EU28+ (including Norway, Switzerland, Iceland, and the Balkan countries)",attrs=list(latitude="",longitude="",proj4string="",summary="Continental assessment in EU28+ countries")),
               newXMLNode("total-area", EU.RLE[k,"Current.Estimated.Total.Area..EU28...in.Km2"],attrs=list(units="km2")),                     
               newXMLNode("scope","Regional"),
               newXMLNode("level","Continental")),
                 parent=AT)


    ##Risk assessment
    RA = newXMLNode("Risk-Assessment",parent=AU,
        children=c(newXMLNode("assessment-version","1.0"),
            newXMLNode("overall-category",EU.RLE[k,"Overall.Category.EU28."],attrs=list(criterion=EU.RLE[k,"Overall.Criteria.EU28."]))))

    

#############
## criterion A
##############

critA <- newXMLNode("criterion",attrs=list(name="A",version="RLE 2.0"),
                    children=c(newXMLNode("summary",sprintf("%s %s",EU.RLE[k,"Criterion.A.Text"],EU.RLE[k,"Trends.in.Quantity"]))),
                    parent=RA
                    )

a <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A1.EU28."],attrs=list(criterion="A",subcriterion="1",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A1..EU28.."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28..Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A1..EU28.."])))),
                parent=critA)

a <- newXMLNode("subcriterion",attrs=list(name="2a"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A2a.EU28."],attrs=list(criterion="A",subcriterion="2a",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A2a..EU28.."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A2a..EU28.."])))),
                parent=critA)

a <- newXMLNode("subcriterion",attrs=list(name="2b"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A2b.EU28."],attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A2b..EU28.."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28..Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A2b..EU28.."])))),
                parent=critA)


a <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.A3.EU28."],attrs=list(criterion="A",subcriterion="3",reported="yes")),
                    newXMLNode("summary",ifelse(EU.RLE[k,"Criterion.A3..EU28.."] %in% c("unknown","Unknown"),"Trend in quantity is reported as 'unknown' for this time period.",sprintf("Average trend in quantity is reported as '%s' with value '%s %%'",EU.RLE[k,"EU28.Average.Current.Trend.in.Quantity"],EU.RLE[k,"Criterion.A3..EU28.."])))),
                parent=critA)

#############
## criterion B
##############

critB <- newXMLNode("criterion",attrs=list(name="B",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.B.Text"]))),
                        newXMLNode("comment",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Comment.EU28."])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B1.EU28."],attrs=list(criterion="B",subcriterion="1",reported="yes")),
                    newXMLNode("EOO-area",EU.RLE[k,"EOO..EU28...in.Km2"],attrs=list(units="km2")),                 
                    newXMLNode("EOO",EU.RLE[k,"Criterion.B1...EOO..EU28...in.Km2"]),
                    newXMLNode("literal-a",EU.RLE[k,"Criterion.B1a..EU28.."]),
                    newXMLNode("literal-b",EU.RLE[k,"Criterion.B1b..EU28.."]),
                    newXMLNode("literal-c",EU.RLE[k,"Criterion.B1c..EU28.."])),
                parent=critB)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B2.EU28."],attrs=list(criterion="B",subcriterion="2",reported="yes")),
                    newXMLNode("AOO-area",EU.RLE[k,"AOO..EU28...in.nb.of.10x10.Km.grid.cells"],attrs=list(units="10x10cells")),                 
                    newXMLNode("AOO",EU.RLE[k,"Criterion.B2...AOO..EU28...in.nb.of.10x10.Km.grid.cells"]),
                    newXMLNode("literal-a",EU.RLE[k,"Criterion.B2a..EU28.."]),
                    newXMLNode("literal-b",EU.RLE[k,"Criterion.B2b..EU28.."]),
                    newXMLNode("literal-c",EU.RLE[k,"Criterion.B2c..EU28.."])),
                parent=critB)


b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.B3.EU28."],attrs=list(criterion="B",subcriterion="3",reported="yes")),
                    newXMLNode("localities",EU.RLE[k,"Criterion.B3..EU28.."])),
                parent=critB)





#############
## criterion C
##############

critC <- newXMLNode("criterion",attrs=list(name="C",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C1.EU28."],attrs=list(criterion="C",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C1.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C1.Severity..EU28.."],attrs=list(units="%"))),
                parent=critC)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C2.EU28."],attrs=list(criterion="C",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C2.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C2.Severity..EU28.."],attrs=list(units="%"))),
                parent=critC)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C3.EU28."],attrs=list(criterion="C",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C3.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C3.Severity..EU28.."],attrs=list(units="%"))),
                parent=critC)



#############
## criterion D
##############

critD <- newXMLNode("criterion",attrs=list(name="D",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"])))),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D1.EU28."],attrs=list(criterion="D",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D1.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D1.Severity..EU28.."],attrs=list(units="%"))),
                parent=critD)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D2.EU28."],attrs=list(criterion="D",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D2.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D2.Severity..EU28.."],attrs=list(units="%"))),
                parent=critD)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.D3.EU28."],attrs=list(criterion="D",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.D3.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.D3.Severity..EU28.."],attrs=list(units="%"))),
                parent=critD)



#############
## criterion C+D
##############

critCD <- newXMLNode("criterion",attrs=list(name="C/D",version="EU-RLE"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.C.D.Text"]))),
                        newXMLNode("comment","Combination of C and D criteria does not conform with RLE Guidelines (version 1.1) and is strongly discouraged.")),
                    parent=RA
                    )

b <- newXMLNode("subcriterion",attrs=list(name="1"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D1.EU28."],attrs=list(criterion="C/D",subcriterion="1",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D1.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D1.Severity..EU28.."],attrs=list(units="%"))),
                parent=critCD)

b <- newXMLNode("subcriterion",attrs=list(name="2"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D2.EU28."],attrs=list(criterion="C/D",subcriterion="2",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D2.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D2.Severity..EU28.."],attrs=list(units="%"))),
                parent=critCD)

b <- newXMLNode("subcriterion",attrs=list(name="3"),
                children=c(newXMLNode("category",EU.RLE[k,"Category.C.D3.EU28."],attrs=list(criterion="C/D",subcriterion="3",reported="yes")),
                    newXMLNode("extent",EU.RLE[k,"Criterion.C.D3.Extent..EU28.."],attrs=list(units="%")),                 
                    newXMLNode("severity",EU.RLE[k,"Criterion.C.D3.Severity..EU28.."],attrs=list(units="%"))),
                parent=critCD)


#############
## criterion E
##############


critE <- newXMLNode("criterion",attrs=list(name="E",version="RLE 2.0"),
                    children=c(newXMLNode("summary",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.E.Text"]))),
                        newXMLNode("category",EU.RLE[k,"Category.E.EU28."],attrs=list(criterion="E",reported="yes")),
                        newXMLNode("risk-of-collapse",gsub("<[/p]+>","",sprintf("%s",EU.RLE[k,"Criterion.E..EU28.."])))),
                    parent=RA
                    )    

}
 
cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/RA_EU.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
