##http://ec.europa.eu/environment/nature/knowledge/redlist_en.htm
##base de datos en https://forum.eionet.europa.eu/european-red-list-habitats/library/project-deliverables-data/database
##cd ~/Provita/data/RLEDB
##mdb-export Raw\ Database\ -\ 13_1_17.accdb 'European Red List of Habitats Table' > EU_RLE.csv
EU.RLE <- read.csv("~/Provita/data/RLEDB/EU_RLE.csv",stringsAsFactors=F)

EU.RLE[1,"Habitat.Type.Name"]
gsub("<[/p]+>","",EU.RLE[1,"Habitat.Type.Name"])
strsplit(gsub("<[/p]+>","",EU.RLE[5,"Classification"]),"IUCN:")[[1]][2]

##ecosystem type
## name lang="en"
## assessment type='Systematic:continental'
##region Europe
require(xml2)
require(XML)



doc = newXMLDoc()
top = newXMLNode("Case-Studies",doc=doc)
for (k in 1:5) {

    ##Case Study


CS = newXMLNode("Case-Study",attrs=list(id=sprintf("%s_%s","EUplus_RLE",EU.RLE$Habitat.ID[k],name=sprintf("EU Red list of habitats : '%s (%s)'", EU.RLE$Habitat.Type.Name[k], EU.RLE$Overall.Category.EU28[k]),summary="")),
    children=c(newXMLNode("keywords","EUplus,Continental"),
        newXMLNode("author",gsub("<[/p]+>","",as.character(EU.RLE$Assessors[k]))),
        newXMLNode("contributors",gsub("<[/p]+>","",as.character(EU.RLE$Contributors[k]))),
        newXMLNode("reviewer",gsub("<[/p]+>","",as.character(EU.RLE$Reviewers[k]))),

        newXMLNode("citation",""),
        newXMLNode("data-entry-responsible","JRFP"),
        newXMLNode("date-received",as.character(EU.RLE$Date.of.Assessment[k])),
        newXMLNode("date-accepted",as.character(EU.RLE$Date.of.Review[k])),
        newXMLNode("date-webpublished",""),
        newXMLNode("date-published",""),
        newXMLNode("ecosystem-id", sprintf("%s_%s","EU_RLE",EU.RLE$Habitat.ID[k])),
        newXMLNode("distribution","Continental assessment in Europe EU28+",attrs=list(latitude="",longitude="",proj4string="",summary="Continental assessment in EU+")),
        newXMLNode("total-area", EU.RLE[k,"Current.Estimated.Total.Area..EU28...in.Km2"],attrs=list(units="km2")),                     
        newXMLNode("scope","Regional"),
        newXMLNode("level","Continental"),
        ##    newXMLNode("countries","Chile"),
        ##    spatial.data,
        newXMLNode("version","1.0"),
        newXMLNode("overall-category",EU.RLE[k,"Overall.Category.EU28."],attrs=list(criterion=EU.RLE[k,"Overall.Criteria.EU28."]))),
    parent=top)

    ##Threats
threats=newXMLNode("threats",
    children=c(newXMLNode("threatening-processes",gsub("<[/p]+>","",as.character(EU.RLE$Pressures.and.Threats[k])))),
    parent=CS)

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
    conservation=c()
##RA_IUCN_Conservation Action_ID_1
##RA_IUCN_Conservation Action_1
##RA_Protected Area_present
##RA_Protected Area_name
##WCMC_Protected Area_Classification_Scheme
##RA_Conservation Action_Summary
##RA_Conservation Action_References

##Research Actions
    research=c()
##RA_Research Action_ID_1
##RA_Research Action_1
##RA_Research_Action_Summary
##RA_Research_Action_References

    ##Risk assessment
    RA = newXMLNode("Risk-Assessment",parent=CS)

##    spatial.data =  newXMLNode("spatial-data",
##        children=c(newXMLNode("filename","Chile_IUCN_redlist.shp"),
##            newXMLNode("filetype","shapefile"),
##            newXMLNode("filecreator","P Pliscoff"),
##            newXMLNode("dataformat","vector, polygons"),
##            newXMLNode("reference",""),
##            newXMLNode("public-url",""),
##            newXMLNode("id-infile",chile@data$ID[k],attrs=list(column="ID"))))


    
    

}
 
cat( saveXML(doc,file="~/Dropbox/Provita/xml/RLE_test1/RA_EUplus.xml",prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))









 [11] "EOO..EU28...in.Km2"                                             
 [13] "AOO..EU28...in.nb.of.10x10.Km.grid.cells"                       
 [15] "Map.Description"                                                
 [16] "Current.Estimated.Total.Area..EU28...in.Km2"                    
 [18] "Comment.EU28." 
 [50] "Criterion.B1...EOO..EU28...in.Km2"                              
 [52] "Criterion.B1a..EU28.."                                          
 [54] "Criterion.B1b..EU28.."                                          
 [56] "Criterion.B1c..EU28.."                                          
 [58] "Criterion.B2...AOO..EU28...in.nb.of.10x10.Km.grid.cells"        
 [60] "Criterion.B2a..EU28.."                                          
 [62] "Criterion.B2b..EU28.."                                          
 [64] "Criterion.B2c..EU28.."                                          
 [66] "Criterion.B3..EU28.."                                           
[117] "Category.B1.EU28."                                              
[119] "Category.B2.EU28."                                              
[121] "Category.B3.EU28."                                              


EU.RLE[k,c( "EU28..Average.Current.Trend.in.Quantity"   ,                     
 "Criterion.A1..EU28.."                        ,                   
 "Criterion.A2a..EU.28."                        ,                  
 "Criterion.A2a..EU28.."                         ,                 
 "Criterion.A2b..EU.28."                          ,                
 "Criterion.A2b..EU28.."                           ,               
 "Criterion.A3..EU28."                              ,              
 "Criterion.A3..EU28.."                              ,             
 "Criterion.A.Text"                                     ,          
 "Category.A1.EU28."                                   ,           
 "Category.A2a.EU28."                                   ,          
 "Category.A2b.EU28"                                      ,        
 "Category.A2b.EU28."                                      ,       
 "Category.A3.EU28"                                         ,      
 "Category.A3.EU28."            )]
