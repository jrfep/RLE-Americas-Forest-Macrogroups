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

gc()

path <- "~/Dropbox/Provita/doc/000_BDRLE"
source(sprintf("%s/xml01/inc/EUhabitats/00_EU_ecosystems.R",path))

## consider EU28 and EU28+ as two assessment units
source(sprintf("%s/xml01/inc/EUhabitats/01_EU_casestudies.R",path))











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
