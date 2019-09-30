
##https://docs.marklogic.com/guide/getting-started/XQueryTutorial
##https://www.altova.com/mobiletogether/xpath-intro

####
## This query returns the list of ecosystems that have been tagged with ecosystem classification "MAES-2" as html for web page

for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "MAES-2"
let $name := $ecosystem/name
let $scheme := $ecosystem/ecosystem-classification[@name = "MAES-2"]/name

return (
    <h2>{$name/text()}</h2>,
    <p>Ecosystem-type <i>{$name/text()}</i> of the EU-habitat assessment is classified as ''{$scheme/text()}'' according to the MAES-2 classification</p>
)

####
## This query returns the list of ecosystems that have been tagged with ecosystem classification "MAES-2" as csv for further analysis

for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "MAES-2"
let $name := $ecosystem/name
let $scheme := $ecosystem/ecosystem-classification[@name = "MAES-2"]/name

return (
    "",$name/text(),", ",$scheme/text(),"
"
    
)


for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "MAES-2"
let $name := $ecosystem/name
let $scheme := $ecosystem/ecosystem-classification[@name = "MAES-2"]/name





for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "MAES-2"
let $name := $ecosystem/name
return $ecosystem

for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "MAES-2"
let $name := $ecosystem/name
let $scheme := $ecosystem/ecosystem-classification/scheme
return $name


####################
## clasificacion de IUCN -
##   usar consistentemente 'name' and 'scheme' para que funcione

for $ecosystem in db:open('RLE_test1')//ecosystem
where $ecosystem//ecosystem-classification/@name = "IUCN"
let $name := $ecosystem/name
let $scheme := $ecosystem/ecosystem-classification[@name = "IUCN"]/scheme

order by $scheme
return (
    "",$scheme/text(),", ",$name/text(),"
"
    
)
#################

## Xpath expression
//ecosystem//ecosystem-classification[@name="IUCN"]/../name 


## Xpath for category with attributes
//category[@criterion="C" and @subcriterion="2"]/../ecosystem-id

//ecosystem//ec-classification[name="IUCN"]/../name
//category[@criterion="C" and @subcriterion="2"]/../../../ecosystem-id
//ecosystem[@id="MAC_RLE_Chile_2015_57"]/name
//ecosystem[@id=data(//category[@criterion="C" and @subcriterion="2"]/../../../ecosystem-id)]/name

## follow tutorial in
##http://www.ffzg.unizg.hr/klafil/dokuwiki/doku.php/z:basex-adv

//*:Risk-Assessment[*:criterion/*:subcriterion/*:category = "LC"]/ecosystem-id

for $v in //*:Risk-Assessment[*:criterion/*:subcriterion/*:category = "CR"]/ecosystem-id
return //ecosystem[@id=$v]/name 

##esta funciona bien... criterio A cualquier subcriterio Vulnerable
for $v in //*:Risk-Assessment[*:criterion[@name = "A"]/*:subcriterion/*:category = "VU"]/ecosystem-id
return //ecosystem[@id=$v]/name 



for $v in //category[@criterion="C" and @subcriterion="2"]/../../../ecosystem-id
where 
return 

