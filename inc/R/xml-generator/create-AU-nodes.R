## IN THIS VERSION we are only considering one assessment unit: the regional assessment
##ERA.AUs <- newXMLNode("Assessment-units")
##for (AU in AUS) {
##}

## First, set up all Assessment-Unit nodes

##ERA.AUs <- newXMLNode("Assessment-units") # THis is not needed anymore

AU.subset <- newXMLNode("Ecosystem-subset")
AU.summary <- newXMLNode("RA-Summaries")
AU.ration <- newXMLNode("Rationale")
AU.overall <- newXMLNode("Overall-category")
AU.bounds <- newXMLNode("Plausible-bounds")
AU.support <- newXMLNode("Supporting-subcriteria")
AU.criteria <- newXMLNode("Criterions")

## summaries prepared for NatureServe :
newXMLNode("RA-Summary",subset(OverallCat,IVC.macrogroup_key %in% case.study)$Overall.rationale,attrs=list(lang="en"),parent=AU.summary)



## Now, add information per node

## subset:
#AU.subset
newXMLNode("Subset-descriptions",
  children=list(newXMLNode("Subset-description","This assessment refers to the entire distribution of the assessment target as delimited by the assessment authors.",attrs=list(lang="en"))),
  parent=AU.subset)
  addChildren(AU.subset,kids=list(Countries.node,
     newXMLNode("Spatial-data",children=list(x,y))))

##AU.overall
xmlValue(AU.overall) <- assess.total$Overall.Category

##AU.bounds
if (!is.na(assess.total$Overall.Bounds)) {
  bb <- strsplit(assess.total$Overall.Bounds," -- ")[[1]]
  xmlAttrs(AU.bounds) <- list(lower=bb[1],upper=bb[2])
}
##
xmlValue(AU.support) <- gsub(", ","+",assess.total$Threat.criteria)

 ##AU.summary

##"This assessment unit was not evaluated due to inconsistency in its distribution data."

##According to the evaluated indicators the threshold for VU was reached for criterion. Uncertainty in the indicators suggests that the overall category can be in the range os NT -- VU.
##
##with(assess.spa,
##    sprintf("Historical data on woodland cover suggests a historical decline of  %s %% (with plausible bound of %s) since 1750, and a decline of %s %% (%s) between 1950 and 2000", best.estimate.decline.1750.2000,bounds.estimate.decline.1750.2000, best.estimate.decline.1950.2000,bounds.estimate.decline.1950.2000 ))
##    with(assess.spa,
##      sprintf("According to time series of current forest cover, the predicted rate of decline between 2000 and 2050 could reach up to %s %% (with plausible bound of %s).", best.estimate.decline.2000.2050,bounds.estimate.decline.2000.2050 ))



#if (all(assess.total[,c("B1","B2","B3")] %in% "LC")) {
#addChildren(crit.B,#
#   list(newXMLNode("Rationale","EOO and AOO are well above the threshold for Vulnerable."),
#      newXMLNode("Category","LC"),
#      newXMLNode("Subcriterions"))
#}




makeKeyIndicatorVariable <- function(dataName,dataDesc,dataSources,dataCT,overallExtent,overallSeverity=NULL,dataValue,valueName) {
   IDV <- newXMLNode("Indicator-data")

   newXMLNode("Data-descriptions", children=newXMLNode("Data-description",attrs=list(lang="en"),dataDesc), parent=IDV)
   for (dS in dataSources)
      newXMLNode("Data-source",dS,parent=IDV)
   ValueList <- newXMLNode("Values", attrs=list(method=valueName),parent=IDV)
   if ("Severity" %in% colnames(dataValue)) {
      for (k in 1:nrow(dataValue))
         newXMLNode("Value", attrs=list(level=k),children=list(
            newXMLNode("Year",dataValue[k,"Year"]),
            newXMLNode("Indicator-value", attrs=list(units="")),
            newXMLNode("Extent", attrs=list(units=dataValue[k,"Unit"]),dataValue[k,"Extent"]),
            newXMLNode("Severity", attrs=list(units=dataValue[k,"Unit"]),dataValue[k,"Severity"])
         ),parent=ValueList)
   } else {
      for (k in 1:nrow(dataValue))
         newXMLNode("Value", attrs=list(level=k),children=list(newXMLNode("Year",dataValue[k,"Year"]), newXMLNode("Indicator-value", attrs=list(units=dataValue[k,"Unit"]),dataValue[k,"Value"])),parent=ValueList)
   }

   KI <- newXMLNode("Key-indicator",attrs=list(name=dataName),children=newXMLNode("Indicator-data-values",children=IDV))

   OE <- newXMLNode("Overall-Extent",attrs=list(units=overallExtent$units,method=overallExtent$method),overallExtent$value,parent=KI)
   if (!is.null(overallSeverity))
      newXMLNode("Overall-Severity",attrs=list(units=overallSeverity$units,method=overallSeverity$method),overallSeverity$value, parent=KI)
   CT <- newXMLNode("Collapse-threshold",attrs=list(units=dataCT$units),dataCT$value,parent=KI)

   KIV <- newXMLNode("Key-indicator-variables",
          children=KI)

   return(KIV)
}

    getOExt <- function(CAT,Sev.30,Sev.50,Sev.80,th=c(30,50,80)) {
      switch(CAT,
          NE="",
          DD="",
          LC=Sev.30,
          NT=ifelse(Sev.80>(th[1]*.9), Sev.80, ifelse(Sev.50>(th[2]*.9), Sev.50, Sev.30)),
          VU=ifelse(Sev.80>th[1], Sev.80, ifelse(Sev.50>th[2], Sev.50, Sev.30)),
          EN=ifelse(Sev.80>th[2], Sev.80, Sev.50),
          CR=Sev.80)
        }


    getOSev <- function(CAT,Sev.30,Sev.50,Sev.80,th=c(30,50,80)) {
          switch(CAT,
              NE="",
              DD="",
              LC=30,
              NT=ifelse(Sev.80>(th[1]*.9), 80, ifelse(Sev.50>(th[2]*.9), 50, 30)),
              VU=ifelse(Sev.80>th[1], 80, ifelse(Sev.50>th[2], 50,30)),
              EN=ifelse(Sev.80>th[2], 80, 50),
              CR=80)
            }

source(sprintf("%s/create-subcriteria-A-node.R",inc.dir),verbose=F)

  source(sprintf("%s/create-subcriteria-B-node.R",inc.dir))
##crit.B <- newXMLNode("Criterion",attrs=list(name="B"),
##    parent=AU.criteria)


    source(sprintf("%s/create-subcriteria-C-node.R",inc.dir))
source(sprintf("%s/create-subcriteria-D-node.R",inc.dir))
source(sprintf("%s/create-subcriteria-E-node.R",inc.dir))

## ensamble together
ERA.AUs <- newXMLNode("Assessment-unit",
  children=list(AU.subset, AU.summary, AU.ration, AU.overall, AU.bounds, AU.support, AU.criteria))
