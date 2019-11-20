## IN THIS VERSION we are only considering one assessment unit: the regional assessment
##ERA.AUs <- newXMLNode("Assessment-units")
##for (AU in AUS) {
##}

## First, set up all Assessment-Unit nodes

AU.subset <- newXMLNode("Ecosystem-subset")
AU.summary <- newXMLNode("RA-Summaries")
AU.ration <- newXMLNode("Rationale")
AU.overall <- newXMLNode("Overall-category")
AU.bounds <- newXMLNode("Plausible-bounds")
AU.support <- newXMLNode("Supporting-subcriteria")
AU.criteria <- newXMLNode("Criterions")
AU.summary

## Now, add information per node

## subset:
#AU.subset
newXMLNode("Subset-descriptions",
  children=list(newXMLNode("Subset-description","This assessment refers to the entire distribution of the assessment target as delimited by the assessment authors.",attrs=list(lang="en"))),
  parent=AU.subset)
  addChildren(AU.subset,kids=list(Countries.node,
     newXMLNode("Spatial-data",children=list(x,y))))

assess.total <- subset(Macrogroups.Global,IVC.macrogroup_key %in% case.study)

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
 assess.spa <- subset(SpatialCriteria.Global,IVC.macrogroup_key %in% case.study)
assess.fun <- subset(FunctionalCriteria.Global,IVC.macrogroup_key %in% case.study)

"This assessment unit was not evaluated due to inconsistency in its distribution data."

##According to the evaluated indicators the threshold for VU was reached for criterion. Uncertainty in the indicators suggests that the overall category can be in the range os NT -- VU.
##
with(assess.spa,
    sprintf("Historical data on woodland cover suggests a historical decline of  %s %% (with plausible bound of %s) since 1750, and a decline of %s %% (%s) between 1950 and 2000", best.estimate.decline.1750.2000,bounds.estimate.decline.1750.2000, best.estimate.decline.1950.2000,bounds.estimate.decline.1950.2000 ))
    with(assess.spa,
      sprintf("According to time series of current forest cover, the predicted rate of decline between 2000 and 2050 could reach up to %s %% (with plausible bound of %s).", best.estimate.decline.2000.2050,bounds.estimate.decline.2000.2050 ))

 newXMLNode("RA-summary",attrs=list(lang="en"),
 sprintf(""),
  parent=AU.summary)
##
AU.ration

##

crit.A <- newXMLNode("Criterion",attrs=list(name="A"),
  parent=AU.criteria)

crit.B <- newXMLNode("Criterion",attrs=list(name="B"),
    parent=AU.criteria)
crit.C <- newXMLNode("Criterion",attrs=list(name="C"),
      parent=AU.criteria)
crit.D <- newXMLNode("Criterion",attrs=list(name="D"),
        parent=AU.criteria)
crit.E <- newXMLNode("Criterion",attrs=list(name="E"),
          parent=AU.criteria)

if (all(assess.total[,c("B1","B2","B3")] %in% "LC")) {
addChildren(crit.B,
   list(newXMLNode("Rationale","EOO and AOO are well above the threshold for Vulnerable."),
      newXMLNode("Category","LC")
      newXMLNode("Subcriterions"))
}


## ensamble together
newXMLNode("Assessment-unit",
  children=list(AU.subset, AU.summary, AU.ration, AU.overall, AU.bounds, AU.support, AU.criteria),
  parent=ERA.AUs)
