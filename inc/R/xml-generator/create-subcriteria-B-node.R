
numerals <- c()
cont.decline <- c()
threat.process <- ""
if (assess.spa$best.estimate.decline.2000.2050>0) {
  numerals <- c(numerals,"i")
  cont.decline <- c(cont.decline, "in spatial extent was inferred from the negative trends in tree cover")
}
if (!is.na(assess.fun$best.estimate.mean.severity.SurfaceWater.19XX.20XX)) {
  numerals <- c(numerals,"ii")
  cont.decline <- c(cont.decline, "in environmental quality is inferred from the negative trends in surface water")
}
if (!is.na(assess.fun$best.estimate.mean.severity.Defaunation.19XX.20XX)) {
  numerals <- c(numerals,"iii")
  cont.decline <- c(cont.decline, "in the disruption of biotic interactions is inferred from the indicator of potential defaunation")
}
if (!is.na(assess.fun$best.estimate.mean.severity.ClimateChange.2000.2050)) {
  threat.process <- "Climate change is a threatening process likely to cause continuing declines in environmental quality."
}

rD <- function(CAT,varname="Extent of Occurrence",th=c("2,000 km^2","20,000 km^2","50,000 km^2"),CD,TP) {
  justs <- unique(c(ifelse(length(CD)>0,sprintf("Continuing decline %s.",CD),""),TP))
  justs <- paste(justs,collapse=" ")
  switch(CAT,
      NE=sprintf("This subcriterion was not evaluated."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("The %s was larger that %s, so it is considered Least Concern.", varname,th[3]),
      NT=sprintf("The %s was close to the thresholds for Vulnerable. %s The ecosystem is considered to be Near Threatened.",varname,justs),
      VU=sprintf("The %s was less than %s but more than %s. %s It is thus considered Vulnerable.", varname,th[3],th[2],justs),
      EN=sprintf("The %s was less than %s but more than %s. %s It is thus considered Endangered.",varname,th[2],th[1],justs),
      CR=sprintf("The %s was smaller than %s. %s It is thus considered Critically Endangered.", varname,th[1],justs),
      CO=sprintf("The %s has probably declined to zero (collapse threshold), but field confirmation is required, thus the ecosystem is considered Critically Endangered with plausible bounds (CR -- CO).", varname,th[1]))
}

rD3 <- function(CAT) {
  switch(CAT,
      NE=sprintf("This subcriterion was not evaluated."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("This ecosystem is present at a large number of locations and not exposed to severe and widespread threats, it was considered Least Concern."),
      VU=sprintf("The ecosystem is found in less than five threat defined locations and is prone to the effects of severe threats, and thus capable of changing status  within a very short time period. It is thus considered Vulnerable.")
    )
}

##B1
B1.rationale <- with(assess.total,
  rD(as.character(B1),CD=cont.decline,TP=threat.process))

B1.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. For contemporary forest cover we overlaped the potential macrogroup distribution with an averaged percent tree cover from two time series of remote sensing data for the year 2012. We used a cutoff value of 25%% tree cover to determine presence, and estimated the extent of occurrence as the minimum convex polygon around the presence data.  %s", B1.rationale))


B1.variable <-
 makeKeyIndicatorVariable(
   dataName="EOO",
   dataDesc = "For contemporary forest cover we considered two time series of data: the Global Forest Cover based on Landsat mosaics (GFC, version 1.2, Hansen et al. 2013)⁠  and the land cover data derived from MODIS  (MODIS LC, version 5.1, Friedl et al. 2010)⁠⁠. GFC data represents estimates of percentage tree cover in 2000 and estimates of forest loss and gain between 2000 and 2014, from which we calculated total forest cover per year. MODIS LC data represents yearly estimates of land cover classes between 2001 and 2012, including several forest types, semi-natural and artificial land cover. We converted land cover classes to percentage tree cover (100% for natural forest classes, 25% for mosaic vegetation, 0% otherwise). We then averaged percent tree cover from both sources for each year between 2001 and 2012 in order to partially correct over- and under-estimation of forest change due to increases in plantations and other artificial land uses (Song et al. 2014; Tropek et al. 2014)⁠. We used a cutoff value of 25% tree cover to determine presence, and estimated the extent of occurrence as the minimum convex polygon around the presence data. ",
   dataSources = c("Hansen et al. 2013","Friedl et al. 2010"),
   dataCT=data.frame(units="km2",value=0,stringsAsFactors=F),
   overallExtent =  data.frame(units="km2",method="estimated",value="",stringsAsFactors=F),
   dataValue=data.frame(Unit="km2",Year=2012,Value=switch(assess.total$B1,LC=">50000",VU="<50000",EN="<20000",CR="<2000",NE=""),stringsAsFactors=F),
   valueName="estimated")

 PB <-  newXMLNode("Plausible-bounds",attrs=list(lower="",upper=""))

B1 <- newXMLNode("Subcriterion",attrs=list(name="B1"),
      children=list(newXMLNode("Summaries",children=list(B1.summary)),
      ##  newXMLNode("Rationale",B1.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),B1.rationale)),
        newXMLNode("Category",assess.total$B1),
        PB,#newXMLNode("Bounds",""),
        B1.variable,
        newXMLNode("Threat-defined-locations","More than 10"),
        newXMLNode("Continuing-decline", attrs=list(numeral=paste(numerals,collapse=",")), ifelse(length(numerals)>0,"yes","no")),
        newXMLNode("Threatening-process",ifelse(threat.process %in% "","no","yes"))))

## B2

B2.rationale <- with(assess.total,
rD(as.character(B2),varname="Area of Occupancy",th=c("2 10x10 km grid cells","20 10x10 km grid cells","50 10x10 km grid cells"),CD=cont.decline,TP=threat.process))


B2.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. For contemporary forest cover we overlaped the potential macrogroup distribution with an averaged percent tree cover from two time series of remote sensing data for the year 2012. We used a cutoff value of 25%% tree cover to determine presence, and counted the number of 10x10 km cells that had at least 1%% of their area occupied the the ecosystem.  %s", B2.rationale))

B2.variable <-
 makeKeyIndicatorVariable(
   dataName="AOO",
   dataDesc = "For contemporary forest cover we considered two time series of data: the Global Forest Cover based on Landsat mosaics (GFC, version 1.2, Hansen et al. 2013)⁠  and the land cover data derived from MODIS  (MODIS LC, version 5.1, Friedl et al. 2010)⁠⁠. GFC data represents estimates of percentage tree cover in 2000 and estimates of forest loss and gain between 2000 and 2014, from which we calculated total forest cover per year. MODIS LC data represents yearly estimates of land cover classes between 2001 and 2012, including several forest types, semi-natural and artificial land cover. We converted land cover classes to percentage tree cover (100% for natural forest classes, 25% for mosaic vegetation, 0% otherwise). We then averaged percent tree cover from both sources for each year between 2001 and 2012 in order to partially correct over- and under-estimation of forest change due to increases in plantations and other artificial land uses (Song et al. 2014; Tropek et al. 2014)⁠. We used a cutoff value of 25% tree cover to determine presence, overlaped this with a 10x10 km grid, and counted the number of 10x10 km cells that had at least 1% of their area occupied the the ecosystem.",
   dataSources = c("Hansen et al. 2013","Friedl et al. 2010"),
   dataCT=data.frame(units="km2",value=0,stringsAsFactors=F),
   overallExtent =  data.frame(units="10x10km cells",method="estimated",value="",stringsAsFactors=F),
   dataValue=data.frame(Unit="10x10km cells", Year=2012, Value=switch(assess.total$B1,LC=">50",VU="<50",EN="<20",CR="<2",NE=""), stringsAsFactors=F),
   valueName="estimated")



B2 <- newXMLNode("Subcriterion",attrs=list(name="B2"),
      children=list(newXMLNode("Summaries",children=list(B2.summary)),
      ##  newXMLNode("Rationale",B2.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),B2.rationale)),
        newXMLNode("Category",assess.total$B2),
        PB,#newXMLNode("Bounds",""),
        B2.variable,
        newXMLNode("Threat-defined-locations","More than 10"),
        newXMLNode("Continuing-decline", attrs=list(numeral=paste(numerals,collapse=",")), ifelse(length(numerals)>0,"yes","no")),
        newXMLNode("Threatening-process",ifelse(threat.process %in% "","no","yes"))))

## B3
B3.rationale <- with(assess.total,
            rD3(as.character(B3)))

B3.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. Threat defined locations and threatening processes were assesed qualitatively.  %s", B3.rationale))

B3 <- newXMLNode("Subcriterion",attrs=list(name="B3"),
      children=list(newXMLNode("Summaries",children=list(B3.summary)),
       ## newXMLNode("Rationale",B3.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),B3.rationale)),
        newXMLNode("Category",assess.total$B3),
        PB,#newXMLNode("Bounds",""),
        ##NO B3.variable,
        newXMLNode("Threat-defined-locations","More than 10"),
        newXMLNode("Continuing-decline", attrs=list(numeral=paste(numerals,collapse=",")), ifelse(length(numerals)>0,"yes","no")),
        newXMLNode("Threatening-process",ifelse(threat.process %in% "","no","yes"))))


## overall B


wch <- which.max(cat.weights[unlist(assess.total[,c("B1","B2","B3")])])
assess.total$B <- assess.total[,c("B1","B2","B3")][[wch]]
B.category <- newXMLNode("Category",assess.total$B)

subcrits <- c(sprintf(c("B1%s%s","B2%s%s"),ifelse(length(numerals)>0,
  sprintf("a(%s)",paste(numerals,collapse=",")),""),
  ifelse(threat.process %in% "","","b")),"B3")

B.subcrit <- c(subcrits)[assess.total[,c("B1","B2","B3")] %in% assess.total$B]


B.rationale <- switch(assess.total$B,
          NE="The subcriteria could not be asssessed, thus under this criterion it is considered Not Evaluated.",
          DD="Assessment outcome was not conclusive, thus under this criterion it is considered Data Deficient.",
          LC="None of the assessed subcriteria met the threshold for Vulnerable, and under this criterion this Macrogroup is considered Least Concern",
          NT=sprintf("Subcriter%s %s %s close to the threshold for Vulnerable, thus under this criterion this Macrogroup is considered Near Threatened",ifelse(length(B.subcrit==1),"ion","ia"),paste(B.subcrit,collapse="and"),ifelse(length(B.subcrit==1),"was","were")),

          VU=sprintf("Subcriter%s %s %s above the threshold for Vulnerable but below the threshold for Endangered, thus under this criterion this Macrogroup is considered Vulnerable", ifelse(length(B.subcrit==1),"ion","ia"),paste(B.subcrit,collapse="and"),ifelse(length(B.subcrit==1),"was","were")),
          EN=sprintf("Subcriter%s %s %s above the threshold for Endangered but below the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Endangered", ifelse(length(B.subcrit==1),"ion","ia"),paste(B.subcrit,collapse="and"),ifelse(length(B.subcrit==1),"was","were")),
          CR=sprintf("Subcriter%s %s %s above the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Critically Endangered", ifelse(length(B.subcrit==1),"ion","ia"),paste(B.subcrit,collapse="and"),ifelse(length(B.subcrit==1),"was","were")))

B.summary <- newXMLNode("Summaries",children=list(newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. For contemporary forest cover we overlaped the potential macrogroup distribution with an averaged percent tree cover from two time series of remote sensing data for the year 2012. We used a cutoff value of 25%% tree cover to determine presence, and estimated the extent of occurrence using a convex hull polygon, and area of occupancy using a 10x10 grid. %s",B.rationale))))
#newXMLNode("Rationale",attrs=list(name="E"),"This criterion was not evaluated."),
#newXMLNode("Category","NE")
newXMLNode("Criterion",attrs=list(name="B"),
  children=list(
     B.summary,
##  newXMLNode("Rationale",attrs=list(name="B"),B.rationale),
  newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),B.rationale)),
  newXMLNode("Category",B.category),
  newXMLNode("Subcriterions",children=list(B1,B2,B3))),
parent=AU.criteria)
