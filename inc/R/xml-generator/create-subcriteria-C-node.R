# Categories for subcriterion
rD <- function(CAT,Mean.Sev,PB.Sev=NA,Sev.30,Sev.50,Sev.80,th=c(30,50,80)) {
  switch(CAT,
      NE=sprintf("This subcriterion was not evaluated."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("Mean relative severity was %s%s%%, and it did not reach any of the thresholds for Vulnerable.",Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev))),
      NT=sprintf("Mean relative severity was %s%s%%, but the estimates of relative severity and extent were close to the thresholds for Vulnerable, so it was considered to be Near Threatened",Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev))),
      VU=sprintf("Mean relative severity was %s%s%%. %s, exceeding the threshold for Vulnerable.", Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[1], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), ifelse(Sev.50>th[2], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]),sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.30,th[1])))),
      EN=sprintf("Mean relative severity was %s%s%%. %s, exceeding the threshold for Endangered.", Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[2], sprintf("Up to%s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]))),
      CR=sprintf("Mean relative severity was %s%s%% and %s %% of the extent had a relative severity of %s %% or higher, exceeding the threshold for Critically Endangered.", Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)),Sev.80,th[3]))
}


##C1

C1.rationale <- "No suitable indicator was evaluated for this period of time."
C1.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("The data used to estimate deterioration of environmental conditions was relevant to present and future  time frames, but not applicable to historic or past time frames. Alternative data sources need to be considered for assessing this subcriterion."))
C1 <- newXMLNode("Subcriterion",attrs=list(name="C1"),
  children=list(newXMLNode("Summaries",children=list(C1.summary)),
  ##    newXMLNode("Rationale",C1.rationale),
    newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),C1.rationale)),
    newXMLNode("Category","NE")))

##C2a
C2a.rationale <- with(assess.fun,
  rD(assess.total$C2a,best.estimate.mean.severity.ClimateChange.2000.2050,
     bounds.estimate.mean.severity.ClimateChange.2000.2050,
    extent.with.severity.30.or.higher.Climate.Change.2000.2050,
      extent.with.severity.50.or.higher.Climate.Change.2000.2050,
        extent.with.severity.80.or.higher.Climate.Change.2000.2050)
)


C2a.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered climate change as a plausible future threat of degradation of environmental conditions as it might influence both the predominant growth form and characteristic biota. We used a set of 19 standard bioclimatic variables as indicators of characteristic environmental conditions (Hijmans et al. 2005)⁠ and used a Random Forest Classification algorithm to evaluate suitability of environmental conditions under current climate and future projections based on climate change models. We evaluated the probability of the target macrogroup to be replaced by any forest or non-forest vegetation macrogroup under changing climatic conditions. %s", C2a.rationale))

if (assess.total$C2a %in% "DD") {
   C2a.IndValues <- data.frame(Unit="",Year=2050,Value="",stringsAsFactors=F)
## newXMLNode("Values","Model performance on current data was not good enough, and no future predictions were attempted")
} else {
   xtn <- as.vector(t(unname(assess.fun[,sprintf("extent.with.severity.%s.or.higher.Climate.Change.2000.2050",c(30,50,80))])))

   C2a.IndValues <- data.frame(Unit="%",Year=2050,Extent=xtn, Severity=c(">30",">50",">80"),stringsAsFactors=F)
}

C2a.variable <-
 makeKeyIndicatorVariable(
   dataName="Changes in climate conditions",
   dataDesc = "We selected a group of 19 standard bioclimatic variables as indicators of overall environmental conditions  and compared current and future conditions. “Current” climate is based on interpolation of weather station data from a reference time frame between 1960 and 1990 (Worldclim, v1.4; Hijmans et al. 2005)⁠; while future scenarios for the year 2050 were calculated for different combinations of global circulation models (GCM) and representative concentration pathways (RCP; IPCC 2014)⁠. All data was available in the same format, resolution and from the same source (http://worldclim.org). We used the *random forest* (RF) algorithm, which is a machine learning method based on an ensemble of classification trees (Liaw & Wiener 2002)⁠ to predict changes from a focal macrogroup to any of the associated forest or non-forest macrogroups. First we define a bounding box around each target macrogroup, and take a spatially uniform random sample of locations. For each location we record the potential macrogroup according to the potential distribution map and the values of the 19 bioclimatic variables. We partition the data in calibration and test subsets (75 and 25% of the locations respectively) using a checkerboard pattern to reduce spatial autocorrelation⁠. Then we fitted the RF model with the calibration data, sample size for each classification tree was set to 2/3 of the calibration data, but allowing for a representative sample of all represented classes, number of variables per tree was set to five, other setting were set to default values. Models were considered accepted if nominal classification error for the focal class was lower than 20% in the test samples, and the area under the sensitivity and specificity curve (AUC) was higher than 85%. The first statistic measures the misclassification when using the modal class from the ensemble prediction which can be a combination of model performance error and natural overlap between related classes⁠, the second one measures discrimination performance when considering only the focal class and  different threshold of model support (proportion of “votes”, or proportion of classification trees assigning the focal class). Future prediction was only attempted when model performance on current data was acceptable. Future predictions were performed for each set of future bioclimatic conditions corresponding to five GCM (BCC-CSM1-1, CCSM4, HadGEM2-AO, HadGEM2-ES and MIROC5) and four RCP (rcp26, rcp45, rcp60 and rcp85). In order to calculate relative severity for the indicator of changing climatic conditions over the whole bounding box we used RF-model prediction (as proportion of votes for the focal class) as a measure of appropriate climatic conditions for the focal macrogroup, and considered the threshold for equal specificity and sensitivity as the threshold at which environmental conditions shift to favor a different macrogroup (collapse threshold, CT). We compared predictions based on current climatic condition (initial value) and predictions based on each combination of GCM and RCP for 2050 (final value). We summarized the results of the 20 combinations of GCMs and RCP to calculate the best estimate and plausible bounds (range including 90% probabilities) for the threat category (Bland et al. 2017)⁠. This indicator measures the expected decline for the next 50 years and thus corresponds to subcriterion C2a.",
   dataSources = c("Hijmans et al. 2005","Liaw & Wiener 2002"),
   dataCT=data.frame(units="",value="We considered the threshold for equal specificity and sensitivity as the threshold at which environmental conditions shift to favor a different macrogroup.",stringsAsFactors=F),
   overallSeverity =  data.frame(units="%",method="projected",value=ifelse(is.na(assess.fun$best.estimate.mean.severity.ClimateChange.2000.2050), "", assess.fun$best.estimate.mean.severity.ClimateChange.2000.2050),stringsAsFactors=F),
   overallExtent =  data.frame(units="%",method="projected",value=100,stringsAsFactors=F),
   # overallExtent =  data.frame(units="%",method="projected",value=with(assess.fun,getOSev(assess.total$C2a,
   #   extent.with.severity.30.or.higher.Climate.Change.2000.2050,
   #     extent.with.severity.50.or.higher.Climate.Change.2000.2050,
   #      extent.with.severity.80.or.higher.Climate.Change.2000.2050)),stringsAsFactors=F),
   # overallSeverity =  data.frame(units="%",method="projected",value=with(assess.fun,getOExt(assess.total$C2a,
   #        extent.with.severity.30.or.higher.Climate.Change.2000.2050,
   #          extent.with.severity.50.or.higher.Climate.Change.2000.2050,
   #           extent.with.severity.80.or.higher.Climate.Change.2000.2050)),stringsAsFactors=F),
   dataValue=C2a.IndValues,
   valueName="projected")


if(is.na(assess.total$bounds.C2a)) {
  PB <-  newXMLNode("Plausible-bounds",attrs=list(lower="",upper=""))
} else {
  PB <-  newXMLNode("Plausible-bounds",attrs=list(lower=strsplit(assess.total$bounds.C2a," -- ")[[1]][1], upper=strsplit(assess.total$bounds.C2a," -- ")[[1]][2]))
}


C2a <- newXMLNode("Subcriterion",attrs=list(name="C2a"),
      children=list(newXMLNode("Summaries",children=list(C2a.summary)),
   ##     newXMLNode("Rationale",C2a.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),C2a.rationale)),
        newXMLNode("Category",assess.total$C2a),
      PB,#  newXMLNode("Bounds",ifelse(is.na(assess.total$bounds.C2a),"",assess.total$bounds.C2a)),
        C2a.variable))


##C2b

C2b.rationale <- with(assess.fun,
  rD(assess.total$C2b,best.estimate.mean.severity.SurfaceWater.19XX.20XX,
     NA,
    extent.with.severity.30.or.higher.Surface.Water.19XX.20XX,
      extent.with.severity.50.or.higher.Surface.Water.19XX.20XX,
        extent.with.severity.80.or.higher.Surface.Water.19XX.20XX)
)


if (assess.total$C2b %in% "NE") {
   C2b.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("The data used to estimate deterioration of environmental conditions was relevant to future  time frames, but not applicable to present, historic or past time frames. Alternative data sources need to be considered for assessing this subcriterion."))
   C2b <- newXMLNode("Subcriterion",attrs=list(name="C2b"),
  children=list(newXMLNode("Summaries",children=list(C2b.summary)),
    newXMLNode("Rationale",C2b.rationale),
    newXMLNode("Category","NE")))

} else {
   C2b.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("For this macrogroup the permanent and temporal water bodies form a relevant element of the ecosystem and interact with characteristic biota (e.g. species tolerant to flooding and high ground water levels) and important processes (e.g. nutrient cycling, vegetation growth)⁠. We considered a global indicator of trends in water surface (Pekel et al. 2016) to assess this subcriterion. %s", C2b.rationale))



   if (assess.total$C2b %in% c("NE","DD")) {
      C2b.IndValues <- data.frame(Unit="",Year=2015,Value="",stringsAsFactors=F)
   ## newXMLNode("Values","Model performance on current data was not good enough, and no future predictions were attempted")
   } else {
      xtn <- as.vector(t(unname(assess.fun[,sprintf("extent.with.severity.%s.or.higher.Surface.Water.19XX.20XX",c(30,50,80))])))
      C2b.IndValues <- data.frame(Unit="%",Year=2015,Extent=xtn, Severity=c(">30",">50",">80"),stringsAsFactors=F)
   }

   C2b.variable <-
    makeKeyIndicatorVariable(
      dataName="Changes in surface water",
      dataDesc = "We used the Global Surface Water Dataset (GSW; Pekel et al. 2016)⁠ to estimate the spatial and temporal change in the distribution of surface water within the potential distribution of all macrogroups related to flood regimes. The GSW Surface Water Occurrence and Occurrence Change Intensity map layers provides information on overall surface water occurrence and increase or decrease of surface water occurrence between two epochs spanning the last 32 years: 1984-1999 and 2000-2015. We overlapped the Macrogroups potential distribution maps and the GSW occurrence layer to calculate the amount of area covered by surface water, and then overlapped the potential distribution maps and the GSW change layer to measure the amount of surface water area lost between epochs. The reference time period (1984-1999) represents the initial value and the recent time period (2000-2015) represent the final value. We set 85% decline as the collapse threshold. We consider that this value represent a reasonable estimate of minimum expected decline for a 50 year window including past, present and future and thus corresponds to subcriterion C2b.",
      dataSources = c("Pekel et al. 2016"),
      dataCT=data.frame(units="% decline in surface water",value=85,stringsAsFactors=F),
      overallSeverity =  data.frame(units="%",method="estimated",value=ifelse(is.na(assess.fun$best.estimate.mean.severity.SurfaceWater.19XX.20XX),"",assess.fun$best.estimate.mean.severity.SurfaceWater.19XX.20XX),stringsAsFactors=F),
      overallExtent =  data.frame(units="%",method="estimated",value=100,stringsAsFactors=F),
   ## alternative apporach
   ##   overallExtent =  data.frame(units="%",method="projected",value=with(assess.fun,getOSev(assess.total$C2a,
   ##     extent.with.severity.30.or.higher.Climate.Change.2000.2050,
   ##       extent.with.severity.50.or.higher.Climate.Change.2000.2050,
   ##        extent.with.severity.80.or.higher.Climate.Change.2000.2050)),stringsAsFactors=F),
   ##   overallSeverity =  data.frame(units="%",method="estimated",value=with(assess.fun,getOExt(assess.total$C2a,
   ##          extent.with.severity.30.or.higher.Climate.Change.2000.2050,
   ##            extent.with.severity.50.or.higher.Climate.Change.2000.2050,
   ##             extent.with.severity.80.or.higher.Climate.Change.2000.2050)),stringsAsFactors=F),
           dataValue=C2b.IndValues,
      valueName="estimated")



   C2b <- newXMLNode("Subcriterion",attrs=list(name="C2b"),
      children=list(newXMLNode("Summaries",children=list(C2b.summary)),
      ##  newXMLNode("Rationale",C2b.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),C2b.rationale)),
        newXMLNode("Category",assess.total$C2b),
       ## no bounds
        C2b.variable))

}

##
C3.rationale <- "No suitable indicator was evaluated for this period of time."
C3.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("The data used to estimate deterioration of environmental conditions was relevant to present and future  time frames, but not applicable to historic or past time frames. Alternative data sources need to be considered for assessing this subcriterion."))
C3 <- newXMLNode("Subcriterion",attrs=list(name="C3"),
  children=list(newXMLNode("Summaries",children=list(C3.summary)),
  newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),C3.rationale)),
##    newXMLNode("Rationale",C3.rationale),
    newXMLNode("Category","NE")))

## overall C


wch <- which.max(cat.weights[unlist(assess.total[,c("C2a","C2b")])])
if  (length(wch)>0) {
   assess.total$C <- assess.total[,c("C2a","C2b")][[wch]]
   C.category <- newXMLNode("Category",assess.total$C)
   C.subcrit <- c("C2a","C2b")[assess.total[,c("C2a","C2b")] %in% assess.total$C]

} else {
   assess.total$C <- "DD"
   C.category <- newXMLNode("Category",assess.total$C)
   C.subcrit <- ""

}

C.rationale <- switch(assess.total$C,
          NE="The subcriteria could not be asssessed, thus under this criterion it is considered Not Evaluated.",
          DD="Assessment outcome was not conclusive, thus under this criterion it is considered Data Deficient.",
          LC="None of the assessed subcriteria met the threshold for Vulnerable, and under this criterion this Macrogroup is considered Least Concern",
          NT=sprintf("Subcriter%s %s %s close to the threshold for Vulnerable, thus under this criterion this Macrogroup is considered Near Threatened",ifelse(length(C.subcrit==1),"ion","ia"),paste(C.subcrit,collapse="and"),ifelse(length(C.subcrit==1),"was","were")),

          VU=sprintf("Subcriter%s %s %s above the threshold for Vulnerable but below the threshold for Endangered, thus under this criterion this Macrogroup is considered Vulnerable", ifelse(length(C.subcrit==1),"ion","ia"),paste(C.subcrit,collapse="and"),ifelse(length(C.subcrit==1),"was","were")),
          EN=sprintf("Subcriter%s %s %s above the threshold for Endangered but below the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Endangered", ifelse(length(C.subcrit==1),"ion","ia"),paste(C.subcrit,collapse="and"),ifelse(length(C.subcrit==1),"was","were")),
          CR=sprintf("Subcriter%s %s %s above the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Critically Endangered", ifelse(length(C.subcrit==1),"ion","ia"),paste(C.subcrit,collapse="and"),ifelse(length(C.subcrit==1),"was","were")))

C.summary <- newXMLNode("Summaries",children=list(newXMLNode("Summary",attrs=list(lang="en"),sprintf("Climate conditions are generally considered of great importance for the maintainance of forest vegetation and characteristic processes of several macrogroups, and were also incorporated in the process of macrogroup potential distribution modeling and validation. Thus we considered climate change as a plausible future threat of degradation of environmental conditions. Natural disturbances are considered within the IVC classification as processes influencing the dominant and characteristic growth forms and diagnostic species of formations, and can be greatly altered by human activities⁠. Flood and fire regimes play an important role in forest formations, but we did not find a reliable indicator for changes in fire regime at the continental scale. %s",ifelse(assess.total$C2b %in% "NE","Flood regime was not considered to be relevant to this ecosystem and was not evaluated.","For this macrogroup permanent and temporal water bodies form a relevant element of the ecosystem and interact with characteristic biota (e.g. species tolerant to flooding and high ground water levels) and important processes (e.g. nutrient cycling, vegetation growth; Keddy et al. 2009)⁠. We considered flood regime and surface water as an important component of these forest macrogroups. A global indicator of trends in water surface was used for this assessment (Pekel et al. 2016)⁠.")))))
#newXMLNode("Rationale",attrs=list(name="E"),"This criterion was not evaluated."),
#newXMLNode("Category","NE")
newXMLNode("Criterion",attrs=list(name="C"),
  children=list(
     C.summary,
     newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),C.rationale)),
 ##  newXMLNode("Rationale",attrs=list(name="C"),C.rationale),
  newXMLNode("Category",C.category),
  newXMLNode("Subcriterions",children=list(C1,C2a,C2b,C3))),
parent=AU.criteria)
