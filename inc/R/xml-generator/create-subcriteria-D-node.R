# Categories for subcriterion
rD <- function(CAT,Mean.Sev,Sev.30,Sev.50,Sev.80,th=c(30,50,80)) {
  switch(CAT,
      NE=sprintf("This subcriterion was not evaluated."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("Mean relative severity was %s %%, and it did not reach any of the thresholds for Vulnerable.",Mean.Sev),
      NT=sprintf("Mean relative severity was %s %%, but the estimates of relative severity and extent were close to the thresholds for Vulnerable, so it was considered to be Near Threatened",Mean.Sev),
      VU=sprintf("Mean relative severity was %s. %s, exceeding the threshold for Vulnerable.", Mean.Sev, ifelse(Sev.80>th[1], sprintf("%s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), ifelse(Sev.50>th[2], sprintf("%s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]),sprintf("%s %% of the extent had a relative severity of %s %% or higher", Sev.30,th[1])))),
      EN=sprintf("Mean relative severity was %s. %s, exceeding the threshold for Endangered.", Mean.Sev, ifelse(Sev.80>th[2], sprintf("%s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), sprintf("%s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]))),
      CR=sprintf("Mean relative severity was %s and %s %% of the extent had a relative severity of %s %% or higher, exceeding the threshold for Critically Endangered.", Sev.80,th[3]))
}
D1.rationale <- with(assess.fun,
  rD(as.character(D1),best.estimate.mean.severity.LandUseIntensity.1950.2000,
    extent.with.severity.30.or.higher.LandUseIntensity.1950.2000,
      extent.with.severity.50.or.higher.LandUseIntensity.1950.2000,
        extent.with.severity.80.or.higher.LandUseIntensity.1950.2000)
)

D3.rationale <- with(assess.fun,
  rD(as.character(D3),best.estimate.mean.severity.LandUseIntensity.1750.2000,
    extent.with.severity.50.or.higher.LandUseIntensity.1750.2000,
      extent.with.severity.70.or.higher.LandUseIntensity.1750.2000,
        extent.with.severity.90.or.higher.LandUseIntensity.1750.2000,th=c(50,70,90))
)
D2a.rationale <- "No suitable indicator was evaluated for this period of time."

D2b.rationale <- with(assess.fun,
  rD(as.character(D2b),best.estimate.mean.severity.Defaunation.19XX.20XX,
    extent.with.severity.30.or.higher.Defaunation.19XX.20XX,
      extent.with.severity.50.or.higher.Defaunation.19XX.20XX,
        extent.with.severity.80.or.higher.Defaunation.19XX.20XX)
)

D1.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1900 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). %s", D1.rationale))

D2a.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("The data used to estimate disruption of biotic processes was relevant to past and present time frames, but not applicable to future time frames. Alternative data sources need to be considered for assessing this subcriterion.", D2b.rationale))

D2b.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We applied a simplified formula relating response ratios (RR) of the abundance of mammals in hunted and unhunted sites with three variables: distance to access points for hunters, distance to markets for wildlife products⁠ and level of protection (Benítez-López et al. 2017). We then transformed the spatial estimate of RR to an estimate of potential decline in mammal abundance as a percentage. An overall weighted estimate of percentage decline was calculated over the potential distribution of each Macrogroup, using mean forest cover between 2001 and 2012 as a weighting variable. %s", D2b.rationale))

D3.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1700 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). %s", D3.rationale))

if (assess.total$D1 %in% c("NE","DD")) {
   D1.IndValues <- data.frame(Unit="",Year=2000,Value="",stringsAsFactors=F)
## newXMLNode("Values","Model performance on current data was not good enough, and no future predictions were attempted")
} else {
   xtn <- as.vector(t(unname(assess.fun[,sprintf("extent.with.severity.%s.or.higher.LandUseIntensity.1950.2000",c(30,50,80))])))
   D1.IndValues <- data.frame(Unit="%",Year=2000,Extent=xtn, Severity=c(">30",">50",">80"),stringsAsFactors=F)
}

D1.variable <-
makeKeyIndicatorVariable(
   dataName="Resource use intensity",
   dataDesc = "We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1900 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). We assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential'). We considered equal step weights for changes between consecutive classes, thus a change from 'Natural' to 'Used' or from 'Used' to 'Populated' was represented as RS=25%, while a change from 'Natural' to 'Populated' or from 'Used' to 'Residential' would be represented by RS=50%, etc. We do not have a quantitative estimate of collapse, but assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential').",
   dataSources = c("Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606."),
   dataCT=data.frame(units="%",value="",stringsAsFactors=F),
   overallSeverity =  data.frame(units="%",method="inferred",value=ifelse(is.na(assess.fun$best.estimate.mean.severity.LandUseIntensity.1950.2000),"",assess.fun$best.estimate.mean.severity.LandUseIntensity.1950.2000),stringsAsFactors=F),
   overallExtent =  data.frame(units="%",method="inferred",value=100,stringsAsFactors=F),
        dataValue=D1.IndValues,
   valueName="inferred")


   if (assess.total$D3 %in% c("NE","DD")) {
      D3.IndValues <- data.frame(Unit="",Year=2000,Value="",stringsAsFactors=F)
   ## newXMLNode("Values","Model performance on current data was not good enough, and no future predictions were attempted")
   } else {
      xtn <- as.vector(t(unname(assess.fun[,sprintf("extent.with.severity.%s.or.higher.LandUseIntensity.1750.2000",c(50,70,90))])))

      D3.IndValues <- data.frame(Unit="%",Year=2000,Extent=xtn, Severity=c(">50",">70",">90"),stringsAsFactors=F)
   }

   D3.variable <-
   makeKeyIndicatorVariable(
      dataName="Resource use intensity",
      dataDesc = "We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1700 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). We assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential'). We considered equal step weights for changes between consecutive classes, thus a change from 'Natural' to 'Used' or from 'Used' to 'Populated' was represented as RS=25%, while a change from 'Natural' to 'Populated' or from 'Used' to 'Residential' would be represented by RS=50%, etc. We do not have a quantitative estimate of collapse, but assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential').",
      dataSources = c("Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606."),
      dataCT=data.frame(units="%",value="",stringsAsFactors=F),
      overallSeverity =  data.frame(units="%",method="inferred",value=ifelse(is.na(assess.fun$best.estimate.mean.severity.LandUseIntensity.1750.2000),"",assess.fun$best.estimate.mean.severity.LandUseIntensity.1750.2000),stringsAsFactors=F),
      overallExtent =  data.frame(units="%",method="inferred",value=100,stringsAsFactors=F),
           dataValue=D3.IndValues,
      valueName="inferred")


      if (assess.total$D2b %in% c("NE","DD")) {
         D2b.IndValues <- data.frame(Unit="",Year=2000,Value="",stringsAsFactors=F)
      ## newXMLNode("Values","Model performance on current data was not good enough, and no future predictions were attempted")
      } else {
         xtn <- as.vector(t(unname(assess.fun[,sprintf("extent.with.severity.%s.or.higher.Defaunation.19XX.20XX",c(30,50,80))])))

         D2b.IndValues <- data.frame(Unit="%",Year=2000,Extent=xtn, Severity=c(">30",">50",">80"),stringsAsFactors=F)
      }

      D2b.variable <-
      makeKeyIndicatorVariable(
         dataName="Potential defaunation",
         dataDesc = "We applied a simplified formula relating response ratios (RR) of the abundance of mammals in hunted and unhunted sites with three variables: distance to access points for hunters (approximated by distance in km to the  main road network, from Digital Chart of the World), distance to markets for wildlife products (approximated by the distance in km to major urban centers, calculated from urban land cover in MODIS LC, version 5.1, Friedl et al. 2010)⁠ and level of protection (approximated by the distribution of any kind of protected areas, from the World Database on Protected Areas, IUCN and UNEP-WCMC 2016). According to figures and detailed results from (Benítez-López et al. 2017)⁠⁠, we used the following formula to estimate the expected RR from raster images representing the three variables: RR = -2.370 + 0.057 access + 0.010 market + 1.270 protected . We then transformed the spatial estimate of RR to an estimate of potential decline in mammal abundance as a percentage . For negative RR values we estimated the decline as: decline = (1-exp(RR)) * 100. For zero or positive RR values we assumed zero decline. An overall weighted estimate of percentage decline was calculated over the potential distribution of each Macrogroup, using mean forest cover between 2001 and 2012 as a weighting variable. We assume that the applied formula represents the potential relative decline in mammal abundance and used a collapse threshold equivalent of 10% of the original abundance. We further assume that the formula represents a current level of threat representative a 50 year window including past, present and future.",
         dataSources = c("Benítez-López, A., Alkemade, J.R.M., Schipper, A.M., Ingram, D.J., Verweij, P.A., Eikelboom, J. & Huijbregts, M. (2017). The impact of hunting on tropical mammal and bird populations. Science (80-. )., 356, 180–183.","Digital Chart of the World: road network per country","Friedl, M.A., Sulla-Menashe, D., Tan, B., Schneider, A., Ramankutty, N., Sibley, A. & Huang, X. (2010). MODIS Collection 5 global land cover: Algorithm refinements and characterization of new datasets, 2001-2012, Collection 5.1 IGBP Land Cover.","World Database on Protected Areas, IUCN and UNEP-WCMC 2016"),
         dataCT=data.frame(units="% of the original abundance",value="10",stringsAsFactors=F),
         overallSeverity =  data.frame(units="%",method="inferred",value=ifelse(is.na(assess.fun$ best.estimate.mean.severity.Defaunation.19XX.20XX),"",assess.fun$ best.estimate.mean.severity.Defaunation.19XX.20XX),stringsAsFactors=F),
         overallExtent =  data.frame(units="%",method="inferred",value=100,stringsAsFactors=F),
              dataValue=D2b.IndValues,
         valueName="inferred")




        D2a.category <- newXMLNode("Category","NE")
        D2b.category <- newXMLNode("Category",assess.total$D2b)


    D1 <- newXMLNode("Subcriterion",attrs=list(name="D1"),
      children=list(newXMLNode("Summaries",children=list(D1.summary)),
   ##     newXMLNode("Rationale",D1.rationale),
        newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),D1.rationale)),
        newXMLNode("Category",assess.total$D1),
        ## D1.bounds, # no bounds estimated
        D1.variable))


    D3 <- newXMLNode("Subcriterion",attrs=list(name="D3"),
          children=list(newXMLNode("Summaries",children=list(D3.summary)),
            ##newXMLNode("Rationale",D3.rationale),
            newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),D3.rationale)),
            newXMLNode("Category",assess.total$D3),
            ## D1.bounds, # no bounds estimated
            D3.variable))


   D2a <- newXMLNode("Subcriterion",attrs=list(name="D2a"),
         children=list(newXMLNode("Summaries",children=list(D2a.summary)),
          ## newXMLNode("Rationale",D2a.rationale),
           newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),D2a.rationale)),
           newXMLNode("Category",assess.total$D2a))) ## NE

  D2b <- newXMLNode("Subcriterion",attrs=list(name="D2b"),
        children=list(newXMLNode("Summaries",children=list(D2b.summary)),
         ## newXMLNode("Rationale",D2b.rationale),
          newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),D2b.rationale)),
          newXMLNode("Category",assess.total$D2b),
          ## D1.bounds, # no bounds estimated
          D2b.variable))

wch <- which.max(cat.weights[unlist(assess.total[,c("D1","D2b","D3")])])
assess.total$D <- ifelse(all(is.na(wch)),"NE", assess.total[,c("D1","D2b","D3")][[wch]])
D.category <- newXMLNode("Category",assess.total$D)
D.subcrit <- c("D1","D2b","D3")[assess.total[,c("D1","D2b","D3")] %in% assess.total$D]

D.rationale <- switch(assess.total$D,
          NE="The subcriteria could not be asssessed, thus under this criterion it is considered Not Evaluated.",
          DD="Assessment outcome was not conclusive, thus under this criterion it is considered Data Deficient.",
          LC="None of the assessed subcriteria met the threshold for Vulnerable, and under this criterion this Macrogroup is considered Least Concern",
          NT=sprintf("Subcriter%s %s %s close to the threshold for Vulnerable, thus under this criterion this Macrogroup is considered Near Threatened",ifelse(length(D.subcrit==1),"ion","ia"),paste(D.subcrit,collapse="and"),ifelse(length(D.subcrit==1),"was","were")),

          VU=sprintf("Subcriter%s %s %s above the threshold for Vulnerable but below the threshold for Endangered, thus under this criterion this Macrogroup is considered Vulnerable", ifelse(length(D.subcrit==1),"ion","ia"),paste(D.subcrit,collapse="and"),ifelse(length(D.subcrit==1),"was","were")),
          EN=sprintf("Subcriter%s %s %s above the threshold for Endangered but below the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Endangered", ifelse(length(D.subcrit==1),"ion","ia"),paste(D.subcrit,collapse="and"),ifelse(length(D.subcrit==1),"was","were")),
          CR=sprintf("Subcriter%s %s %s above the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Critically Endangered", ifelse(length(D.subcrit==1),"ion","ia"),paste(D.subcrit,collapse="and"),ifelse(length(D.subcrit==1),"was","were")))

D.summary <- newXMLNode("Summaries",children=list(newXMLNode("Summary",attrs=list(lang="en"),"For criterion D we considered that large vertebrates (and specially large mammals) are a characteristic component of forests, and their due to overexploitation has an impact in important biotic associations in ecosystems (Galetti & Dirzo 2013; Dirzo et al. 2014)⁠.  We used recent estimates of hunting-depletion distances to assess potential ecosystem degradation severity and extent in tropical regions (Benítez-López et al. 2017)⁠. We did not find reliable indicators of other important threatening processes like invasive species or selective logging at a continental scale, but we considered that increased human use intensity could be used as indirect indicator of those (Ellis et al. 2010).")))
#newXMLNode("Rationale",attrs=list(name="E"),"This criterion was not evaluated."),
#newXMLNode("Category","NE")
newXMLNode("Criterion",attrs=list(name="D"),
  children=list(
     D.summary,
     ##newXMLNode("Rationale",attrs=list(name="D"),D.rationale),
  newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),D.rationale)),
  newXMLNode("Category",D.category),
  newXMLNode("Subcriterions",children=list(D1,D2a,D2b,D3))),
parent=AU.criteria)
