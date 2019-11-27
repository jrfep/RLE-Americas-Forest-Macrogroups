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

D1.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1900 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). %s", D1.rationale))

D3.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1700 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). %s", D3.rationale))


D1.variable <- with(assess.fun,newXMLNode("Key-indicator-variable",attrs=list(name="Resource use intensity"),
  children=list(newXMLNode("Key-indicator",
    children=list(newXMLNode("Indicator-data",
      children=list(newXMLNode("Data-source","Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606.")),
      newXMLNode("Data-description","We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1900 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). We assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential'). We considered equal step weights for changes between consecutive classes, thus a change from 'Natural' to 'Used' or from 'Used' to 'Populated' was represented as RS=25%, while a change from 'Natural' to 'Populated' or from 'Used' to 'Residential' would be represented by RS=50%, etc."),
      newXMLNode("Values",children=list(
        newXMLNode("Value",attrs=list(method="mean estimated"),children=list(
            newXMLNode("Severity",attrs=list(units="%"), best.estimate.mean.severity.LandUseIntensity.1950.2000),
            newXMLNode("Extent",attrs=list(units="%"),'100')
        )),
      newXMLNode("Value",attrs=list(method="estimated"),children=list(
            newXMLNode("Severity",attrs=list(units="%"),">30"),
            newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.30.or.higher.LandUseIntensity.1950.2000)
        )),
        newXMLNode("Value",attrs=list(method="estimated"),children=list(
            newXMLNode("Severity",attrs=list(units="%"),">50"),
            newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.50.or.higher.LandUseIntensity.1950.2000)
        )),
        newXMLNode("Value",attrs=list(method="estimated"),children=list(
            newXMLNode("Severity",attrs=list(units="%"),">80"),
            newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.80.or.higher.LandUseIntensity.1950.2000)
        )))))
      ,
    newXMLNode("Collapse-threshold","We do not have a quantitative estimate of collapse, but assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential')."))))))


    D3.variable <- with(assess.fun,newXMLNode("Key-indicator-variable",attrs=list(name="Resource use intensity"),
      children=list(newXMLNode("Key-indicator",
        children=list(newXMLNode("Indicator-data",
          children=list(newXMLNode("Data-source","Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606.")),
          newXMLNode("Data-description","We estimate changes in resource use intensity by reclassifying the reconstructed land cover classes for 1700 and 2000 from the Anthrome products (Ellis et al. 2010). We crossed the potential distribution of the Macrogroup with the distinct woodland cover classes, and calculated the proportion of area that changed between the two Anthromes layers from low intensity ('wild woodlands' or 'used woodlands') to high intensity of use ('populated woodlands' or 'residential woodlands'). We assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential'). We considered equal step weights for changes between consecutive classes, thus a change from 'Natural' to 'Used' or from 'Used' to 'Populated' was represented as RS=25%, while a change from 'Natural' to 'Populated' or from 'Used' to 'Residential' would be represented by RS=50%, etc."),
          newXMLNode("Values",children=list(
            newXMLNode("Value",attrs=list(method="mean estimated"),children=list(
                newXMLNode("Severity",attrs=list(units="%"), best.estimate.mean.severity.LandUseIntensity.1750.2000),
                newXMLNode("Extent",attrs=list(units="%"),'100')
            )),
          newXMLNode("Value",attrs=list(method="estimated"),children=list(
                newXMLNode("Severity",attrs=list(units="%"),">50"),
                newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.50.or.higher.LandUseIntensity.1750.2000)
            )),
            newXMLNode("Value",attrs=list(method="estimated"),children=list(
                newXMLNode("Severity",attrs=list(units="%"),">70"),
                newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.70.or.higher.LandUseIntensity.1750.2000)
            )),
            newXMLNode("Value",attrs=list(method="estimated"),children=list(
                newXMLNode("Severity",attrs=list(units="%"),">90"),
                newXMLNode("Extent",attrs=list(units="%"),extent.with.severity.90.or.higher.LandUseIntensity.1750.2000)
            )))))
          ,
        newXMLNode("Collapse-threshold","We do not have a quantitative estimate of collapse, but assume that the class 'Natural woodlands' represents natural conditions and the other classes represent increasing amounts of disruption leading to collapse ('Natural' < 'Used' < 'Populated' < 'Residential')."))))))



    D1 <- newXMLNode("Subcriterion",attrs=list(name="D1"),
      children=list(newXMLNode("Summaries",children=list(D1.summary)),
        newXMLNode("Rationale",D1.rationale),
        newXMLNode("Category",assess.total$D1),
        ## D1.bounds, # no bounds estimated
        D1.variable))
    D3 <- newXMLNode("Subcriterion",attrs=list(name="D3"),
          children=list(newXMLNode("Summaries",children=list(D3.summary)),
            newXMLNode("Rationale",D3.rationale),
            newXMLNode("Category",assess.total$D3),
            ## D1.bounds, # no bounds estimated
            D3.variable))


D2a.category <- newXMLNode("Category","NE")
D2b.category <- newXMLNode("Category",assess.total$D2b)

wch <- which.max(cat.weights[unlist(assess.total[,c("D1","D2b","D3")])])
assess.total$D <- assess.total[,c("D1","D2b","D3")][[wch]]
D.category <- newXMLNode("Category",assess.total$D)


D2a <- newXMLNode("Subcriterion",attrs=list(name="D2a"), children=list(D2a.summary,D2a.rationale,D2a.category,D2a.bounds,D2a.variable))
D2b <- newXMLNode("Subcriterion",attrs=list(name="D2b"), children=list(D2b.summary,D2b.rationale,D2b.category,D2b.bounds,D2b.variable))
D3 <- newXMLNode("Subcriterion",attrs=list(name="D3"), children=list(D3.summary,D3.rationale,D3.category,D3.bounds,D3.variable))

D.summary <- newXMLNode("Summaries",children=list(newXMLNode("Summary",attrs=list(lang="en"),"No quantitative assessment of ecosystem collapse was performed for this assessment unit."))),
newXMLNode("Rationale",attrs=list(name="E"),"This criterion was not evaluated."),
newXMLNode("Category","NE")

newXMLNode("Criterion",attrs=list(name="D"),
  children=list(D.summary,D.rationale,D.category,D.bounds,D.subcriteria))
