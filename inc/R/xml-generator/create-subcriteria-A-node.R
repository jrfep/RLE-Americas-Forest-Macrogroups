rD <- function(CAT,Mean.Est,PB.Est=NA,th=c(30,50,80)) {
  switch(CAT,
      NE=sprintf("This subcriterion was not evaluated."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("Mean estimated decline was %s%s%%, and it did not reach any of the thresholds for Vulnerable.",Mean.Est,ifelse(is.na(PB.Est),"",sprintf(" (%s) ",PB.Est))),
      NT=sprintf("Mean estimated decline was %s%s%% and was close to the thresholds for Vulnerable, so it was considered to be Near Threatened",Mean.Est,ifelse(is.na(PB.Est),"",sprintf(" (%s) ",PB.Est))),
      VU=sprintf("Mean estimated decline was %s%s%%, exceeding the threshold for Vulnerable, but below the threshold for Endangered.", Mean.Est,ifelse(is.na(PB.Est),"",sprintf(" (%s) ",PB.Est))),
      EN=sprintf("Mean estimated decline was %s%s%%, exceeding the threshold for Endangered, but below the threshold for Critically Endangered.", Mean.Est,ifelse(is.na(PB.Est),"",sprintf(" (%s) ",PB.Est))),
      CR=sprintf("Mean estimated decline was %s%s%%, exceeding the threshold for Critically Endangered.", Mean.Est,ifelse(is.na(PB.Est),"",sprintf(" (%s) ",PB.Est))))
}

##A1
## leer la guia y usar overall extent para A1


A1.rationale <- with(assess.spa,
  rD(as.character(A1), best.estimate.decline.1950.2000, bounds.estimate.decline.1950.2000,th=c(30,50,80)))

A1.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. Historical changes in forest cover were measured by contrasting series of reconstructed land cover classes for 1700, 1800, 1900 and 2000. We used woodland cover to estimate changes in forest cover. The proportional rate of decline (PRD) was estimated by interpolation of the area in the four time frames assuming a quadratic response curve to describe increasing or decreasing rates in the last 100 years. %s", A1.rationale))

A1.variable <-
 makeKeyIndicatorVariable(
   dataName="Change in distribution",
   dataDesc = "Changes in forest cover were measured by contrasting series of spatial layers from different sources of global extent. For historical representation of forest cover we used the Anthrome products (http://ecotope.org/anthromes/v2/data/, v 2.0, Ellis et al. 2010)⁠ which represent the reconstructed extent of different land cover classes for 1700, 1800, 1900 and 2000. We reclassified the layers and focused on woodland cover independent of use intensity (wild, used, populated and residential woodlands) to estimate changes in forest cover. The reclassified maps were crossed with the potential distribution of each Macrogroup in order to obtain an area estimate for each Macrogroup and each time-frame. The proportional rate of decline (PRD) was estimated by interpolation of the area in the four time frames assuming a quadratic response curve to describe increasing or decreasing rates in the last 100 years. The curve was fitted with a generalized linear model fitted using a quasipoisson error distribution and a log-link to avoid negative estimates of area. For this subcriterion we used the curve to estimate the PRD between 1950 and 2000.",
   dataSources = c("Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606."),
   dataCT=data.frame(units="km2",value=0,stringsAsFactors=F),
   overallExtent =  data.frame(units="%",method="estimated",value=assess.spa$best.estimate.decline.1950.2000,stringsAsFactors=F),
   dataValue=data.frame(Unit="km2",Year=c(1950,2000),Value=c("",""),stringsAsFactors=F),
   overallSeverity=NULL,
   valueName="estimated")

     if(is.na(assess.total$bounds.A1)) {
        PB <-  newXMLNode("Plausible-bounds","",attrs=list(lower="",upper=""))
     } else {
        PB <-  newXMLNode("Plausible-bounds",assess.total$bounds.A1,attrs=list(lower=strsplit(assess.total$bounds.A1," -- ")[[1]][1], upper=strsplit(assess.total$bounds.A1," -- ")[[1]][2]))
     }

     A1 <- newXMLNode("Subcriterion",attrs=list(name="A1"),
           children=list(newXMLNode("Summaries",children=list(A1.summary)),
             newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),A1.rationale)),
             newXMLNode("Category",assess.total$A1),
             PB,
             A1.variable))




##A3
A3.rationale <- with(assess.spa,
rD(as.character(A3), best.estimate.decline.1750.2000, bounds.estimate.decline.1750.2000,th=c(50,70,90)))

A3.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. Historical changes in forest cover were measured by contrasting series of reconstructed land cover classes for 1700, 1800, 1900 and 2000. We used woodland cover to estimate changes in forest cover. The proportional rate of decline (PRD) was estimated by interpolation of the area in the four time frames assuming a quadratic response curve to describe increasing or decreasing rates in the last 400 years. %s", A3.rationale))


A3.variable <-
 makeKeyIndicatorVariable(
   dataName="Change in distribution",
   dataDesc = "Changes in forest cover were measured by contrasting series of spatial layers from different sources of global extent. For historical representation of forest cover we used the Anthrome products (http://ecotope.org/anthromes/v2/data/, v 2.0, Ellis et al. 2010)⁠ which represent the reconstructed extent of different land cover classes for 1700, 1800, 1900 and 2000. We reclassified the layers and focused on woodland cover independent of use intensity (wild, used, populated and residential woodlands) to estimate changes in forest cover. The reclassified maps were crossed with the potential distribution of each Macrogroup in order to obtain an area estimate for each Macrogroup and each time-frame. The proportional rate of decline (PRD) was estimated by interpolation of the area in the four time frames assuming a quadratic response curve to describe increasing or decreasing rates in the last 400 years. The curve was fitted with a generalized linear model fitted using a quasipoisson error distribution and a log-link to avoid negative estimates of area. For this subcriterion we used the curve to estimate the PRD between 1750 and 2000.",
   dataSources = c("Ellis, E.C., Goldewijk, K.K., Siebert, S., Lightman, D. & Ramankutty, N. (2010). Anthropogenic transformation of the biomes, 1700 to 2000. Glob. Ecol. Biogeogr., 19, 589–606."),
   dataCT=data.frame(units="km2",value=0,stringsAsFactors=F),
   overallExtent =  data.frame(units="%",method="estimated",value=assess.spa$best.estimate.decline.1750.2000,stringsAsFactors=F),
   dataValue=data.frame(Unit="km2",Year=c(1750,2000),Value=c("",""),stringsAsFactors=F),
   valueName="estimated")

     if(is.na(assess.total$bounds.A3)) {
        PB <-  newXMLNode("Plausible-bounds","",attrs=list(lower="",upper=""))
     } else {
        PB <-  newXMLNode("Plausible-bounds",assess.total$bounds.A3,attrs=list(lower=strsplit(assess.total$bounds.A3," -- ")[[1]][1], upper=strsplit(assess.total$bounds.A3," -- ")[[1]][2]))
     }

   A3 <- newXMLNode("Subcriterion",attrs=list(name="A3"),
         children=list(newXMLNode("Summaries",children=list(A3.summary)),
         newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),A3.rationale)),
          ##newXMLNode("Rationale",A3.rationale),
           newXMLNode("Category",assess.total$A3),
           PB,#newXMLNode("Bounds",ifelse(is.na(assess.total$bounds.A3),"",assess.total$bounds.A3)),
           A3.variable))



##A2a
A2a.rationale <- "No suitable indicator was evaluated for this period of time."
A2a.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("The data used to estimate decline in distribution was not applied to this timeframe. Alternative prediction or forecast models need to be considered for assessing this subcriterion."))
A2a <- newXMLNode("Subcriterion",attrs=list(name="A2a"),
  children=list(newXMLNode("Summaries",children=list(A2a.summary)),
    newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),A2a.rationale)),
         #    newXMLNode("Rationale",A2a.rationale),
    newXMLNode("Category","NE")))



##A2b
A2b.rationale <- with(assess.spa,
rD(as.character(A2b), best.estimate.decline.2000.2050, bounds.estimate.decline.2000.2050))

A2b.summary <- newXMLNode("Summary",attrs=list(lang="en"),sprintf("We considered presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroup. For contemporary forest cover we averaged percent tree cover from two time series of remote sensing data for each year between 2001 and 2012, and we calculated the proportional rate of decline (PRD) and used the difference in the predicted forest cover in 2001 and 2051 as an estimate of expected decline in any 50 year period including past, present and future.  %s", A2b.rationale))


A2b.variable <-
 makeKeyIndicatorVariable(
   dataName="Change in distribution",
   dataDesc = "For contemporary forest cover we considered two time series of data: the Global Forest Cover based on Landsat mosaics (GFC, version 1.2, Hansen et al. 2013)⁠  and the land cover data derived from MODIS  (MODIS LC, version 5.1, Friedl et al. 2010)⁠⁠. GFC data represents estimates of percentage tree cover in 2000 and estimates of forest loss and gain between 2000 and 2014, from which we calculated total forest cover per year. MODIS LC data represents yearly estimates of land cover classes between 2001 and 2012, including several forest types, semi-natural and artificial land cover. We converted land cover classes to percentage tree cover (100% for natural forest classes, 25% for mosaic vegetation, 0% otherwise). We then averaged percent tree cover from both sources for each year between 2001 and 2012 in order to partially correct over- and under-estimation of forest change due to increases in plantations and other artificial land uses (Song et al. 2014; Tropek et al. 2014)⁠. The estimated percentage tree cover of each year was summarized over the potential distribution of each Macrogroup to provide annual estimates of forest cover. The proportional rate of decline (PRD) was estimated by a generalized linear model fitted to the estimates per year using a quasipoisson error distribution and a log-link. For Macrogroups with a negative trend, the model  was used to predict the expected remaining distribution in 50 years. The use of this model avoids negative estimates of the remaining distribution. ",
   dataSources = c("Hansen et al. 2013","Friedl et al. 2010"),
   dataCT=data.frame(units="km2",value=0,stringsAsFactors=F),
   overallExtent =  data.frame(units="%",method="projected",value=assess.spa$best.estimate.decline.2000.2050,stringsAsFactors=F),
   dataValue=data.frame(Unit="km2",Year=c(2000,2050),Value=c("",""),stringsAsFactors=F),
   valueName="projected")


     if(is.na(assess.total$bounds.A2b)) {
        PB <-  newXMLNode("Plausible-bounds","",attrs=list(lower="",upper=""))
     } else {
        PB <-  newXMLNode("Plausible-bounds",assess.total$bounds.A2b,attrs=list(lower=strsplit(assess.total$bounds.A2b," -- ")[[1]][1], upper=strsplit(assess.total$bounds.A2b," -- ")[[1]][2]))
     }

A2b <- newXMLNode("Subcriterion",attrs=list(name="A2b"),
children=list(newXMLNode("Summaries",children=list(A2b.summary)),
     newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),A2b.rationale)),
         newXMLNode("Category",assess.total$A2b),
         PB,#  newXMLNode("Bounds",ifelse(is.na(assess.total$bounds.A2b),"",assess.total$bounds.A2b)),
 A2b.variable))




## overall A


wch <- which.max(cat.weights[unlist(assess.total[,c("A1","A2b","A3")])])
assess.total$A <- assess.total[,c("A1","A2b","A3")][[wch]]
A.category <- newXMLNode("Category",assess.total$A)
A.subcrit <- c("A1","A2b","A3")[assess.total[,c("A1","A2b","A3")] %in% assess.total$A]


A.rationale <- switch(assess.total$A,
          NE="The subcriteria could not be asssessed, thus under this criterion it is considered Not Evaluated.",
          DD="Assessment outcome was not conclusive, thus under this criterion it is considered Data Deficient.",
          LC="None of the assessed subcriteria met the threshold for Vulnerable, and under this criterion this Macrogroup is considered Least Concern",
          NT=sprintf("Subcriter%s %s %s close to the threshold for Vulnerable, thus under this criterion this Macrogroup is considered Near Threatened",ifelse(length(A.subcrit==1),"ion","ia"),paste(A.subcrit,collapse="and"),ifelse(length(A.subcrit==1),"was","were")),

          VU=sprintf("Subcriter%s %s %s above the threshold for Vulnerable but below the threshold for Endangered, thus under this criterion this Macrogroup is considered Vulnerable", ifelse(length(A.subcrit==1),"ion","ia"),paste(A.subcrit,collapse="and"),ifelse(length(A.subcrit==1),"was","were")),
          EN=sprintf("Subcriter%s %s %s above the threshold for Endangered but below the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Endangered", ifelse(length(A.subcrit==1),"ion","ia"),paste(A.subcrit,collapse="and"),ifelse(length(A.subcrit==1),"was","were")),
          CR=sprintf("Subcriter%s %s %s above the threshold for Critically Endangered, thus under this criterion this Macrogroup is considered Critically Endangered", ifelse(length(A.subcrit==1),"ion","ia"),paste(A.subcrit,collapse="and"),ifelse(length(A.subcrit==1),"was","were")))

A.summary <- newXMLNode("Summaries",children=list(newXMLNode("Summary",attrs=list(lang="en"),"For this macrogroup, forest vegetation is the dominant and diagnostic growth form. Thus, we consider presence of trees (as closed forest or woodlands) as a characteristic element and the main indicator of spatial distribution of the macrogroups. In order to cover both historical and present changes in distribution, we selected two different data sources that estimate tree presence: “woodlands” cover from historic biome and land use reconstruction (subcriteria A1 and A3), and percentage tree cover calculated from a combination of contemporary remote sensing data (subcriterion A2b)⁠.")))
#newXMLNode("Rationale",attrs=list(name="E"),"This criterion was not evaluated."),
#newXMLNode("Category","NE")
newXMLNode("Criterion",attrs=list(name="A"),
  children=list(
     A.summary,
     newXMLNode("Rationales",children=newXMLNode("Rationale",attrs=list(lang="en"),A.rationale)),
  ##newXMLNode("Rationale",attrs=list(name="A"),A.rationale),
  newXMLNode("Category",A.category),
  newXMLNode("Subcriterions",children=list(A1,A2a,A2b,A3))),
parent=AU.criteria)
