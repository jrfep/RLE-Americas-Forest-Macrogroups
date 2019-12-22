## define category names
cat.names <- c("Not Evaluated","Data Deficient","Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered","Collapsed")
 names(cat.names) <- c("NE","DD","LC","NT","VU","EN","CR","CO")

## Criterion A
## subcriteria
for (subCrit in c("A1","A2b","A3")) {
   bound.col <- sprintf("%s.bounds",subCrit)
   bestdec.col <- min(grep(subCrit,colnames(SpatialCriteria.Global),fixed=T)-2)
   bounddec.col <- min(grep(subCrit,colnames(SpatialCriteria.Global),fixed=T)-1)
   opening.phrase <- switch(subCrit,A1="It was estimated that the geographic distribution measured as woodland cover declined %s %% in the past 50 years",
   A2b="It was estimated that, at the present rate of decline in tree cover, the geographic distribution could decline %s %% in a 50-year period",
   A3="It was estimated that the geographic distribution measured as woodland cover declined %s %% historically")
   minB <- sapply(as.character(SpatialCriteria.Global[,bound.col]),function(x) strsplit(x," -- ")[[1]][1])
   maxB <- sapply(as.character(SpatialCriteria.Global[,bound.col]),function(x) strsplit(x," -- ")[[1]][2])

   J1  <- sprintf(opening.phrase,SpatialCriteria.Global[,bestdec.col])

   J1[SpatialCriteria.Global[,bestdec.col] %in% 0] <- switch(subCrit,A1="It was estimated that the geographic distribution remained constant or increased in the past 50 years",
   A2b="It was estimated that the current trend is stable or increasing and no decline is expected in a 50-year period",
   A3="It was estimated that the current distribution is similar to its historical distribution")


   J2 <- sprintf("; thus, the ecosystem is classified as %s under sub-criterion %s. ", cat.names[as.character(SpatialCriteria.Global[,subCrit])], subCrit)
   J2[SpatialCriteria.Global[,subCrit] %in% "DD"] <- ""

   J2[SpatialCriteria.Global[,subCrit] %in% "NT"] <- sprintf(". This is very close to the minimum threshold to assign a risk category; thus, the ecosystem is classified as Near Threatened under sub-criterion %s. ",subCrit)
   J2[SpatialCriteria.Global[,subCrit] %in% "LC"] <- sprintf(". This is below the minimum threshold to assign a risk category; thus, the ecosystem is classified as Least Concern under sub-criterion %s. ",subCrit)
   J2[(SpatialCriteria.Global[,bestdec.col] %in% 0) & (SpatialCriteria.Global[,subCrit] %in% "LC")] <- sprintf("; thus, the ecosystem is classified as Least Concern under sub-criterion %s. ",subCrit)

   J3 <- ifelse(minB==maxB,
      sprintf("Uncertainty in this estimate (90%% C.I. %s %%) remains within the thresholds for this category.",SpatialCriteria.Global[,bounddec.col]),
      sprintf("Uncertainty in this estimate (90%% C.I. %s %%) suggests a plausible range from %s to %s.",SpatialCriteria.Global[,bounddec.col],cat.names[minB],cat.names[maxB]))
      J3[SpatialCriteria.Global[,subCrit] %in% "DD"] <- sprintf("%s It is thus considered Data Deficient according to section 3.3.3 in the IUCN RLE Guidelines.",gsub("Uncertainty in this estimate ", ", but this estimate has considerable uncertainty ",
      J3[SpatialCriteria.Global[,subCrit] %in% "DD"]))
       J3[(SpatialCriteria.Global[,bestdec.col] %in% 0)] <- ""

      Macrogroups.Global[match(as.character(SpatialCriteria.Global$IVC.macrogroup_key), Macrogroups.Global$IVC.macrogroup_key),sprintf("%s.rationale",subCrit)] <- paste(J1,J2,J3,sep="")

}

Macrogroups.Global[,"A2a.rationale"] <- "No suitable indicator of decline was evaluated for this period of time."

Macrogroups.Global[Macrogroups.Global$A1 %in% "NE","A1.rationale"] <- "No suitable indicator of decline in distribution was evaluated for this period of time."
Macrogroups.Global[Macrogroups.Global$A2b %in% "NE","A2b.rationale"] <- "No suitable indicator of decline in distribution was evaluated for this period of time."
Macrogroups.Global[Macrogroups.Global$A3 %in% "NE","A3.rationale"] <- "No suitable indicator of decline in distribution was evaluated for this period of time."


Macrogroups.Global[,"B1.rationale"] <- sapply(Macrogroups.Global[,"B1"],function(x) switch(x,
LC="The estimated EOO is above the maximum threshold to assign a risk category; thus, the ecosystem is classified as Least Concern under sub-criterion B1.",NE="No suitable indicator of distribution was evaluated for this subcriterion."))

Macrogroups.Global[,"B2.rationale"] <-  sapply(Macrogroups.Global[,"B2"],function(x) switch(x,LC="The estimated AOO is above the maximum threshold to assign a risk category; thus, the ecosystem is classified as Least Concern under sub-criterion B2.",EN="The AOO is ≤ 20 and there is an observed or inferred spatial or functional continuing decline; thus, the ecosystem is classified as Endangered under sub-criterion B2a.",NE="No suitable indicator of distribution was evaluated for this subcriterion."))

Macrogroups.Global[,"B3.rationale"] <- sapply(Macrogroups.Global[,"B3"],function(x) switch(x,
LC="The number of threat-defined locations is not very small (> 5); thus, the ecosystem is classified as Least Concern for sub-criterion B3.",NE="No suitable indicator of distribution was evaluated for this subcriterion."))


## Criterion C

rD <- function(CAT,Mean.Sev,PB.Sev=NA,Sev.30,Sev.50,Sev.80,Indicator="selected indicator",th=c(30,50,80)) {
  if (is.na(CAT)) {
    "Available data was not accurate enough to fit the environmental suitability model, thus we assigned it to the Data Deficient category."
  } else {
  switch(CAT,
      NE=sprintf("No suitable indicator of environmental degradation was evaluated for this period of time."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("Mean relative severity was %s%s%%, and it did not reach any of the thresholds for Vulnerable.",Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev))),
      NT=sprintf("The estimates of relative severity and extent of %s were close to the thresholds for Vulnerable, so it was considered to be Near Threatened",Indicator,Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev))),
      VU=sprintf("Mean relative severity of %s was %s%s%%. %s, exceeding the threshold for Vulnerable.", Indicator, Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[1], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), ifelse(Sev.50>th[2], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]),sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.30,th[1])))),
      EN=sprintf("Mean relative severity of %s was %s%s%%. %s, exceeding the threshold for Endangered.", Indicator,Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[2], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]))),
      CR=sprintf("Mean relative severity of %s was %s%s%% and %s %% of the extent had a relative severity of %s %% or higher, exceeding the threshold for Critically Endangered.", Indicator, Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)),Sev.80,th[3]))
    }
}
for ( j in 1:nrow(FunctionalCriteria.Global)) {
  Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"C2a.rationale"] <-   with(FunctionalCriteria.Global[j,],rD(as.character(C2a),
    best.estimate.mean.severity.ClimateChange.2000.2050,
    bounds.estimate.mean.severity.ClimateChange.2000.2050,
    extent.with.severity.30.or.higher.Climate.Change.2000.2050,
    extent.with.severity.50.or.higher.Climate.Change.2000.2050,
    extent.with.severity.80.or.higher.Climate.Change.2000.2050,Indicator="projected change in environmental suitability"))

  Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"C2b.rationale"] <-   with(FunctionalCriteria.Global[j,],rD(as.character(C2b),
  best.estimate.mean.severity.SurfaceWater.19XX.20XX,
     NA,
    extent.with.severity.30.or.higher.Surface.Water.19XX.20XX,
      extent.with.severity.50.or.higher.Surface.Water.19XX.20XX,
        extent.with.severity.80.or.higher.Surface.Water.19XX.20XX,Indicator="estimated decline in surface water"))
}


Macrogroups.Global[,"C1.rationale"] <- "No suitable indicator of environmental degradation was evaluated for this period of time."
Macrogroups.Global[,"C3.rationale"] <- "No suitable indicator of environmental degradation was evaluated for this period of time."

##Macrogroups.Global[Macrogroups.Global$A1 %in% "NE","C2a.rationale"] <- "No suitable indicator of environmental degradation was evaluated for this period of time."

## Criterion D
rD <- function(subCrit,CAT,Mean.Sev,PB.Sev=NA,Sev.30,Sev.50,Sev.80,Indicator="selected indicator",th=c(30,50,80)) {
  if (is.na(CAT)) {
    "Available data was not accurate enough to assess biotic disruption in this time frame, thus we assigned it to the Data Deficient category."
  } else {
  switch(CAT,
      NE=sprintf("No suitable indicator of biotic disruption was evaluated for this period of time."),
      DD=sprintf("Available data was not accurate enough, resulting in unreliable estimates, thus we assigned it to the Data Deficient category."),
      LC=sprintf("Mean relative severity of %s was %s%s%%, and it did not reach any of the thresholds for Vulnerable.",Indicator,Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev))),
      NT=sprintf("Estimates of relative severity and extent of %s were close to the thresholds for Vulnerable, so it was considered to be Near Threatened under subcriterion %s.",Indicator,Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)),subCrit),
      VU=sprintf("Mean relative severity of %s was %s%s%%. %s, exceeding the threshold for Vulnerable for subcriterion %s.", Indicator, Mean.Sev, ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[1], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), ifelse(Sev.50>th[2], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2]),sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.30,th[1]))),subCrit),
      EN=sprintf("Mean relative severity of %s was %s%s%%. %s, exceeding the threshold for Endangered under subcriterion %s.", Indicator, Mean.Sev, ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)), ifelse(Sev.80>th[2], sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.80,th[3]), sprintf("Up to %s %% of the extent had a relative severity of %s %% or higher", Sev.50,th[2])),subCrit),
      CR=sprintf("Mean relative severity of %s was %s%s%% and %s %% of the extent had a relative severity of %s %% or higher, exceeding the threshold for Critically Endangered under subcriterion %s.", Indicator, Mean.Sev,ifelse(is.na(PB.Sev),"",sprintf(" (%s) ",PB.Sev)),Sev.80,th[3],subCrit))
    }
}
Macrogroups.Global[,"D2a.rationale"] <- "No suitable indicator of biotic disruption was evaluated for this period of time."


for ( j in 1:nrow(FunctionalCriteria.Global)) {
  Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"D1.rationale"] <-   with(FunctionalCriteria.Global[j,],rD("D1",as.character(D1),
  best.estimate.mean.severity.LandUseIntensity.1950.2000, NA,
    extent.with.severity.30.or.higher.LandUseIntensity.1950.2000,
      extent.with.severity.50.or.higher.LandUseIntensity.1950.2000,
        extent.with.severity.80.or.higher.LandUseIntensity.1950.2000,Indicator="intensity in land use"))

  Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"D2b.rationale"] <-   with(FunctionalCriteria.Global[j,],rD("D2b",as.character(D2b),
  best.estimate.mean.severity.Defaunation.19XX.20XX,NA,
    extent.with.severity.30.or.higher.Defaunation.19XX.20XX,
      extent.with.severity.50.or.higher.Defaunation.19XX.20XX,
        extent.with.severity.80.or.higher.Defaunation.19XX.20XX,Indicator="potential defaunation"))

  Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"D3.rationale"] <-   with(FunctionalCriteria.Global[j,],rD("D3",as.character(D3),
  best.estimate.mean.severity.LandUseIntensity.1750.2000,NA,
    extent.with.severity.50.or.higher.LandUseIntensity.1750.2000,
      extent.with.severity.70.or.higher.LandUseIntensity.1750.2000,
        extent.with.severity.90.or.higher.LandUseIntensity.1750.2000,Indicator="intensity in land use",th=c(50,70,90)))
}
## Criterion E
Macrogroups.Global[match(as.character(FunctionalCriteria.Global$IVC.macrogroup_key[j]), Macrogroups.Global$IVC.macrogroup_key),"E.rationale"] <-  "No suitable model was evaluated to estimate probability of collapse."

## Overall

Indicators <- c("past and historic decline in woodland cover",
"current extent and trends in tree cover",
"past and historic decline in woodland cover",
"current extent and trends in tree cover",
"current extent and trends in tree cover",
"current extent and trends in tree cover",
"predicted future changes in climatic suitability",
"estimated current changes in surface water",
"past and historic changes in the intensity of land use",
"potential defaunation of medium and large mammals",
"past and historic changes in the intensity of land use")


for ( j in 1:nrow(FunctionalCriteria.Global)) {
  k <- strsplit(Macrogroups.Global$Threat.criteria[j],", ")[[1]]
  if (!all(is.na(k))) {
   ks <- sprintf("%s.rationale",k)
   threat.rationale <- paste(   Macrogroups.Global[j,ks],collapse=" ")
  } else {
   threat.rationale <- "None of these indicators exceeded the thresholds for Vulnerable."
  }

  indlist <- unique(Indicators[!Macrogroups.Global[j,c("A1","A2b","A3","B1","B2","B3","C2a","C2b","D1","D2b","D3")] %in% "NE"])

  if (is.na(Macrogroups.Global$Overall.Bounds[j])) {
      thebounds <- ""
  } else {
    minB <- strsplit(Macrogroups.Global$Overall.Bounds[j]," -- ")[[1]][1]
    maxB <- strsplit(Macrogroups.Global$Overall.Bounds[j]," -- ")[[1]][2]
    thebounds <- sprintf(" with plausible bounds %s to %s", cat.names[minB],cat.names[maxB])
  }

  outcome <- sprintf("Thus the overall category is %s%s.", cat.names[ Macrogroups.Global$Overall.Category[j]], thebounds)

   Macrogroups.Global$Overall.rationale[j] <- sprintf("The IUCN Red List of Ecosystem assessment of the Macrogroup '%s' was based on %s indicators evaluated over different time frames: %s. %s %s", Macrogroups.Global$IVC.Name[j], length(indlist),paste(indlist,collapse=", "), threat.rationale, outcome)
}

Macrogroups.Global$Overall.rationale[Macrogroups.Global$Overall.Category %in% "NE"]  <- "At the time of assessment the spatial data for this Macrogroup was inconsistent, and the assessors decided no to include it in their evaluation."

write.csv(file=sprintf("%s/results/csvs/OverallCat.csv",script.dir),
Macrogroups.Global[,c("IVC.division","IVC.formation","IVC.macrogroup_key","IVC.Name","Overall.Category","Overall.Bounds","Threat.criteria","Overall.rationale"
##,"A1.rationale","A2a.rationale","A2b.rationale","A3.rationale",
##"B1.rationale","B2.rationale","B3.rationale",
##"C1.rationale","C2a.rationale","C2b.rationale","C3.rationale",
##"D1.rationale","D2a.rationale","D2b.rationale","D3.rationale",
##"E.rationale"
)],row.names=F,na="")
