## define category names
cat.names <- c("Not Evaluated","Data Deficient","Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered","Collapsed")
 names(cat.names) <- c("NE","DD","LC","NT","VU","EN","CR","CO")

## Criterion A
## subcriteria
for (subCrit in c("A1","A2b","A3")) {
   bound.col <- sprintf("%s.bounds",subCrit)
   bestdec.col <- min(grep(subCrit,colnames(SpatialCriteria.Global),fixed=T)-2)
   bounddec.col <- min(grep(subCrit,colnames(SpatialCriteria.Global),fixed=T)-1)
   opening.phrase <- switch(subCrit,A1="It was estimated that the geographic distribution declined %s %% in the past 50 years",
   A2b="It was estimated that, at the present rate of decline, the geographic distribution could decline %s %% in a 50-year period",
   A3="It was estimated that the geographic distribution declined %s %% historically")
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


Macrogroups.Global$Overall.rationale <- NA
Macrogroups.Global$Overall.rationale[Macrogroups.Global$Overall.Category %in% "NE"]  <- "At the time of assessment the spatial data for this Macrogroup was inconsistent, and the assessors decided no to include it in their evaluation."
SpatialCriteria.Global[1,]

write.csv(file=sprintf("%s/results/csvs/OverallCat.csv",script.dir),
Macrogroups.Global[,c("IVC.division","IVC.formation","IVC.macrogroup_key","IVC.Name","Overall.Category","Overall.Bounds","Threat.criteria")],row.names=F)
