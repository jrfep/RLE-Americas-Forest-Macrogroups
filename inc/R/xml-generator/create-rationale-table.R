Macrogroups.Global$Overall.rationale <- NA
Macrogroups.Global$Overall.rationale[Macrogroups.Global$Overall.Category %in% "NE"]  <- "At the time of assessment the spatial data for this Macrogroup was inconsistent, and the assessors decided no to include it in their evaluation."
SpatialCriteria.Global[1,]

cat.names <- c("Not Evaluated","Data Deficient","Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered","Collapsed")
 names(cat.names) <- c("NE","DD","LC","NT","VU","EN","CR","CO")


minB <- sapply(as.character(SpatialCriteria.Global$A1.bounds),function(x) strsplit(x," -- ")[[1]][1])
maxB <- sapply(as.character(SpatialCriteria.Global$A1.bounds),function(x) strsplit(x," -- ")[[1]][2])

J1  <- with(SpatialCriteria.Global,sprintf("It was estimated that the geographic distribution declined %s %% in the past 50 years",best.estimate.decline.1950.2000))
J2 <- with(SpatialCriteria.Global,sprintf("; thus, the ecosystem is classified as %s under sub-criterion A1. ",cat.names[as.character(A1)]))
J2[SpatialCriteria.Global$A1 %in% "DD"] <- ""

J2[SpatialCriteria.Global$A1 %in% "NT"] <- ". This is very close to the minimum threshold to assign a risk category; thus, the ecosystem is classified as Near Threatened under sub-criterion A1. "
J2[SpatialCriteria.Global$A1 %in% "LC"] <- ". This is below the minimum threshold to assign a risk category; thus, the ecosystem is classified as Least Concern under sub-criterion A1. "

J3 <- with(SpatialCriteria.Global,ifelse(minB==maxB,
   sprintf("Uncertainty in this estimate (90%% C.I. %s %%) remains within the thresholds for this category.",bounds.estimate.decline.1950.2000),
   sprintf("Uncertainty in this estimate (90%% C.I. %s %%) suggests a plausible range from %s to %s.",bounds.estimate.decline.1950.2000,cat.names[minB],cat.names[maxB])))
J3[SpatialCriteria.Global$A1 %in% "DD"] <- sprintf("%s It is thus considered Data Deficient according to section 3.3.3 in the IUCN RLE Guidelines.",gsub("Uncertainty in this estimate ", ", but this estimate has considerable uncertainty ", J3[SpatialCriteria.Global$A1 %in% "DD"]))

paste(J1,J2,J3,sep="")


write.csv(file=sprintf("%s/results/csvs/OverallCat.csv",script.dir),
Macrogroups.Global[,c("IVC.division","IVC.formation","IVC.macrogroup_key","IVC.Name","Overall.Category","Overall.Bounds","Threat.criteria")],row.names=F)
