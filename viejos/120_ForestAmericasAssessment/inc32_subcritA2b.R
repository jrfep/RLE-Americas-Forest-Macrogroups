if (sp$A2b %in% "DD") { ## data deficient
    newXMLNode("subcriterion",attrs=list(name="2b"),
               children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                   newXMLNode("summary"," Available data on current forest cover for this Macrogroup in this assessment unit was not sufficient for fitting an informative model and predict future rates of declines. It is considered Data Deficient under subcriterion A2b."),
                   newXMLNode("period-begins","2001"),
                   newXMLNode("period-ends","2051")),
               parent=critA)
} else {
    if (sp$p.glm<0.05) {
        if (sign(sp$PRD>0)) {
            ## significant positive trend
            newXMLNode("subcriterion",attrs=list(name="2b"),
                       children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                           newXMLNode("summary",
                                      sprintf("Data on recent changes in forest cover area within the Macrogroup's potential distribution suggest a significant positive trend in forest cover. Assuming that this trend will continue in the future, this Macrogroup is considered Least Concern under subcriterion A2b.",(1-sp$p2051)*100,(1-sp$x2051)*100,(1-sp$n2051)*100,IUCN.cats[sp$A2b],sp$A2b.min,sp$A2b.max)),
                           newXMLNode("period-begins","2001"),
                           newXMLNode("period-ends","2051")),
                       parent=critA)
            
        } else {
            ## if significant decline with plausible bounds
            if(sp$A2b.min != sp$A2b.max) {
                newXMLNode("subcriterion",attrs=list(name="2b"),
                           children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                               newXMLNode("summary",
                                          sprintf("Future decline was estimated from recent changes in forest cover area within the Macrogroup's potential distribution. We used yearly composite estimates of forest cover for the period 2001-2012 and assume a linear change model. The fitted model suggests a significant negative trend. The coefficient of the model is interpreted as a proportional rate of decline. Based on the predicted %0.1f (%0.1f -- %0.1f) %% decline over a 50 year period, this Macrogroup is considered %s (%s -- %s) under subcriterion A2b",(1-sp$p2051)*100,(1-sp$x2051)*100,(1-sp$n2051)*100,IUCN.cats[sp$A2b],sp$A2b.min,sp$A2b.max)),
                               newXMLNode("plausible-bounds",sprintf("%s -- %s",sp$A2b.min,sp$A2b.max)),
                               newXMLNode("period-begins","2001"),
                               newXMLNode("period-ends","2051"),
                               newXMLNode("parameter-value",
                                          children=c(newXMLNode("proportional-rate-of-decline",sp$PRD,attrs=c(units="proportion loss per year",type="estimated")),
                                              newXMLNode("standard-error",sp$PRD.se),
                                              newXMLNode("p-value",sp$p.glm),
                                              newXMLNode("period","2001-2012"),
                                              newXMLNode("method","Proportional rate of decline (PRD) between 2001 and 2012 was estimated from a linear quasipoisson regression with logarithmic link-function fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]). The reported value of PRD corresponds to the fitted regression coefficient and its standard error. Significance was tested using a t-test."))),
                               newXMLNode("parameter-value",
                                          children=c(newXMLNode("proportional-decline",round((1-sp$p2051)*100,1),attrs=c(units="percentage",type="estimated")),
                                              newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sp$x2051)*100,(1-sp$n2051)*100),attrs=c(confidence="90%")),
                                              newXMLNode("period","2001-2051"),
                                              newXMLNode("method","Proportional decline between 2001 and 2051 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, linear term) fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]).  The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2051 (f1) and 2001 (f0), and their respective standard errors multiplied by 1.645.")))),
                           parent=critA) 
            } else {
                ## if significant decline with unambiguous assignment
                    newXMLNode("subcriterion",attrs=list(name="2b"),
                               children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                                   newXMLNode("summary",
                                              sprintf("Future decline was estimated from recent changes in forest cover area within the Macrogroup's potential distribution. We used yearly composite estimates of forest cover for the period 2001-2012 and assume a linear change model. The fitted model suggests a significant negative trend. The coefficient of the model is interpreted as a proportional rate of decline. Based on the predicted %0.1f (%0.1f -- %0.1f) %% decline over a 50 year period, this Macrogroup is considered %s under subcriterion A2b",(1-sp$p2051)*100,(1-sp$x2051)*100,(1-sp$n2051)*100,IUCN.cats[sp$A2b])),

                                   newXMLNode("period-begins","2001"),
                                   newXMLNode("period-ends","2051"),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-rate-of-decline",sp$PRD,attrs=c(units="proportion loss per year",type="estimated")),
                                                  newXMLNode("standard-error",sp$PRD.se),
                                                  newXMLNode("p-value",sp$p.glm),
                                                  newXMLNode("period","2001-2001"),
                                                  newXMLNode("method","Proportional rate of decline (PRD) between 2001 and 2012 was estimated from a linear quasipoisson regression with logarithmic link-function fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]). The reported value of PRD corresponds to the fitted regression coefficient and its standard error. Significance was tested using a t-test."))),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-decline",round((1-sp$p2051)*100,1),attrs=c(units="percentage",type="estimated")),
                                                  newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sp$x2051)*100,(1-sp$n2051)*100),attrs=c(confidence="90%")),
                                                  newXMLNode("period","2001-2051"),
                                                  newXMLNode("method","Proportional decline between 2001 and 2051 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, linear term) fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]).  The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2051 (f1) and 2001 (f0), and their respective standard errors multiplied by 1.645.")))),
                               parent=critA)
                }
        }
    } else {
            if (sign(sp$PRD>0)) {
                ## non-significant positive trend
                newXMLNode("subcriterion",attrs=list(name="2b"),
                       children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                           newXMLNode("summary",
                                      sprintf("Data on recent changes in forest cover area within the Macrogroup's potential distribution suggest a non-significant positive trend in forest cover. It is likely that forest cover remains constant or increases in the future, thus this Macrogroup is considered Least Concern under subcriterion A2b.",(1-sp$p2051)*100,(1-sp$x2051)*100,(1-sp$n2051)*100,IUCN.cats[sp$A2b],sp$A2b.min,sp$A2b.max)),
                           newXMLNode("period-begins","2001"),
                           newXMLNode("period-ends","2051")),
                       parent=critA)
            
        } else {
            if (sp$A2b %in% "LC") {

                ##if non-significant small decline 

                newXMLNode("subcriterion",attrs=list(name="2b"),
                           children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                               newXMLNode("summary",
                                          sprintf("Future decline was estimated from recent changes in forest cover area within the Macrogroup's potential distribution. We used yearly composite estimates of forest cover for the period 2001-2001 and assume a linear change model. The available data for this Macrogroup in this assessment shows a slight, non-significant negative trend. It is considered Least Concern under subcriterion A2b.")),

                                   newXMLNode("period-begins","2001"),
                                   newXMLNode("period-ends","2051"),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-rate-of-decline",sp$PRD,attrs=c(units="proportion loss per year",type="estimated")),
                                                  newXMLNode("standard-error",sp$PRD.se),
                                                  newXMLNode("p-value",sp$p.glm),
                                                  newXMLNode("period","2001-2012"),
                                                  newXMLNode("method","Proportional rate of decline (PRD) between 2001 and 2012 was estimated from a linear quasipoisson regression with logarithmic link-function fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]). The reported value of PRD corresponds to the fitted regression coefficient and its standard error. Significance was tested using a t-test."))),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-decline",round((1-sp$p2051)*100,1),attrs=c(units="percentage",type="estimated")),
                                                  newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sp$x2051)*100,(1-sp$n2051)*100),attrs=c(confidence="90%")),
                                                  newXMLNode("period","2001-2051"),
                                                  newXMLNode("method","Proportional decline between 2001 and 2051 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, linear term) fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]).  The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2051 (f1) and 2001 (f0), and their respective standard errors multiplied by 1.645.")))),
                               parent=critA)

            } else {
                ##if non-significant large decline
                
                newXMLNode("subcriterion",attrs=list(name="2b"),
                           children=c(newXMLNode("category",sp$A2b,attrs=list(criterion="A",subcriterion="2b",reported="yes")),
                               newXMLNode("summary",
                                          sprintf("Future decline was estimated from recent changes in forest cover area within the Macrogroup's potential distribution. We used yearly composite estimates of forest cover for the period 2001-2012 and assume a linear change model. The available data for this Macrogroup in this assessment unit suggests a large negative trend, but the fitted model was not significant, probably due to the strong fluctuation between years or the coarse resolution of the available data. It is assigned to the Near Threatened category, and requires further analysis of subcriterion A2b."),
                               newXMLNode("period-begins","2001"),
                               newXMLNode("period-ends","2051"),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-rate-of-decline",sp$PRD,attrs=c(units="proportion loss per year",type="estimated")),
                                                  newXMLNode("standard-error",sp$PRD.se),
                                                  newXMLNode("p-value",sp$p.glm),
                                                  newXMLNode("period","2001-2012"),
                                                  newXMLNode("method","Proportional rate of decline (PRD) between 2001 and 2012 was estimated from a linear quasipoisson regression with logarithmic link-function fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]). The reported value of PRD corresponds to the fitted regression coefficient and its standard error. Significance was tested using a t-test."))),
                                   newXMLNode("parameter-value",
                                              children=c(newXMLNode("proportional-decline",round((1-sp$p2051)*100,1),attrs=c(units="percentage",type="estimated")),
                                                  newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sp$x2051)*100,(1-sp$n2051)*100),attrs=c(confidence="90%")),
                                                  newXMLNode("period","2001-2051"),
                                                  newXMLNode("method","Proportional decline between 2001 and 2051 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, linear term) fitted to yearly estimates of forest cover area. These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) with a composite map of current forest cover (considering Modis based Land Cover classification of natural forest and mosaic vegetation [REF] and LandSat based estimates of percentage tree cover [REF]).  The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2051 (f1) and 2001 (f0), and their respective standard errors multiplied by 1.645."))))),
                           parent=critA)
            }
        }
        }
}
