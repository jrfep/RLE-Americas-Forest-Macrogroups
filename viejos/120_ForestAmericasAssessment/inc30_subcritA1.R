
if (sh$A1 %in% "DD") { ## data deficient
    newXMLNode("subcriterion",attrs=list(name="1"),
               children=c(newXMLNode("category",sh$A1,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                   newXMLNode("summary"," Available historical data on woodland distribution for this Macrogroup in this assessment unit was not sufficient for fitting an informative model and estimate rates of declines. It is considered Data Deficient under subcriterion A1."),
                   newXMLNode("period-begins","1950"),
                   newXMLNode("period-ends","2000")),
           parent=critA)
} else {
    if (sh$p1.glm<0.05 |sh$p2.glm<0.05 ) {
        ## if significant decline with plausible bounds
        if(sh$A1.min != sh$A1.max) {
            newXMLNode("subcriterion",attrs=list(name="1"),
                       children=c(newXMLNode("category",sh$A1,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                           newXMLNode("summary",
                                      sprintf("Recent historical decline was estimated from the curve of decline in woodland area within the Macrogroup's potential distribution in the last four centuries. In most of the continent, this decline appears to be non-linear due to increased rates of population growth in the XX century (REF). Thus a quadratic model was fitted to the historical data (four data points, one for each century). The fitted model suggests a significant negative trend. The proportional decline was estimated from model predictions instead of using direct interpolation between data-points, this allows to smooth-out measurement-errors and provides a confidence interval for estimates that is translated to plausible bounds in the final category assignment. Based on %0.1f (%0.1f -- %0.1f) %% decline over a 50 year period, this Macrogroup is considered %s (%s -- %s) under subcriterion A1",(1-sh$p1950)*100,(1-sh$x1950)*100,(1-sh$n1950)*100,IUCN.cats[sh$A1],sh$A1.min,sh$A1.max)),
                           newXMLNode("plausible-bounds",sprintf("%s -- %s",sh$A1.min,sh$A1.max)),
                           newXMLNode("period-begins","1950"),
                           newXMLNode("period-ends","2000"),
                           newXMLNode("parameter-value",
                                      children=c(newXMLNode("proportional-decline",round((1-sh$p1950)*100,1),attrs=c(units="percentage",type="estimated")),
                                          newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sh$x1950)*100,(1-sh$n1950)*100),attrs=c(confidence="90%")),
                                          newXMLNode("period","1950-2000"),
                                          newXMLNode("method","Proportional decline between 1950 and 2000 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, quadratic term) fitted to four point-estimates of historical forest cover (1700, 1800, 1900 and 2000). These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) and historical reconstruction of anthropogenic biomes (REF). The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2000 (f1) and 1950 (f0), and their respective standard errors multiplied by 1.645.")))),
           parent=critA) 
    } else {
        ## if significant decline with unambiguous assignment
            newXMLNode("subcriterion",attrs=list(name="1"),
                       children=c(newXMLNode("category",sh$A1,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                           newXMLNode("summary",
                                      sprintf("Recent historical decline was estimated from the curve of decline in woodland area within the Macrogroup's potential distribution in the last four centuries. In most of the continent, this decline appears to be non-linear due to increased rates of population growth in the XX century (REF). Thus a quadratic model was fitted to the historical data (four data points, one for each century). The fitted model suggests a significant negative trend. The proportional decline was estimated from model predictions instead of using direct interpolation between data-points, this allows to smooth-out measurement-errors and provides a confidence interval for estimates. Based on %0.1f (%0.1f -- %0.1f) %% decline over a 50 year period, this Macrogroup is considered %s under subcriterion A1",(1-sh$p1950)*100,(1-sh$x1950)*100,(1-sh$n1950)*100,IUCN.cats[sh$A1])),
                           newXMLNode("period-begins","1950"),
                           newXMLNode("period-ends","2000"),
                           newXMLNode("parameter-value",
                                      children=c(newXMLNode("proportional-decline",round((1-sh$p1950)*100,1),attrs=c(units="percentage",type="estimated")),
                                          newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sh$x1950)*100,(1-sh$n1950)*100),attrs=c(confidence="90%")),
                                          newXMLNode("period","1950-2000"),
                                          newXMLNode("method","Proportional decline between 1950 and 2000 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, quadratic term) fitted to four point-estimates of historical forest cover (1700, 1800, 1900 and 2000). These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) and historical reconstruction of anthropogenic biomes (REF). The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2000 (f1) and 1950 (f0), and their respective standard errors multiplied by 1.645.")))),
           parent=critA) 
        }
    } else {
        if (sh$A1 %in% "LC") {
            ##if non-significant small decline
            
            newXMLNode("subcriterion",attrs=list(name="1"),
                       children=c(newXMLNode("category",sh$A1,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                           newXMLNode("summary","Recent historical decline was estimated from the curve of decline in woodland area within the Macrogroup's potential distribution in the last four centuries. In most of the continent, this decline appears to be non-linear due to increased rates of population growth in the XX century (REF). Thus a quadratic model was fitted to the historical data (four data points, one for each century). The available historical data for this Macrogroup in this assessment unit does not show a significant negative trend. It is considered Least Concern under subcriterion A1."),
                           newXMLNode("period-begins","1950"),
                           newXMLNode("period-ends","2000"),
                           newXMLNode("parameter-value",
                                      children=c(newXMLNode("proportional-decline",round((1-sh$p1950)*100,1),attrs=c(units="percentage",type="estimated")),
                                          newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sh$x1950)*100,(1-sh$n1950)*100),attrs=c(confidence="90%")),
                                          newXMLNode("period","1950-2000"),
                                          newXMLNode("method","Proportional decline between 1950 and 2000 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, quadratic term) fitted to four point-estimates of historical forest cover (1700, 1800, 1900 and 2000). These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) and historical reconstruction of anthropogenic biomes (REF). The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2000 (f1) and 1950 (f0), and their respective standard errors multiplied by 1.645.")))),
                       parent=critA)

        } else {
            ##if non-significant large decline
            
            newXMLNode("subcriterion",attrs=list(name="1"),
                       children=c(newXMLNode("category",sh$A1,attrs=list(criterion="A",subcriterion="1",reported="yes")),
                           newXMLNode("summary","Recent historical decline was estimated from the curve of decline in woodland area within the Macrogroup's potential distribution in the last four centuries. In most of the continent, this decline appears to be non-linear due to increased rates of population growth in the XX century (REF). Thus a quadratic model was fitted to the historical data (four data points, one for each century). The available historical data for this Macrogroup in this assessment unit suggests a large negative trend, but the fitted model was not significant, probably due to the small sample size or the coarse resolution of the available data. It is assigned to the Near Threatened category, and requires further analysis of subcriterion A1."),
                           newXMLNode("period-begins","1950"),
                           newXMLNode("period-ends","2000"),
                           newXMLNode("parameter-value",
                                      children=c(newXMLNode("proportional-decline",round((1-sh$p1950)*100,1),attrs=c(units="percentage",type="estimated")),
                                          newXMLNode("confidence-interval",sprintf("%0.1f -- %0.1f",(1-sh$x1950)*100,(1-sh$n1950)*100),attrs=c(confidence="90%")),
                                          newXMLNode("period","1950-2000"),
                                          newXMLNode("method","Proportional decline between 1950 and 2000 was estimated from a Proportional Rate of Decline model (quasipoisson regression, logarithmic link-function, quadratic term) fitted to four point-estimates of historical forest cover (1700, 1800, 1900 and 2000). These estimates are based on a simple raster crossing of potential Macrogroup distribution (NatureServe REF) and historical reconstruction of anthropogenic biomes (REF). The proportional decline and 90% confidence intervals are calculated from the back-transformed raw model predictions for 2000 (f1) and 1950 (f0), and their respective standard errors multiplied by 1.645.")))),
                       parent=critA)
        }
    }
}
