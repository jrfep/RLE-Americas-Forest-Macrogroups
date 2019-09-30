##ls respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/*pdf

##pdftotext respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/Mexico_Central_America_EcosystemClassifications_ConditionAssessment_NatureServe_2014.pdf 
##pdftotext respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/Caribbean_EcosystemClassifications_EcolAssessment_NatureServe_2014.pdf 
##pdftotext respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/Vegetation\ macrogroups\ of\ South\ America_concept\ summaries.pdf 
##pdftotext respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/Boreal\ Forest\ MGs_classification_ConditionAssessment_NatureServe2014.pdf
##grep [cC]onceptual respaldoFTPNatureServe/IUCN_Red_List_Ecosystems/Deliverables/Documents/Classification_Condition_Assessment/*txt

dts <- read.csv("~/Provita/data/IVC/ResumenInformacionPorMacrogrupo_ModelosConceptuales.csv")

unique(substr(subset(dts,substr(dts$Division,1,2) %in% "1." & Conceptual.model %in% "X")$Macrogroup,1,4))
dim(subset(dts,substr(dts$Division,1,2) %in% "1." & Conceptual.model %in% "X"))
