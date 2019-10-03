CS.id <- sprintf("%s_%s",mi.reflabel, CS.counter)
rsm <- subset(Macrogroups.Global,IVC.macrogroup_key %in% case.study)
ATids.reg %>% filter(`Case-Study id` == CS.id) -> AT.id.Provita

CS.name <- with(rsm,sprintf("%s: '%s, %s'",
  "Forest Macrogroups of the Americas",
  IVC.Name,
  ifelse(is.na(Overall.Category),"NE",
    ifelse(is.na(Overall.Bounds),Overall.Category,
      sprintf("%s (%s)",Overall.Category,Overall.Bounds)))))
CS.altnames.en <- rsm$IVC.Name
CS.name.lang <- "en"

rsm.info <- data.frame()
for (j in sprintf("rsm.MG%s",1:5)) {
  if (case.study %in% get(j)$Codigo)
    rsm.info <- subset(get(j),Codigo %in% case.study)
}

division.name <- subset(classification.cross.walk,mcdg %in% case.study & classification %in% "IVC version 2014" & level %in% 4)$name


## First, set up all Assessment-Target nodes
AT.id <- newXMLNode("AT-id",AT.id.Provita$`AT-id`)
AT.descs <- newXMLNode("AT-descriptions")
AT.names <- newXMLNode("AT-names")
AT.biota <- newXMLNode("Characteristic-biota")
AT.abiotic <- newXMLNode("Abiotic-environment")
AT.biotic <- newXMLNode("Biotic-processes")
AT.services <- newXMLNode("Ecosystem-services")
AT.threats <- newXMLNode("Threats")
AT.actions <- newXMLNode("Conservation-actions")
AT.research <- newXMLNode("Research-needs")
AT.CEM <- newXMLNode("Conceptual-ecosystem-model")
AT.class <- newXMLNode("Classifications")
AT.dist <- newXMLNode("Distribution")
AT.collapse <- newXMLNode("Collapse-definition")

## Now, add information per node

## descriptions:
newXMLNode("AT-description",sprintf("The '%s' is a forest macrogroup belonging to the '%s' division in the International Vegetation Classification system (Faber-Langendoen et al. 2014). It is considered here as a proxy for a forest ecosystem type in a regional assessment of forest ecosystems in the Americas.",rsm$IVC.Name, division.name), attrs=list(lang=CS.name.lang), parent=AT.descs)

## alternative names
newXMLNode("AT-name",rsm$IVC.Name, attrs=list(lang=CS.name.lang), parent=AT.names)

## characteristic Biota
if (nrow(rsm.info)>0) {
  spp.list <-unique(trim(unlist(sapply(as.matrix(rsm.info[,grep("Especies.",colnames(rsm.info))]),function(x) strsplit(x,",")))))
  spp.list <- spp.list[!spp.list %in% ""]
  if (length(spp.list)>0) {
    newXMLNode("Biota-Summaries",
      children=list(newXMLNode("Biota-Summary",
      sprintf("A list of %s characteristic species was extracted from descriptive profiles of the Macrogroup%s.", length(spp.list),ifelse(sum(!rsm.info$Codigo.Sistema.Ecologico %in% "")>0," and related ecological systems","")),
      attrs=list(lang="en"))),parent=AT.biota)
    taxon.list <- newXMLNode("taxons",parent=AT.biota)
    for (taxon in spp.list)
      newXMLNode("taxon",taxon, attrs=list(lang="scientific"), parent=taxon.list)

  }
}

## Abiotic environment
if (nrow(rsm.info)>0) {
  abiotic.text.es <- paste(unique(c(rsm.info$Variables.meteorologicas,
  rsm.info$Tipo.Suelo)), collapse=". ")
  if (nchar(abiotic.text.es)>10) {
    newXMLNode("Abiotic-Summaries",
          children=list(newXMLNode("Abiotic-Summary",
          sprintf("Information on soil and general climatic conditions was extracted from descriptive profiles of the Macrogroup%s.", ifelse(sum(!rsm.info$Codigo.Sistema.Ecologico %in% "")>0," and related ecological systems","")),
          attrs=list(lang="en")),
          newXMLNode("Abiotic-Summary",
          abiotic.text.es,
          attrs=list(lang="es"))),parent=AT.abiotic)
  }
}

## Biotic processes
if (nrow(rsm.info)>0) {
  biotic.text.es <- paste(unique(c(rsm.info$Info_Relevante_Ecologica)), collapse=". ")
  if (nchar(biotic.text.es)>10) {
    newXMLNode("Processes-Summaries",
          children=list(newXMLNode("Processes-Summary",
          sprintf("Information on biotic characteristics and processes was extracted from descriptive profiles of the Macrogroup%s.", ifelse(sum(!rsm.info$Codigo.Sistema.Ecologico %in% "")>0," and related ecological systems","")),
          attrs=list(lang="en")),
          newXMLNode("Processes-Summary",
          biotic.text.es,
          attrs=list(lang="es"))),parent=AT.biotic)
  }
}


## Threats
slc <- subset(threat.match,Macrogroup %in% case.study)$code
threat.list <- subset(threat.desc,code %in% slc)
if (nrow(threat.list)>0) {
  for (tt in 1:nrow(threat.list)) {
    this.Threat.class <- newXMLNode("Threat-classification", attrs=list(id="IUCN", version="3.2", selected="yes", `assigned-by`="JRFP"))
    for (level in 1:3) {
      tl <- threat.list[tt,sprintf("level%s",level)]
      if (!is.na(tl)) {
        for (k in trim(strsplit(tl,",")[[1]])) {
            newXMLNode("Threat-classification-element", k, attrs=list(level=level), parent=this.Threat.class)
        }
      }
    }
    newXMLNode("Threat",
     children=list(
       newXMLNode("Threat-name",threat.list[tt,"name"]),
       newXMLNode("Threat-description",threat.list[tt,"description"],attrs=list(lang="en")),
       newXMLNode("Threat-Impact",
         children=list(
           newXMLNode("Threat-Timing",threat.list[tt,"timing"],attrs=list(id="IUCN", version="3.2", selected="yes", `assigned-by`="JRFP")),
           newXMLNode("Threat-Scope",threat.list[tt,"scope"],attrs=list(id="IUCN", version="3.2", selected="yes", `assigned-by`="JRFP")),
           newXMLNode("Threat-Severity",threat.list[tt,"severity"],attrs=list(id="IUCN", version="3.2", selected="yes", `assigned-by`="JRFP"))
       )),
       this.Threat.class
     ),
     parent=AT.threats)
  }
}

## CEM
newXMLNode("CEM-summaries",
      children=list(newXMLNode("CEM-summary","A general conceptual model is presented graphically in Fig. S1. We include in this model the most important large scale threats to forest in the Americas: loss of forest cover due to agriculture and live stock expansion; defaunation due to hunting (or 'empty forest syndrome'), structural and compositional changes due to selective logging and invasion of exotic species, changes in disturbance regimes due to land use practices and development and shifting climatic conditions due to climate change.",
      attrs=list(lang="en"))),
      parent=AT.CEM)

newXMLNode("CEM-type","Cause-effect",
      attrs=list(graphic="yes"),
      parent=AT.CEM)

newXMLNode("CEM-source","Assessment authors, based on Laurance and Williamson (2001), Allen et al. (2010), Faber-Langendoen et al. (2014), Lewis et al. (2015), Levis et al. (2017)",
      parent=AT.CEM)


## Classification
selected.string <- "yes"
ccw <- subset(classification.cross.walk,mcdg %in% case.study)

for (class.string in unique(ccw$classification)) {
  class.sys <- strsplit(class.string," version ")[[1]]
  class.typ <- newXMLNode("Classification-system", attrs=list(id=class.sys[1], version=class.sys[2], selected=selected.string, `assigned-by`="Assessment authors"),
        parent=AT.class)
  cross.walk <- subset(ccw, classification %in% class.string)
  for (k in 1:nrow(cross.walk)) {
    newXMLNode("Classification-element", cross.walk[k,"name"], attrs=list(level=cross.walk[k,"level"]), parent=class.typ)
  }
  selected.string <- "no"
}

##collapse
newXMLNode("Spatial-collapse","As the tree growth form is the main structural element of any forest macrogroup, a forest macrogroup was assumed to collapse if the original woodland cover was completely replaced by a non-woodland cover, or if the tree-cover within its potential distribution declined to zero (criteria A and B).",
      attrs=list(lang="en"),parent=AT.collapse)
newXMLNode("Functional-collapse","For assessing environmental degradation under criterion C, we considered two different indicators. In the case of climate change, we assume that the ecosystem collapsed if the climatic conditions shifted from mostly suitable for the focus macrogroup, to mostly suitable to a different macrogroup. Additionally for flooded and swamp forest, we assumed ecosystem collapse if the amount of detected surface water within the potential distribution of the macrogroup declined to zero.  For assessing disruption to biotic processes and interactions under criterion D we assumed that most of the fundamental processes and interactions disappeared with intensive use of woodlands under high population density. We also assumed that the ecosystem would collapse if the population of large mammals declined to less than 10% of their original population size.",
            attrs=list(lang="en"),parent=AT.collapse)


## distribution
if (nrow(rsm.info)>0) {
  newXMLNode("Distribution-Summaries",
    children=list(newXMLNode("Distribution-Summary", paste(unique(rsm.info$Distribución.Geografica),collapse=" "), attrs=list(lang="es"))),
    parent=AT.dist)
  }
    Countries.node <- newXMLNode("Countries",
        parent=AT.dist)

  country.list <- unique(subset(Macrogroups.Country,IVC.macrogroup_key %in% case.study)$Country)
  country.list <- country.list[!country.list %in% ""]
  iso.list <- ISO_3166_1[match(country.list, ISO_3166_1$Name),"Alpha_2"]
  names(country.list) <- iso.list

  for (pais in iso.list) {
      newXMLNode("Country",country.list[pais], attrs=list(`iso-code-2`=pais), parent=Countries.node)
  }
  if(all(iso.list %in% c("US","CA"))) {
    newXMLNode("Biogeographic-realm","Nearctic",parent=AT.dist)
  } else {
    if (any(iso.list %in% c("US","CA"))) {
      newXMLNode("Biogeographic-realm","Nearctic",parent=AT.dist)
    }
    newXMLNode("Biogeographic-realm","Neotropic",parent=AT.dist)
  }

  ## Ecosystem services
  AT.services
  ## Conservation actions
  AT.actions
  ## Research needs
  AT.research
