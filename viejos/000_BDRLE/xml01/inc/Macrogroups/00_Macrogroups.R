##Para los macrogrupos del análisis de bosques

for (ff in c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5",
             "1.B.1","1.B.2","1.B.3")) {
    doc = newXMLDoc()
    top = newXMLNode("ecosystems",doc=doc)
    for (k in grep(ff,confTest$Name)) {
        mid <- subset(RLEDB[[2]],EcoName %in% confTest[k,"Name"])$ecosystem_ID
        mg <- confTest[k,"Macrogroup.code"]
        tt <- subset(tipologia,macrogroup_key %in% confTest[k,"Macrogroup.code"])

        paises <- c()
        for (dd in c("tabSAM","tabNAC")) {
            if (file.exists(sprintf("~/tmp/%s/Paises/%s.txt",dd,mg))) {
                pss <- read.table(sprintf("~/tmp/%s/Paises/%s.txt",dd,mg))
                paises <- c(paises,names(rev(sort(table(pss$V2)))))
            }
        }
        paises <- unique(paises[!paises %in% "*"])
        slc.paises <- subset(lista.paises,OBJECTID %in% paises)

        ecoregs <- c()
        for (dd in c("tabSAM","tabNAC")) {
            if (file.exists(sprintf("~/tmp/%s/WWFecoregions/%s.txt",dd,mg))) {
                pss <- read.table(sprintf("~/tmp/%s/WWFecoregions/%s.txt",dd,mg))
                ecoregs <- c(ecoregs,names(rev(sort(table(pss$V2)))))
            }
        }
        ecoregs <- unique(ecoregs[!ecoregs %in% "*"])
        slc.ecoregs <- subset(lista.ecoreg,OBJAT.id.Provita$`AT-id`ECTID %in% ecoregs)
        ss <- NULL

        if (confTest[k,"Macrogroup.code"] %in% rsm.MG1$Codigo) {
            ss <- subset(rsm.MG1,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG2$Codigo) {
            ss <- subset(rsm.MG2,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG3$Codigo) {
            ss <- subset(rsm.MG3,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG4$Codigo) {
            ss <- subset(rsm.MG4,Codigo %in% mg)
        }
        if (confTest[k,"Macrogroup.code"] %in% rsm.MG5$Codigo) {
            ss <- subset(rsm.MG5,Codigo %in% mg)
        }
        ss[ss %in% "N.T."] <- NA

    ## ecosystem description
    a = newXMLNode("ecosystem",attrs=list(id=mid),
        parent=top,
        children =
            c(newXMLNode("name", confTest[k,"Name"],attrs=list(lang="en")),
              newXMLNode("system",subset(RLEDB[[2]],EcoName %in% confTest[k,"Name"])$Realm ),
              newXMLNode("description-source", "published international classification"),

              newXMLNode("comments", "International Vegetation Classification (IVC or EcoVeg) by Faberlangedoen et al (20XX).")
              ))

        cc1 <- c()
        cc2 <- c()
        if (ff %in% c("1.A.4","1.A.5","1.B.3")) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "hydric regime"))
        }
        if (any(!is.na(ss[,"Tipos.de.Bosques"])) | any(!is.na(ss[,"Info_Relevante_Ecologica"]))) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "vegetation type"))
            qry <- c()
            for (kk in c("Tipos.de.Bosques","Info_Relevante_Ecologica")) {
                if (any(!is.na(ss[,kk]))) {
                    qry <- c(qry,unique(ss[,kk]))
                }
            }
            qry <- unique(qry[!is.na(qry)])
            cc2 <- c(cc2,newXMLNode("Vegetation-type", paste(qry,collapse=". "),attrs=list(lang="es")))
        }
colnames(ss)[colnames(ss) %in% c("Distribución.Geográfica","Distribución.Geografica")] <- "distribucion"

        if (any(!is.na(ss[,"distribucion"]))) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "topography"))
            cc2 <- c(cc2,newXMLNode("Topography", paste(unique(ss[,"distribucion"]),collapse=". "),attrs=list(lang="es")))
    }
        if (any(!is.na(ss[,"Variables.meteorologicas"]))) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "climate"))
            cc2 <- c(cc2,newXMLNode("climate", paste(unique(ss[,"Variables.meteorologicas"]),collapse=". "),attrs=list(lang="es")))
    }
        if (any(!is.na(ss[,"Tipo.Suelo"]))) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "soil"))
            cc2 <- c(cc2,newXMLNode("Soil", paste(unique(ss[,"Tipo.Suelo"]),collapse=". "),attrs=list(lang="es")))
    }

        if (any(!is.na(ss[,"Especies.en.la.descripcion.del..Macrogrupo"])) | any(!is.na(ss[,"Especies.en.la.descripcion.de.los.sistemas.ecologicos.que.los.conforman"]))) {
            cc1 <- c(cc1,newXMLNode("description-based-on", "characteristic biota"))

            qry <- c()
            for (kk in c("Especies.en.la.descripcion.del..Macrogrupo","Especies.en.la.descripcion.de.los.sistemas.ecologicos.que.los.conforman")) {
                if (any(!is.na(ss[,kk]))) {
                    qry <- c(qry,unique(ss[,kk]))
                }
            }
            qry <- unique(qry[!is.na(qry)])
            cc2 <- c(cc2,newXMLNode("Characteristic-biota", paste(qry,collapse=". "),attrs=list(lang="es")))

        }


    a <- addChildren(a, cc1)
    if (!is.null(cc2))
        a <- addChildren(a, cc2)

    b = newXMLNode("ecosystem-classification",attrs=list(id="IVC",version="2015",selected="yes"),
        children=c(
            newXMLNode("classification-system","International Vegetation Classification"),
            newXMLNode("level0",as.character(subset(tipologia,Division.Code %in% tt$grp)$class),attrs=list(name="Class",code=tt$grp)),
            newXMLNode("level1",as.character(subset(tipologia,Division.Code %in% tt$sgrp)$subclass),attrs=list(name="Subclass",code=tt$sgrp)),
            newXMLNode("level2",as.character(subset(tipologia,Division.Code %in% tt$frmt)$formation),attrs=list(name="Formation",code=tt$frmt)),
            newXMLNode("level3",as.character(subset(tipologia,Division.Code %in% tt$Division.Code & division !="")$division),attrs=list(name="Division",code=tt$Division.Code)),
            newXMLNode("level4",confTest[k,"Name"],attrs=list(name="Macrogroup",code=mg)),
            newXMLNode("scale","international"),
            newXMLNode("reference","Faberlangedoen et al. 2014"),
            newXMLNode("assigned-by","NatureServe")
        ))
    a <- addChildren(a, b)


    d = newXMLNode("ecosystem-classification",attrs=list(id="IUCN",version="3.1",selected="no"),
        children=c(
            newXMLNode("classification-system","IUCN Habitats Classification Scheme"),
            newXMLNode("level1",IUCN.subclass[tt$frmt],
                       attrs=list(name="level 1")),
            newXMLNode("level0","1. Forest",attrs=list(name="level 0")),
            newXMLNode("scale","global"),
            newXMLNode("reference","IUCN Habitats Classification Scheme"),
            newXMLNode("version","3.1"),
            newXMLNode("url","http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3"),
            newXMLNode("assigned-by","JRFP")
        ))

        a <- addChildren(a, d)

        ##Distribution
        regions <- unique(slc.ecoregs$G200_REGIO)
        regions <- regions[!is.na(regions)]

    d = newXMLNode("distribution",attrs=list(summary="Vegetation Macrogroup with continental distribution"),
        children=c(
            newXMLNode("description",ss$Distribución.Geografica,attrs=list(lang="es")),
            newXMLNode("ecoregions",sprintf("Distributed among the Global 200 regions of %s.",paste(iconv(regions,"latin1","utf8"),collapse=", "))),
            newXMLNode("continent","America",attrs=list(id="Am"))
        ))

        if (any(slc.ecoregs$REALM %in% "NT"))
            addChildren(d,newXMLNode("biogeographic-realm","Neotropic",attrs=list(id="006/NT")))
        if (any(slc.ecoregs$REALM %in% "NA"))
            addChildren(d,newXMLNode("biogeographic-realm","Nearctic",attrs=list(id="005/NA")))

        countries <- c()
        for (p in 1:nrow(slc.paises)) {
            countries <- c(countries,newXMLNode("country", iconv(slc.paises$LOCAL[p],"latin1","utf8"),attrs=list(id=slc.paises$ISO2[p])))
        }
        addChildren(d,countries)
        a <- addChildren(a, d)


        ##Delimitation
        d = newXMLNode("delimitation",attrs=list(summary="Delimitation is based on a multiple step procedure of supervised classification based on several spatial layers. Validation was based on several tests by the original authors at NatureServe and independent assessors."),
        children=c(
            newXMLNode("methods",""),
            newXMLNode("input-data",""),
            newXMLNode("data-source",""),
            newXMLNode("validation",
                       ifelse(confTest[k,"Included.in.analysis"],"Macrogroup delimitation is considered consistent according to various validation tests",sprintf("Macrogroup delimitation might require refinement or improvement. %s",confTest[k,"Notes"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Local validation"),
                           newXMLNode("result", confTest[k,"Local"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Buffer (5km) validation"),
                           newXMLNode("result", confTest[k,"Within.5km2"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Confidence level according to consulted experts."),
                           newXMLNode("result", confTest[k,"Expert.confidence"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Percentage of agreement of Macrogroup distribution with corresponding environmental strata."),
                           newXMLNode("result", confTest[k,"Bioclimate"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Percentage of agreement of Macrogroup distribution with corresponding ecoregions."),
                           newXMLNode("result", confTest[k,"Ecoregion"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Percentage of Macrogroup distribution that overlaps with distribution records (from GBIF) of its characteristic biota. In general, low values are expected due to bias in geographic coverage of GBIF records."),
                           newXMLNode("result", confTest[k,"Characteristic.Biota"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Measure of overlap with similar forest macrogroups (possible confusion of Macrogroup identity)."),
                           newXMLNode("result", confTest[k,"Low.overlap"]))),
            newXMLNode("validation-test",
                       children=c(
                           newXMLNode("description","Overlap with historic forest cover"),
                           newXMLNode("result", confTest[k,"Historic.Forest.Cover"])))
        ))
    a <- addChildren(a, d)

    }

    cat( saveXML(doc,file=sprintf("%s/Macrogroups_Americas_%s.xml",output.dir,ff),prefix = '<?xml version="1.0" encoding="UTF-8"?>', encoding = "UTF-8"))
}
