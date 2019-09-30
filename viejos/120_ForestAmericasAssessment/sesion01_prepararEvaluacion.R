## un ejemplo del bosque seco de la Guajira
frmt <- "1.A.1"
mcdg <- "M563"
output.dir <- sprintf("%s/MGs/%s/%s",gsub("doc","output",mi.path),frmt,mcdg)
cdg <- as.numeric(gsub("M","",mcdg))
gc()
## source("~/mi.git/redlistr/R/RelativeSeverity_functions.R")


c.futr <- w.pres <- data.frame()
d.hist <- d.pres <- data.frame()
b.pres <- b.hist <- AOOs <- data.frame()
(load(sprintf("%s/%s_tablas_Mgs.rda",
             gsub("doc","Rdata",mi.path),"20180317")))

for (frmt in c("1.A.1","1.A.2","1.A.3","1.A.4","1.A.5","1.B.1","1.B.2","1.B.3")) {
    cat(sprintf("%s ::",frmt))
    for (mcdg in cdgs[cdgs %in% subset(tipologia,format %in% frmt & macrogroup_key != "")$macrogroup_key]) {
        cat(sprintf(": %s :",mcdg))

        output.dir <- sprintf("%s/MGs/%s/%s",gsub("doc","output",mi.path),frmt,mcdg)

        system(sprintf("mkdir -p %s",output.dir))
        cdg <- as.numeric(gsub("M","",mcdg))
        gc()
        
        source(sprintf("%s/%s/inc10_readTabs.R",mi.path,mi.dir))
         cat(sprintf("."))

        ## se redefinieron los costos para incorporar diferentes escalas (LC, VU, EN) en cada criterio... *~*
        if (!mcdg %in% b.pres$mcdg) {
            source(sprintf("%s/%s/inc20_spatialindicators_forest.R",mi.path,mi.dir))
            cat(sprintf("."))
        }
       if (!mcdg %in% w.pres$mcdg) {
         source(sprintf("%s/%s/inc21_environmentalindicators_forest.R",mi.path,mi.dir))
         cat(sprintf("."))
       }
         if (!mcdg %in% d.pres$mcdg) {
             source(sprintf("%s/%s/inc22_bioticindicators_forest.R",mi.path,mi.dir))
         cat(sprintf("."))
         }
        save(file=sprintf("%s/%s_tablas_Mgs.rda",
                          gsub("doc","Rdata",mi.path),hoy),c.futr,w.pres,d.hist,
             d.pres,b.pres,b.hist,AOOs )
    }
    cat(sprintf(":\n",frmt))
}



