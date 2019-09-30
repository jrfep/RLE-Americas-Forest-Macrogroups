cli.rsm <- data.frame()
for (mm in cdgs) {

    for (pp in unique(subset(c.futr,mcdg %in% mm)$pais) ) {
        mc <- subset(c.futr,mcdg %in% mm & pais %in% pp)
        if (nrow(mc)<1) {

            
          cli.rsm <- rbind(cli.rsm,data.frame(mcdg=mm,pais=pp,C2a = "DD",
                                              bounds = NA,
                                              freq=  NA,
                                              costo.LC=0,
                                              costo.VU=0,
                                              costo.EN=0))
        } else {
            tab.cli <- table(factor(mc$C2a,levels=names(IUCN.cats)))
            tab.cli <- tab.cli[tab.cli>0]
            mic2a <- names(which.max(tab.cli))
            cli.rsm <- rbind(cli.rsm,data.frame(mcdg=mm,pais=pp,
                                                C2a = factor(mic2a,levels=names(IUCN.cats)),
                                                bounds = paste(unique(names(tab.cli)[c(length(tab.cli),1)]),collapse="--"),
                                                freq=  tab.cli[mic2a]/sum(tab.cli),
                                                costo.LC=median(mc$costo.LC),
                                                costo.VU=median(mc$costo.VU),
                                                costo.EN=median(mc$costo.EN)))
        }
    }
}
cli.rsm[cli.rsm$bounds %in% c("NT--CR", "LC--CR") ,"C2a"] <- "DD"
cli.rsm[cli.rsm$bounds %in% c("NT--CR", "LC--CR") ,"bounds"] <- "DD"
