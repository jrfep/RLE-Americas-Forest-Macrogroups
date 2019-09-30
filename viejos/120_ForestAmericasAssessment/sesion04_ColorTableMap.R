
## mapa categorias globales
## en AmericasRobinson
##NE es 255 255 y 255
rojo <-  c(NF=255,NE=200,DD=128,LC=0,  NT=173,VU=255,EN=255,CR=255,CO=0)
verde <- c(NF=255,NE=200,DD=128,LC=128,NT=255,VU=255,EN=165,CR=0,  CO=0)
azul <-  c(NF=255,NE=200,DD=128,LC=0,  NT=47, VU=0,  EN=0,  CR=0,  CO=0)
rgb(rojo,verde,azul,max=255)

tabla.colores <- sprintf("%sgrass/tablaBSQ_globalAssessment.txt",gsub("doc","lib",mi.path))


prb <- data.frame(mcdg=seq(1:873),
           Cat="NF",stringsAsFactors=F)
prb$Cat[match(as.numeric(gsub("M","",resumen.global$mcdg)),prb$mcdg)] <- resumen.global$oacats
cat(paste(sprintf("%s %s:%s:%s",prb$mcdg,
                  rojo[prb   $Cat],
                  verde[prb   $Cat],
                  azul[prb   $Cat]),collapse="\n"),file=tabla.colores)


##r.colors map=MGsam@RLE rules=/home/jferrer/Provita/lib/grass/tablaBSQ_globalAssessment.txt
##r.colors map=MGnac@RLE rules=/home/jferrer/Provita/lib/grass/tablaBSQ_globalAssessment.txt

##d.mon x0
##d.rast MGsam
##d.rast -o MGnac 
##d.vect Paises type=line,boundary
######d.barscale at=0,44.5
##d.out.file /home/jferrer/Provita/etc/MS/20180308_RLEamericas format=png size=1800,2000

