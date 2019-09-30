################
## Fig 2: Recent and current rates of decline
#################
xx <- merge(subset(b.hist,pais %in% "global"),subset(dts,pais %in% "global"),by="mcdg")
xx$frmt <- resumen.global$frmt[match(xx$mcdg,resumen.global$mcdg)]
xx <- xx[match(resumen.global$mcdg,xx$mcdg),]

xx$aa <- seq(along=xx$p1950) + (as.numeric(as.factor(xx$frmt))*3)

xx[xx$mcdg %in% subset(resumen.global,oacats %in% "NE")$mcdg,"A1"] <- "NE"
xx[xx$mcdg %in% subset(resumen.global,oacats %in% "NE")$mcdg,"A2a"] <- "NE"
xx[xx$mcdg %in% subset(resumen.global,oacats %in% "NE")$mcdg,"A3"] <- "NE"

layout(matrix(1:3,ncol=1))
par(mar=c(2,4,0,2),oma=c(1,3,1,1))


plot(p1750~aa,xx,col=NA,ylim=c(0,1),pch=19,cex=1.2,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,ylab="Since 1750")
axis(2)
box()
with(subset(xx,!A3 %in% "NE"),segments(aa,n1750,aa,x1750,col="grey77"))
with(subset(xx,!A3 %in% "NE"),points(p1750~aa,col=IUCNclr[as.character(A3)],pch=19,cex=1.2))
abline(v=c(22,46,65,101,106,117,142))
axis(1,c(11,34,55,85,103.5,111.5,130,150),c(sprintf("1.A.%s",1:5),sprintf("1.B.%s",1:3)),tick=FALSE,cex.axis=0.85)

par(mar=c(1,4,1,2))

plot(p1950~aa,xx,col=NA,ylim=c(0,1),pch=19,cex=1.2,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,ylab="1950-2000")
axis(4)
box()
with(subset(xx,!(A1 %in% "NE")),segments(aa,n1950,aa,x1950,col="grey77"))
with(subset(xx,!(A1 %in% "NE")),points(p1950~aa,col=IUCNclr[as.character(A1)],pch=19,cex=1.2))
abline(v=c(22,46,65,101,106,117,142))


with(xx,axis(1,sprintf(ifelse(A2a %in% "NE","(%s)"," %s "),mcdg),at=aa,las=2,cex.axis=.4))

par(mar=c(0,4,2,2))

plot(p2051~aa,xx,col=NA,ylim=c(0,1.05),pch=19,cex=1.2,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,ylab="2000-2050")
with(subset(xx,p2051<1 & !(A2a %in% "NE")),segments(aa,n2051,aa,x2051,col="grey77"))
with(subset(xx,p2051>1 & !(A2a %in% "NE")),arrows(aa[p2051>1],0.97,aa[p2051>1],1.03,length=0.07,angle=45,col=IUCNclr[as.character(A2a)],lwd=1.2))
points(p2051~aa,subset(xx,p2051<1),col=IUCNclr[as.character(subset(xx,p2051<1)$A2a)],pch=19,cex=1.2)

##with(subset(xx,p2051>1 & !(A2a %in% "NE")),points(aa,p2051^0 * .99,col=IUCNclr[as.character(A2a)],pch=19,cex=1.4))
abline(v=c(22,46,65,101,106,117,142))
axis(2)
box()

  mtext("Proportional decline",side=2,line=1.2,outer=T)
 
dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_Figure2.pdf",hoy),width=8,height=6)
dev.off()

###################
##Fig 3: Mean severity
kk <- subset(w.pres,pais %in% "global" & !is.na(mean.severity))
kk$aa <- xx$aa[match(kk$mcdg,xx$mcdg)]
ll <- with(subset(c.futr,pais %in% "global"),
           aggregate(mean.severity,list(mcdg=mcdg),quantile,c(0,.5,1)))

ll$aa <- xx$aa[match(ll$mcdg,xx$mcdg)]
ll$C2a <- resumen.global$C2a[match(ll$mcdg,resumen.global$mcdg)]
mm <- subset(d.pres,pais %in% "global")
mm$aa <- xx$aa[match(mm$mcdg,xx$mcdg)]
nn <- subset(d.hist,pais %in% "global" & !is.na(mean.severity.D1))
nn$aa <- xx$aa[match(nn$mcdg,xx$mcdg)]

layout(matrix(1:5,ncol=1))

plot(ll$aa,ll$x[,2],xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,ylab="(a) Climate change",type="p",lwd=3,col=IUCNclr[as.character(ll$C2a)],ylim=c(0,100))
with(subset(ll,!C2a %in% "NE"),segments(aa,x[,1],aa,x[,3],col="grey77"))
with(subset(ll,!C2a %in% "NE"),points(aa,x[,2],col=IUCNclr[as.character(C2a)],pch=19,cex=1.2))

axis(2)
box()
abline(v=c(22,46,65,101,106,117,142))

plot(kk$aa,kk$mean.severity,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,ylab="(b) Surface Water change",type="h",lwd=3,col=IUCNclr[as.character(kk$C2b)],ylim=c(0,100))
axis(2)
box()
abline(v=c(22,46,65,101,106,117,142))

axis(1,c(11,34,55,85,103.5,111.5,130,150),c(sprintf("1.A.%s",1:5),sprintf("1.B.%s",1:3)),tick=FALSE,cex.axis=0.85,line=-0.5)

plot(nn$aa,nn$mean.severity.D3,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,type="h",lwd=3,col=IUCNclr[as.character(nn$D3)],ylim=c(0,100),ylab="(c) Use intensity (since 1700)")
axis(2)
box()
abline(v=c(22,46,65,101,106,117,142))



  mtext("Mean severity (%)",side=2,line=1.2,outer=T)

plot(nn$aa,nn$mean.severity.D1,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,type="h",lwd=3,col=IUCNclr[as.character(nn$D1)],ylim=c(0,100),ylab="(d) Use intensity (1900-2000)")
axis(2)
box()
abline(v=c(22,46,65,101,106,117,142))
with(xx,axis(1,sprintf(ifelse(A2a %in% "NE","(%s)"," %s "),mcdg),at=aa,las=2,cex.axis=.4,line=0))


plot(mm$aa,mm$mean.severity,xlim=range(xx$aa)+c(-1,1),xaxs="i",axes=F,type="h",lwd=3,col=IUCNclr[as.character(mm$D2b)],ylim=c(0,100),ylab="(e) Risk of defaunation")
axis(2)
box()
abline(v=c(22,46,65,101,106,117,142))


dev.copy(pdf,file=sprintf("~/Provita/etc/MS/%s_Figure3.pdf",hoy),width=8,height=10)
dev.off()
