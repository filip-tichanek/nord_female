rm(list=ls())
library(stringr)
library(dplyr)


# setting parameters for smoothing
spar=0.4
knot=12

# colors ---------------------------------------------------------

cola<-c(
  rgb(1,0.1,0.1,alpha=1),
  rgb(0.1,0.1,1,alpha=1),
  rgb(0, 0.6, 0.3,alpha=1),
  rgb(0.7,0.7,0.1,alpha=1))

colb<-c(
  rgb(1,0.1,0.1,alpha=0.2),
  rgb(0.1,0.1,1,alpha=0.2),
  rgb(0, 0.6, 0.3,alpha=0.2),
  rgb(0.7,0.7,0.1,alpha=0.2))

colc<-c(
  rgb(1,0.1,0.1,alpha=0.8),
  rgb(0.1,0.1,1,alpha=0.8),
  rgb(0, 0.6, 0.3,alpha=0.8),
  rgb(0.7,0.7,0.1,alpha=0.8))


# Data upload---------------------------------------------------------

## breast data ----------------------------------------------------------
breast_inc_mor<-read.csv("source_data/breast_inc_mor.csv",sep=",")
colnam<-breast_inc_mor[,1]
breast_inc_mor<-data.frame(t(breast_inc_mor))[-1,]
colnames(breast_inc_mor) <- colnam
breast_inc_mor$year <- 1943:2020
### Subseting years 1961-2020
breast_inc_mor<-breast_inc_mor[breast_inc_mor$year>1960,]
### Removing space character and converting characters to numbers
x=1;repeat{
breast_inc_mor[,x]<-str_trim(breast_inc_mor[,x])
breast_inc_mor[,x]<-as.numeric(breast_inc_mor[,x])
x=x+1;if(x>9){break}}
summary(breast_inc_mor)


## female genitals data ----------------------------------------------------------
fem_gen_inc_mor<-read.csv("source_data/fem_gen_inc_mor.csv",sep=",")
colnam<-fem_gen_inc_mor[,1]
fem_gen_inc_mor<-data.frame(t(fem_gen_inc_mor))[-1,]
colnames(fem_gen_inc_mor) <- colnam
fem_gen_inc_mor$year <- 1943:2020
### Subseting years 1961-2020
fem_gen_inc_mor<-fem_gen_inc_mor[fem_gen_inc_mor$year>1960,]
dim(fem_gen_inc_mor)
### Removing space character and converting characters to numbers
x=1;repeat{
  fem_gen_inc_mor[,x]<-str_trim(fem_gen_inc_mor[,x])
  fem_gen_inc_mor[,x]<-as.numeric(fem_gen_inc_mor[,x])
  x=x+1;if(x>9){break}}
summary(fem_gen_inc_mor)

## ovary data ----------------------------------------------------------
ovary_inc_mor<-read.csv("source_data/ovary_inc_mor.csv",sep=",")
colnam<-ovary_inc_mor[,1]
ovary_inc_mor<-data.frame(t(ovary_inc_mor))[-1,]
colnames(ovary_inc_mor) <- colnam
ovary_inc_mor$year <- 1943:2020
### Subseting years 1961-2020
ovary_inc_mor<-ovary_inc_mor[ovary_inc_mor$year>1960,]
### Removing space character and converting characters to numbers
x=1;repeat{
  ovary_inc_mor[,x]<-str_trim(ovary_inc_mor[,x])
  ovary_inc_mor[,x]<-as.numeric(ovary_inc_mor[,x])
  x=x+1;if(x>9){break}}
summary(ovary_inc_mor)

## corpus uteri data ----------------------------------------------------------
corut_inc_mor<-read.csv("source_data/corut_inc_mor.csv",sep=",")
colnam<-corut_inc_mor[,1]
corut_inc_mor<-data.frame(t(corut_inc_mor))[-1,]
colnames(corut_inc_mor) <- colnam
corut_inc_mor$year <- 1943:2020
### Subseting years 1961-2020
corut_inc_mor<-corut_inc_mor[corut_inc_mor$year>1960,]
### Removing space character and converting characters to numbers
x=1;repeat{
  corut_inc_mor[,x]<-str_trim(corut_inc_mor[,x])
  corut_inc_mor[,x]<-as.numeric(corut_inc_mor[,x])
  x=x+1;if(x>9){break}}
summary(corut_inc_mor)



## cervix uteri data ----------------------------------------------------------
cervix_inc_mor<-read.csv("source_data/cervix_inc_mor.csv",sep=",")
colnam<-cervix_inc_mor[,1]
cervix_inc_mor<-data.frame(t(cervix_inc_mor))[-1,]
colnames(cervix_inc_mor) <- colnam
cervix_inc_mor$year <- 1943:2020
### Subseting years 1961-2020
cervix_inc_mor<-cervix_inc_mor[cervix_inc_mor$year>1960,]
### Removing space character and converting characters to numbers
x=1;repeat{
  cervix_inc_mor[,x]<-str_trim(cervix_inc_mor[,x])
  cervix_inc_mor[,x]<-as.numeric(cervix_inc_mor[,x])
  x=x+1;if(x>9){break}}
summary(cervix_inc_mor)






# Plotting - incidence -------------------------
## General setting and titles of plots

m <- matrix(c(18,1,2, 3, 4, 5
              ,6, 8, 9,10,11,12
              ,7,13,14,15,16,17), nrow = 3, ncol =6 ,byrow = TRUE)
layout(mat = m,heights = c(0.025,0.975/2,0.975/2),
       widths = c(0.04,rep(0.96/5,5)))
par(mgp=c(1.6,0.62,0))
par(mar=c(0,0,0,0))


plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Breast cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Endometrial cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Ovarian cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Cervical cancer",cex=1.6,font=3,xpd=TRUE)

plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),ylim=c(-0.85,0.85))
text(0,-0.2,"Vulvar cancer",cex=1.6,font=3,xpd=TRUE)

par(mar=c(2,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(0,1))
text(-0.5,0.5,"Incidence per 100,000 (ASR - World)",
     cex=1.5,srt=90)

par(mar=c(2,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(0,1))
text(-0.5,0.5,"Mortality per 100,000 (ASR - World)",
     cex=1.5,srt=90)


## Plot of breast cancer ----------------
summary(breast_inc_mor)
par(mgp=c(0.1,0.5,0))
par(mar=c(2,0.8,0,0))

range<-c(20,100);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=breast_inc_mor;xx<-1
repeat{
smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1)
xx=xx+1;if(xx>4){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     pos=xrange[1],tck= -0.025)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.8, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"a",cex=2.5)

xx=1;yy=range[1]+scal*0.25
rect(1985,yy+0.035*scal,2017.9,yy-0.21*scal,col="white",border="grey50",lwd=0.8)
repeat{
  lines(c(1987.5,1992.5),c(yy,yy),lwd=12,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.058);if(xx>4){break}}


xx=1;yy=range[1]+scal*0.25
text(2005,yy,"Denmark",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2005,yy,"Finland",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2005,yy,"Norway",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2005,yy,"Sweden",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)


## Plot of corpus uteri cancer ------------------------------------
summary(corut_inc_mor)

range<-c(6,18);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=corut_inc_mor;xx<-1
repeat{
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>4){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=4),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"b",cex=2.5)





## Plot of ovarian and tubes cancer ------------------------------------
summary(ovary_inc_mor)

range<-c(6,18);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=ovary_inc_mor;xx<-1
repeat{
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>4){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=4),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"c",cex=2.5)

  




## Plot of cervix uteri cancer ------------------------------------
summary(cervix_inc_mor)

range<-c(0,36);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=cervix_inc_mor;xx<-1
repeat{
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>4){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=6),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"d",cex=2.5)






## Plot of female genitals cancer ------------------------------------
summary(fem_gen_inc_mor)

range<-c(0,4);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=fem_gen_inc_mor;xx<-1
repeat{
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>4){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"e",cex=2.5)








# Plotting - mortality-------------------
## Plot of breast cancer ----------------
summary(breast_inc_mor)

range<-c(10,28);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


data=breast_inc_mor;xx<-5
repeat{
  smoothingSpline = smooth.spline(data$year, data[,xx], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>8){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=6),
     pos=xrange[1],tck= -0.025)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.8, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"f",cex=2.5)


## Plot of corpus uteri cancer ------------------------------------
summary(corut_inc_mor)
range<-c(0,4);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}


xx<-5
repeat{
  data=corut_inc_mor
  data=data[,c(xx,9)]
  data=na.omit(data)
  smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>8){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"g",cex=2.5)




## Plot of ovarian and tubes cancer ------------------------------------
summary(ovary_inc_mor)


range<-c(4,14);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}

xx<-5
repeat{
  data=ovary_inc_mor
  data=data[,c(xx,9)]
  data=na.omit(data)
  smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>8){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"h",cex=2.5)






## Plot of cervix uteri cancer ------------------------------------
summary(cervix_inc_mor)

range<-c(0,14);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}

xx<-5
repeat{
  data=cervix_inc_mor
  data=data[,c(xx,9)]
  data=na.omit(data)
  smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>8){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(rep("",length(seq(range[1],range[2],by=2)))),
     pos=xrange[1],tck= -0.025)

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     pos=xrange[1],tck= -0.025)


axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"i",cex=2.5)


## Plot of female genitals cancer ------------------------------------
### WARNING! Due to bag in NORDCAN database (Denmark mortality data in 1969
### for some female cancers), the nonsense data were removed from table
### and code here was slightly modified compared to previous.
### The bug was reported to NORDCAN secretary on 7th of November, 2022

summary(fem_gen_inc_mor)

range<-c(0,2);scal<-range[2]-range[1]
xrange<-c(1961,2020)

plot(NULL,xlim=xrange,ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(xrange[1],range[2],xrange[2],range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(xrange[1],xrange[2]),c(x,x),col="white")
  x=x+2;if(x>range[2]){break}}

x<- round(xrange[1]+5,-1)
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white")
  x=x+10;if(x>2020){break}}

xx<-5
repeat{
  data=fem_gen_inc_mor
  data=data[,c(xx,9)]
  data=na.omit(data)
  smoothingSpline = smooth.spline(data[,2], data[,1], spar=spar, nknots=knot)
  lines(smoothingSpline,col=colc[xx-4],lty=1,lwd=2,lend=1)
  xx=xx+1;if(xx>8){break}}

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=2),
     labels=c(range[1], rep("",length(seq(range[1],range[2],by=2))-2),range[2]),
     pos=xrange[1],tck= -0.025)
axis(2,las=2,cex.axis=1.4,at=c(range[1],range[2]),pos=xrange[1],tck= -0.025)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(round(xrange[1]+5,-1),2020,by=10)),pos=range[1],
     tck= -0.025)
lines(c(xrange[1],xrange[2]),c(range[1],range[1]))
title(xlab="Year", line=0.7, cex.lab=1.4,xpd=TRUE)
text(1967,range[2]-0.05*scal,"j",cex=2.5)



