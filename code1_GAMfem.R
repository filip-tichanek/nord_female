rm(list=ls())
library(brms)
library(stringr)
library(dplyr)



# colors ---------------------------------------------------------

cola<-c(
  rgb(1,0,0,alpha=1),
  rgb(0.2,0.2,1,alpha=1),
  rgb(0, 0.7, 0,alpha=1),
  rgb(0.7,0.7,0.1,alpha=1))

alp=0.18;colb<-c(
  rgb(1,0,0,alpha=alp),
  rgb(0.2,0.2,1,alpha=alp),
  rgb(0, 0.9, 0,alpha=alp),
  rgb(0.7,0.7,0.1,alpha=alp))

colc<-c(
  rgb(1,0,0,alpha=0.8),
  rgb(0.2,0.2,1,alpha=0.8),
  rgb(0, 0.9, 0,alpha=0.8),
  rgb(0.7,0.7,0.1,alpha=0.8))

# Defining priors -------------------------
prior_group <- c(
  set_prior("normal(0,30)", class = "b", coef = "groupFinland"),
  set_prior("normal(0,30)", class = "b", coef = "groupNorway"),
  set_prior("normal(0,30)", class = "b", coef = "groupSweden"))


# Data, models and posterior sample extraction ---------------------------------------------------------

## breast data -------------
breast_1y<-read.csv("source_data/breast_1y.csv",sep=";")
breast_5y<-read.csv("source_data/breast_5y.csv",sep=";")

x<-2;breast_1y_est<-data.frame(breast_1y[,1])
repeat{breast_1y_est[,x]<-str_sub(breast_1y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{breast_1y_est[,x]<-str_sub(breast_1y[,x-4],6,9);x<-x+1;if(x>9){break}}
x<-2;breast_5y_est<-data.frame(breast_5y[,1])
repeat{breast_5y_est[,x]<-str_sub(breast_5y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{breast_5y_est[,x]<-str_sub(breast_5y[,x-4],6,9);x<-x+1;if(x>9){break}}

breast<-(data.frame(unlist(breast_1y_est[,2:5])));colnames(breast)<-"surv_1y"
breast$cil_1y<-as.numeric(unlist(breast_1y_est[,6:9]))
breast$surv_5y<-as.numeric(unlist(breast_5y_est[,2:5]))
breast$cil_5y<-as.numeric(unlist(breast_5y_est[,6:9]))
breast$year<-rep(seq(1973,2018,by=5),4)
breast$sex<-c(rep("Females",40))
breast$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),rep("Sweden",10)),1))
breast$shou<-c(rep(c(rep("den_",10),rep("fin_",10),rep("nor_",10),rep("swe_",10)),1))
breast$group<-(breast$country)
breast$years10cen<-(breast$year-1995.5)/10
breast$surv_1y<-as.numeric(breast$surv_1y)
breast$surv_5y<-as.numeric(breast$surv_5y)
breast$cil_1y<-as.numeric(breast$cil_1y)
breast$cil_5y<-as.numeric(breast$cil_5y)
breast$se_1y<-(breast$surv_1y-breast$cil_1y)/1.96
breast$se_5y<-(breast$surv_5y-breast$cil_5y)/1.96
breast$surv_cond<-(breast$surv_5y/breast$surv_1y)*100

## breast model -----
set.seed(17)
breast_1y_model<-brm(surv_1y|se(se_1y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=breast,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))

summary(breast_1y_model)
fixef(breast_1y_model)
conditional_effects(breast_1y_model,effects='years10cen:group')

breast_5y_model<-brm(surv_5y|se(se_5y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=breast,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))
conditional_effects(breast_5y_model,effects='years10cen:group')


## vulva data -------------
vulva_1y<-read.csv("source_data/vulva_1y.csv",sep=",")
vulva_5y<-read.csv("source_data/vulva_5y.csv",sep=",")

x<-2;vulva_1y_est<-data.frame(vulva_1y[,1])
repeat{vulva_1y_est[,x]<-str_sub(vulva_1y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{vulva_1y_est[,x]<-str_sub(vulva_1y[,x-4],6,9);x<-x+1;if(x>9){break}}
x<-2;vulva_5y_est<-data.frame(vulva_5y[,1])
repeat{vulva_5y_est[,x]<-str_sub(vulva_5y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{vulva_5y_est[,x]<-str_sub(vulva_5y[,x-4],6,9);x<-x+1;if(x>9){break}}

vulva<-(data.frame(unlist(vulva_1y_est[,2:5])));colnames(vulva)<-"surv_1y"
vulva$cil_1y<-as.numeric(unlist(vulva_1y_est[,6:9]))
vulva$surv_5y<-as.numeric(unlist(vulva_5y_est[,2:5]))
vulva$cil_5y<-as.numeric(unlist(vulva_5y_est[,6:9]))
vulva$year<-rep(seq(1973,2018,by=5),4)
vulva$sex<-c(rep("Females",40))
vulva$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),rep("Sweden",10)),1))
vulva$shou<-c(rep(c(rep("den_",10),rep("fin_",10),rep("nor_",10),rep("swe_",10)),1))
vulva$group<-(vulva$country)
vulva$years10cen<-(vulva$year-1995.5)/10
vulva$surv_1y<-as.numeric(vulva$surv_1y)
vulva$surv_5y<-as.numeric(vulva$surv_5y)
vulva$cil_1y<-as.numeric(vulva$cil_1y)
vulva$cil_5y<-as.numeric(vulva$cil_5y)
vulva$se_1y<-(vulva$surv_1y-vulva$cil_1y)/1.96
vulva$se_5y<-(vulva$surv_5y-vulva$cil_5y)/1.96
vulva$surv_cond<-(vulva$surv_5y/vulva$surv_1y)*100

## vulva model -----
set.seed(17)
vulva_1y_model<-brm(surv_1y|se(se_1y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=vulva,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))

summary(vulva_1y_model)
fixef(vulva_1y_model)
conditional_effects(vulva_1y_model,effects='years10cen:group')

vulva_5y_model<-brm(surv_5y|se(se_5y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=vulva,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))
conditional_effects(vulva_5y_model,effects='years10cen:group')


## ovary data -------------
ovary_1y<-read.csv("source_data/ovary_1y.csv",sep=";")
ovary_5y<-read.csv("source_data/ovary_5y.csv",sep=";")

x<-2;ovary_1y_est<-data.frame(ovary_1y[,1])
repeat{ovary_1y_est[,x]<-str_sub(ovary_1y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{ovary_1y_est[,x]<-str_sub(ovary_1y[,x-4],6,9);x<-x+1;if(x>9){break}}
x<-2;ovary_5y_est<-data.frame(ovary_5y[,1])
repeat{ovary_5y_est[,x]<-str_sub(ovary_5y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{ovary_5y_est[,x]<-str_sub(ovary_5y[,x-4],6,9);x<-x+1;if(x>9){break}}

ovary<-(data.frame(unlist(ovary_1y_est[,2:5])));colnames(ovary)<-"surv_1y"
ovary$cil_1y<-as.numeric(unlist(ovary_1y_est[,6:9]))
ovary$surv_5y<-as.numeric(unlist(ovary_5y_est[,2:5]))
ovary$cil_5y<-as.numeric(unlist(ovary_5y_est[,6:9]))
ovary$year<-rep(seq(1973,2018,by=5),4)
ovary$sex<-c(rep("Females",40))
ovary$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),rep("Sweden",10)),1))
ovary$shou<-c(rep(c(rep("den_",10),rep("fin_",10),rep("nor_",10),rep("swe_",10)),1))
ovary$group<-(ovary$country)
ovary$years10cen<-(ovary$year-1995.5)/10
ovary$surv_1y<-as.numeric(ovary$surv_1y)
ovary$surv_5y<-as.numeric(ovary$surv_5y)
ovary$cil_1y<-as.numeric(ovary$cil_1y)
ovary$cil_5y<-as.numeric(ovary$cil_5y)
ovary$se_1y<-(ovary$surv_1y-ovary$cil_1y)/1.96
ovary$se_5y<-(ovary$surv_5y-ovary$cil_5y)/1.96
ovary$surv_cond<-(ovary$surv_5y/ovary$surv_1y)*100

## ovary model -----
set.seed(17)
ovary_1y_model<-brm(surv_1y|se(se_1y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=ovary,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))

summary(ovary_1y_model)
fixef(ovary_1y_model)
conditional_effects(ovary_1y_model,effects='years10cen:group')

ovary_5y_model<-brm(surv_5y|se(se_5y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=ovary,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))
conditional_effects(ovary_5y_model,effects='years10cen:group')


## corpus data -------------
corpus_1y<-read.csv("source_data/corpus_1y.csv",sep=";")
corpus_5y<-read.csv("source_data/corpus_5y.csv",sep=";")

x<-2;corpus_1y_est<-data.frame(corpus_1y[,1])
repeat{corpus_1y_est[,x]<-str_sub(corpus_1y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{corpus_1y_est[,x]<-str_sub(corpus_1y[,x-4],6,9);x<-x+1;if(x>9){break}}
x<-2;corpus_5y_est<-data.frame(corpus_5y[,1])
repeat{corpus_5y_est[,x]<-str_sub(corpus_5y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{corpus_5y_est[,x]<-str_sub(corpus_5y[,x-4],6,9);x<-x+1;if(x>9){break}}

corpus<-(data.frame(unlist(corpus_1y_est[,2:5])));colnames(corpus)<-"surv_1y"
corpus$cil_1y<-as.numeric(unlist(corpus_1y_est[,6:9]))
corpus$surv_5y<-as.numeric(unlist(corpus_5y_est[,2:5]))
corpus$cil_5y<-as.numeric(unlist(corpus_5y_est[,6:9]))
corpus$year<-rep(seq(1973,2018,by=5),4)
corpus$sex<-c(rep("Females",40))
corpus$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),rep("Sweden",10)),1))
corpus$shou<-c(rep(c(rep("den_",10),rep("fin_",10),rep("nor_",10),rep("swe_",10)),1))
corpus$group<-(corpus$country)
corpus$years10cen<-(corpus$year-1995.5)/10
corpus$surv_1y<-as.numeric(corpus$surv_1y)
corpus$surv_5y<-as.numeric(corpus$surv_5y)
corpus$cil_1y<-as.numeric(corpus$cil_1y)
corpus$cil_5y<-as.numeric(corpus$cil_5y)
corpus$se_1y<-(corpus$surv_1y-corpus$cil_1y)/1.96
corpus$se_5y<-(corpus$surv_5y-corpus$cil_5y)/1.96
corpus$surv_cond<-(corpus$surv_5y/corpus$surv_1y)*100

## corpus model -----
set.seed(17)
corpus_1y_model<-brm(surv_1y|se(se_1y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=corpus,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))

summary(corpus_1y_model)
fixef(corpus_1y_model)
conditional_effects(corpus_1y_model,effects='years10cen:group')

corpus_5y_model<-brm(surv_5y|se(se_5y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=corpus,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))
conditional_effects(corpus_5y_model,effects='years10cen:group')



## cervix data -------------
cervix_1y<-read.csv("source_data/cervix_1y.csv",sep=";")
cervix_5y<-read.csv("source_data/cervix_5y.csv",sep=";")

x<-2;cervix_1y_est<-data.frame(cervix_1y[,1])
repeat{cervix_1y_est[,x]<-str_sub(cervix_1y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{cervix_1y_est[,x]<-str_sub(cervix_1y[,x-4],6,9);x<-x+1;if(x>9){break}}
x<-2;cervix_5y_est<-data.frame(cervix_5y[,1])
repeat{cervix_5y_est[,x]<-str_sub(cervix_5y[,x],1,4);x<-x+1
if(x>5){break}}
repeat{cervix_5y_est[,x]<-str_sub(cervix_5y[,x-4],6,9);x<-x+1;if(x>9){break}}

cervix<-(data.frame(unlist(cervix_1y_est[,2:5])));colnames(cervix)<-"surv_1y"
cervix$cil_1y<-as.numeric(unlist(cervix_1y_est[,6:9]))
cervix$surv_5y<-as.numeric(unlist(cervix_5y_est[,2:5]))
cervix$cil_5y<-as.numeric(unlist(cervix_5y_est[,6:9]))
cervix$year<-rep(seq(1973,2018,by=5),4)
cervix$sex<-c(rep("Females",40))
cervix$country<-c(rep(c(rep("Denmark",10),rep("Finland",10),rep("Norway",10),rep("Sweden",10)),1))
cervix$shou<-c(rep(c(rep("den_",10),rep("fin_",10),rep("nor_",10),rep("swe_",10)),1))
cervix$group<-(cervix$country)
cervix$years10cen<-(cervix$year-1995.5)/10
cervix$surv_1y<-as.numeric(cervix$surv_1y)
cervix$surv_5y<-as.numeric(cervix$surv_5y)
cervix$cil_1y<-as.numeric(cervix$cil_1y)
cervix$cil_5y<-as.numeric(cervix$cil_5y)
cervix$se_1y<-(cervix$surv_1y-cervix$cil_1y)/1.96
cervix$se_5y<-(cervix$surv_5y-cervix$cil_5y)/1.96
cervix$surv_cond<-(cervix$surv_5y/cervix$surv_1y)*100

## cervix model -----
set.seed(17)
cervix_1y_model<-brm(surv_1y|se(se_1y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=cervix,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))

summary(cervix_1y_model)
fixef(cervix_1y_model)
conditional_effects(cervix_1y_model,effects='years10cen:group')

cervix_5y_model<-brm(surv_5y|se(se_5y)~group+s(years10cen,by=group,k=5)
                     ,family="Gaussian",save_pars = save_pars(all = TRUE),prior=prior_group,seed=17
                     ,data=cervix,iter=7000, warmup=2000,chains=2,cores=1,control = list(adapt_delta = 0.98))
conditional_effects(cervix_5y_model,effects='years10cen:group')




# functions ---------------

breakpo<-function(data,arb){ 
  data<-data.frame((data[,-1] - data[,-ncol(data)])*10)
  data<-data.frame(data[,-1] - data[,-ncol(data)])
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[5,]<-yreal[c(2:499)]-0.049
  data[6,]<-1:498;data[4,51]<-1;data[4,449]<-1
  
  row.names(data)<-c("cil","ciu","est","stat_change","year","timepoint")
  data2=t(data[,data[4,]==1])
  data2<-data.frame(data2)
  tr<-subset(data2,data2$timepoint>50&data2$timepoint<450)
  y=1
  bp<-c()
  bx<-1
  repeat{
    if( (tr[y,1]<0) & (tr[y,2]<0) & (  (tr$year[y+1]-tr$year[y])>3   )  ) {
      tr2<-data[,tr[y,6]:(tr[y+1,6]-2)]
      bp[bx]<- tr2[,order(tr2[3,],decreasing=F)[1]][5]
      bx=bx+1
    }
    if( (tr[y,1]>0) & (tr[y,2]>0) & (  (tr$year[y+1]-tr$year[y])>3   )) {
      tr2<-data[,tr[y,6]:(tr[y+1,6]-2)]
      bp[bx]<- tr2[,order(tr2[3,],decreasing=T)[1]][5]
      bx=bx+1
    }
    y=y+1;if(y>(dim(tr)[1]-1)){break}}
  y<-1;repeat{
    lines(c(bp[y]+arb,bp[y]+arb),c(range[1],range[1]+0.025*scal),col=cola[xx],lwd=3.5,lend=1)
    lines(c(bp[y]+arb,bp[y]+arb),c(range[2],range[2]-0.025*scal),col=cola[xx],lwd=3.5,lend=1)
    lines(c(bp[y]+arb,bp[y]+arb),c(range[1],range[1]+0.999*scal),col=colc[xx],lwd=1,lend=1,lty=2)
    y=y+1;if(y>length(bp)){break}}
  print(bp)
  print(tr)}



polyg_surv<-function(data){ 
  data<-data.frame(data)
  data<-sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cis<-c(data[1,],rev(data[2,]))
  x<-c(yreal[1:500],yreal[500:1])
  cis[cis<range[1]]<-range[1]
  cis[cis>range[2]]<-range[2]
  polygon(x,cis,border=NA,col=colb[xx],xpd=F)}


surv_fit<-function(dat2){
  dat2=data.frame(dat2)
  cis= data.frame((dat2[,-1] - dat2[,-ncol(dat2)])*10)
  cis=sapply(cis, function(p) quantile(p, probs = c(0.025,0.975)))
  est=sapply(dat2, function(p) quantile(p, probs = c(0.5)))
  data=cis
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[4,]<-yreal[c(2:500)]-0.049;data[5,]<-1:499
  row.names(data)<-c("cil","ciu","stat_change","year","timepoint");data[3,499]<-1
  data2=t(data[,data[3,]==1])
  data2<-data.frame(data2)
  tr<-data2;y=1
  yreal=seq(1971,2020,length=500)
  repeat{
    if((((tr[y,1]<0)&(tr[y,2])<0))|(((tr[y,1]>0)&(tr[y,2])>0))&(tr[y+1,4]-tr[y,4])>5)
    {lines(yreal[tr[y,5]:tr[y+1,5]],
           est[tr[y,5]:tr[y+1,5]],lwd=1.9,col=cola[xx],lend=1)}
    y<-y+1;if(y>dim(tr)[1]-1){break}}
  lines(yreal[1:500],est,lwd=1,col=cola[xx],lty=2,lend=1)}

polyg_slope<-function(data){ 
  x<-c(yreal[1:499],yreal[499:1])
  data<-data.frame((data[,-1] - data[,-ncol(data)]))*10
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cis<-c(data[1,],rev(data[2,]))
  cis[cis<range[1]]<-range[1]
  cis[cis>range[2]]<-range[2]
  polygon(x,cis,border=NA,col=colb[xx])}

slope_fit<-function(data){
  data = data.frame((data[,-1] - data[,-ncol(data)])*10)
  dar2<-sapply(data, function(p) quantile(p, probs = c(0.5)))
  data<-data.frame(data[,-1] - data[,-ncol(data)])
  data=sapply(data, function(p) quantile(p, probs = c(0.025,0.975,0.5)))
  cbinl<-c()
  x=1
  repeat{
    cbinl[x]<-
      if(data[1,x]>0|data[2,x]<0){cbinl[x]=1}else{cbinl[x]=0}
    x<-x+1
    if(x>length(data[1,])){break}}
  cbin=1;x<-1
  repeat{
    cbin[x+1]<-abs(cbinl[x]-cbinl[x+1])
    x=x+1
    if(x>(length(cbinl)-1)){break}}
  data<-data.frame(rbind(data,cbin));data[5,]<-yreal[c(2:499)]-0.049;data[6,]<-1:498
  row.names(data)<-c("cil","ciu","est","stat_change","year","timepoint")
  data[4,450]<-1
  data[4,50]<-1
  data=data[,50:450]
  lines(yreal[1:499],dar2,lwd=1,col=cola[xx],lty=2,lend=1)
  data2=(t(data[,data[4,]==1]))
  data2<-data.frame(data2)
  tr<-data2;y=1
  yreal=seq(1971,2020,length=498)
  repeat{
    if(((((tr[y,1]<0)&(tr[y,2])<0))|(((tr[y,1]>0)&(tr[y,2])>0)))&(tr[y+1,5]-tr[y,5])>3)
    {lines(yreal[tr[y,6]:tr[y+1,6]],
           dar2[tr[y,6]:tr[y+1,6]]
           ,lwd=1.9,col=cola[xx],lend=1)}
    y<-y+1;if(y>dim(tr)[1]-1){break}}
  return(data2)}





# Posterior extraction ----------------------------------------------------
## breast posterior extraction ---------------------------------------------------
yreal <- seq(1971,2020,length=500)
first <- expand.grid(years10cen = ((yreal-1995.5)/10),
                     group = c('Denmark','Finland','Norway','Sweden'), y = 0)

ms_1y<-posterior_smooths(breast_1y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(breast_1y_model, variable = c("b_Intercept","b_groupFinland",
      "b_groupNorway","b_groupSweden"))

post_breast_den_1y<-ms_1y[,1:500]     +post_fix[,1]
post_breast_fin_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_breast_nor_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_breast_swe_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]

ms_5y<-posterior_smooths(breast_5y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(breast_5y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))
post_breast_den_5y<-ms_5y[,1:500]     +post_fix[,1]
post_breast_fin_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_breast_nor_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_breast_swe_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]

post_breast_den_cond<-(post_breast_den_5y/post_breast_den_1y)*100
post_breast_fin_cond<-(post_breast_fin_5y/post_breast_fin_1y)*100
post_breast_nor_cond<-(post_breast_nor_5y/post_breast_nor_1y)*100
post_breast_swe_cond<-(post_breast_swe_5y/post_breast_swe_1y)*100


## corpus posterior extraction ---------------------------------------------------
ms_1y<-posterior_smooths(corpus_1y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(corpus_1y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))

post_corpus_den_1y<-ms_1y[,1:500]     +post_fix[,1]
post_corpus_fin_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_corpus_nor_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_corpus_swe_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]

ms_5y<-posterior_smooths(corpus_5y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(corpus_5y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))
post_corpus_den_5y<-ms_5y[,1:500]     +post_fix[,1]
post_corpus_fin_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_corpus_nor_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_corpus_swe_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]

post_corpus_den_cond<-(post_corpus_den_5y/post_corpus_den_1y)*100
post_corpus_fin_cond<-(post_corpus_fin_5y/post_corpus_fin_1y)*100
post_corpus_nor_cond<-(post_corpus_nor_5y/post_corpus_nor_1y)*100
post_corpus_swe_cond<-(post_corpus_swe_5y/post_corpus_swe_1y)*100


## ovary posterior extraction ---------------------------------------------------
ms_1y<-posterior_smooths(ovary_1y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(ovary_1y_model, variable = c("b_Intercept","b_groupFinland",
                                                     "b_groupNorway","b_groupSweden"))

post_ovary_den_1y<-ms_1y[,1:500]     +post_fix[,1]
post_ovary_fin_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_ovary_nor_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_ovary_swe_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]

ms_5y<-posterior_smooths(ovary_5y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(ovary_5y_model, variable = c("b_Intercept","b_groupFinland",
                                                     "b_groupNorway","b_groupSweden"))
post_ovary_den_5y<-ms_5y[,1:500]     +post_fix[,1]
post_ovary_fin_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_ovary_nor_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_ovary_swe_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]

post_ovary_den_cond<-(post_ovary_den_5y/post_ovary_den_1y)*100
post_ovary_fin_cond<-(post_ovary_fin_5y/post_ovary_fin_1y)*100
post_ovary_nor_cond<-(post_ovary_nor_5y/post_ovary_nor_1y)*100
post_ovary_swe_cond<-(post_ovary_swe_5y/post_ovary_swe_1y)*100


## cervix posterior extraction ---------------------------------------------------
ms_1y<-posterior_smooths(cervix_1y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(cervix_1y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))

post_cervix_den_1y<-ms_1y[,1:500]     +post_fix[,1]
post_cervix_fin_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_cervix_nor_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_cervix_swe_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]

ms_5y<-posterior_smooths(cervix_5y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(cervix_5y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))
post_cervix_den_5y<-ms_5y[,1:500]     +post_fix[,1]
post_cervix_fin_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_cervix_nor_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_cervix_swe_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]

post_cervix_den_cond<-(post_cervix_den_5y/post_cervix_den_1y)*100
post_cervix_fin_cond<-(post_cervix_fin_5y/post_cervix_fin_1y)*100
post_cervix_nor_cond<-(post_cervix_nor_5y/post_cervix_nor_1y)*100
post_cervix_swe_cond<-(post_cervix_swe_5y/post_cervix_swe_1y)*100





## vulva posterior extraction ---------------------------------------------------
ms_1y<-posterior_smooths(vulva_1y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(vulva_1y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))

post_vulva_den_1y<-ms_1y[,1:500]     +post_fix[,1]
post_vulva_fin_1y<-ms_1y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_vulva_nor_1y<-ms_1y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_vulva_swe_1y<-ms_1y[,1501:2000] +post_fix[,1]+post_fix[,4]

ms_5y<-posterior_smooths(vulva_5y_model,smooth="s(years10cen,by=group,k=5)",newdata = first)
post_fix<-as.data.frame(vulva_5y_model, variable = c("b_Intercept","b_groupFinland",
                                                      "b_groupNorway","b_groupSweden"))
post_vulva_den_5y<-ms_5y[,1:500]     +post_fix[,1]
post_vulva_fin_5y<-ms_5y[,501:1000]  +post_fix[,1]+post_fix[,2]
post_vulva_nor_5y<-ms_5y[,1001:1500] +post_fix[,1]+post_fix[,3]
post_vulva_swe_5y<-ms_5y[,1501:2000] +post_fix[,1]+post_fix[,4]

post_vulva_den_cond<-(post_vulva_den_5y/post_vulva_den_1y)*100
post_vulva_fin_cond<-(post_vulva_fin_5y/post_vulva_fin_1y)*100
post_vulva_nor_cond<-(post_vulva_nor_5y/post_vulva_nor_1y)*100
post_vulva_swe_cond<-(post_vulva_swe_5y/post_vulva_swe_1y)*100




# Plotting 1 - survival trends ----------------


## DEN figures --------------------------- -----------------
m <- matrix(c(18,1,2, 3, 4, 5
             ,6,8,10,12,14,16
             ,7,9,11,13,15,17), nrow = 3, ncol =6 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6,0.37),
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

range_b<-c(10,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,1.5);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(c(1.3,1.3,1.3),c(range_b[1],50,range_b[2]),
     c(range_b[1],50,range_b[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_b[1]+scal_b*0.5,"Relative survival (%)", cex=1.5,srt=90)

range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(c(1.3,1.3,1.3),c(range_c[1],0,range_c[2]),
     c(range_c[1],0,range_c[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_c[1]+scal_c*0.5,expression(paste(delta, " (survival)")),
     cex=1.5,srt=90)

### breast  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}}
lines(c(1971,2020),c(50,50),col="white",lwd=1.7)


x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_breast_den_1y,-1.2)
xx=xx+1
breakpo(post_breast_den_cond,0)
xx=xx+1
breakpo(post_breast_den_5y,1.2)

# 95% credible interval for survival
xx=1
polyg_surv(post_breast_den_1y);xx=xx+1
polyg_surv(post_breast_den_cond);xx=xx+1
polyg_surv(post_breast_den_5y)

# data points of estimated survival
xx=1
points(breast[breast$shou=="den_",]$surv_1y~breast[breast$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="den_",]$surv_cond~breast[breast$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="den_",]$surv_5y~breast[breast$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_breast_den_1y);xx=xx+1
surv_fit(post_breast_den_cond);xx=xx+1
surv_fit(post_breast_den_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
      
# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(1996+xpo,yy+0.035*scal,2017.9+xpo,yy-0.15*scal,col="white",border="grey50",lwd=0.8)
repeat{
  points(2000+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(1997.3+xpo,2002.7+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.058);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2010+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(1974,range[2]-0.05*scal,"a",cex=2.2)

#### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_breast_den_1y);xx=xx+1
polyg_slope(post_breast_den_cond);xx=xx+1
polyg_slope(post_breast_den_5y)

xx=1
slope_fit(post_breast_den_1y);xx=xx+1
slope_fit(post_breast_den_cond);xx=xx+1
slope_fit(post_breast_den_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### corpus  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_corpus_den_1y,0)
xx=xx+1
breakpo(post_corpus_den_cond,0)
xx=xx+1
breakpo(post_corpus_den_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_corpus_den_1y);xx=xx+1
polyg_surv(post_corpus_den_cond);xx=xx+1
polyg_surv(post_corpus_den_5y)

# data points of estimated survival
xx=1
points(corpus[corpus$shou=="den_",]$surv_1y~corpus[corpus$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="den_",]$surv_cond~corpus[corpus$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="den_",]$surv_5y~corpus[corpus$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_corpus_den_1y);xx=xx+1
surv_fit(post_corpus_den_cond);xx=xx+1
surv_fit(post_corpus_den_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"b",cex=2.2)


ypo=-64
xfl=33
rect(1955+xfl,range[1]+scal*0.965+ypo,1969+xfl,range[1]+scal*0.84+ypo,col="red2",border="grey50")
rect(1955+xfl,range[1]+scal*0.9135+ypo,1969+xfl,range[1]+scal*0.8865+ypo,col="white",border=NA)
rect(1960+xfl,range[1]+scal*0.965+ypo,1962+xfl,range[1]+scal*0.84+ypo,col="white",border=NA)



#### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_corpus_den_1y);xx=xx+1
polyg_slope(post_corpus_den_cond);xx=xx+1
polyg_slope(post_corpus_den_5y)

xx=1
slope_fit(post_corpus_den_1y);xx=xx+1
slope_fit(post_corpus_den_cond);xx=xx+1
slope_fit(post_corpus_den_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)






### ovary  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_ovary_den_1y,0)
xx=xx+1
breakpo(post_ovary_den_cond,0)
xx=xx+1
breakpo(post_ovary_den_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_ovary_den_1y);xx=xx+1
polyg_surv(post_ovary_den_cond);xx=xx+1
polyg_surv(post_ovary_den_5y)

# data points of estimated survival
xx=1
points(ovary[ovary$shou=="den_",]$surv_1y~ovary[ovary$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="den_",]$surv_cond~ovary[ovary$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="den_",]$surv_5y~ovary[ovary$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_ovary_den_1y);xx=xx+1
surv_fit(post_ovary_den_cond);xx=xx+1
surv_fit(post_ovary_den_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"c",cex=2.2)

#### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_ovary_den_1y);xx=xx+1
polyg_slope(post_ovary_den_cond);xx=xx+1
polyg_slope(post_ovary_den_5y)

xx=1
slope_fit(post_ovary_den_1y);xx=xx+1
slope_fit(post_ovary_den_cond);xx=xx+1
slope_fit(post_ovary_den_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)



### cervix  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_cervix_den_1y,0)
xx=xx+1
breakpo(post_cervix_den_cond,0)
xx=xx+1
breakpo(post_cervix_den_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_cervix_den_1y);xx=xx+1
polyg_surv(post_cervix_den_cond);xx=xx+1
polyg_surv(post_cervix_den_5y)

# data points of estimated survival
xx=1
points(cervix[cervix$shou=="den_",]$surv_1y~cervix[cervix$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="den_",]$surv_cond~cervix[cervix$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="den_",]$surv_5y~cervix[cervix$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_cervix_den_1y);xx=xx+1
surv_fit(post_cervix_den_cond);xx=xx+1
surv_fit(post_cervix_den_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"d",cex=2.2)

#### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_cervix_den_1y);xx=xx+1
polyg_slope(post_cervix_den_cond);xx=xx+1
polyg_slope(post_cervix_den_5y)

xx=1
slope_fit(post_cervix_den_1y);xx=xx+1
slope_fit(post_cervix_den_cond);xx=xx+1
slope_fit(post_cervix_den_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)







### vulva  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points identification
xx=1
breakpo(post_vulva_den_1y,0)
xx=xx+1
breakpo(post_vulva_den_cond,0)
xx=xx+1
breakpo(post_vulva_den_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_vulva_den_1y);xx=xx+1
polyg_surv(post_vulva_den_cond);xx=xx+1
polyg_surv(post_vulva_den_5y)

# data points of estimated survival
xx=1
points(vulva[vulva$shou=="den_",]$surv_1y~vulva[vulva$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="den_",]$surv_cond~vulva[vulva$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="den_",]$surv_5y~vulva[vulva$shou=="den_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_vulva_den_1y);xx=xx+1
surv_fit(post_vulva_den_cond);xx=xx+1
surv_fit(post_vulva_den_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"e",cex=2.2)

#### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_vulva_den_1y);xx=xx+1
polyg_slope(post_vulva_den_cond);xx=xx+1
polyg_slope(post_vulva_den_5y)

xx=1
slope_fit(post_vulva_den_1y);xx=xx+1
slope_fit(post_vulva_den_cond);xx=xx+1
slope_fit(post_vulva_den_5y)
axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)








## FIN figures ---------------------------- -----------------
m <- matrix(c(18,1,2, 3, 4, 5
              ,6,8,10,12,14,16
              ,7,9,11,13,15,17), nrow = 3, ncol =6 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6,0.37),
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

range_b<-c(10,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,1.5);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(c(1.3,1.3,1.3),c(range_b[1],50,range_b[2]),
     c(range_b[1],50,range_b[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_b[1]+scal_b*0.5,"Relative survival (%)", cex=1.5,srt=90)

range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(c(1.3,1.3,1.3),c(range_c[1],0,range_c[2]),
     c(range_c[1],0,range_c[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_c[1]+scal_c*0.5,expression(paste(delta, " (survival)")),
     cex=1.5,srt=90)

### breast  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points ifintification
xx=1
breakpo(post_breast_fin_1y,-0.9)
xx=xx+1
breakpo(post_breast_fin_cond,0)
xx=xx+1
breakpo(post_breast_fin_5y,0.9)

# 95% credible interval for survival
xx=1
polyg_surv(post_breast_fin_1y);xx=xx+1
polyg_surv(post_breast_fin_cond);xx=xx+1
polyg_surv(post_breast_fin_5y)

# data points of estimated survival
xx=1
points(breast[breast$shou=="fin_",]$surv_1y~breast[breast$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="fin_",]$surv_cond~breast[breast$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="fin_",]$surv_5y~breast[breast$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_breast_fin_1y);xx=xx+1
surv_fit(post_breast_fin_cond);xx=xx+1
surv_fit(post_breast_fin_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(1996+xpo,yy+0.035*scal,2017.9+xpo,yy-0.15*scal,col="white",border="grey50",lwd=0.8)
repeat{
  points(2000+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(1997.3+xpo,2002.7+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.058);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2010+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(1974,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_breast_fin_1y);xx=xx+1
polyg_slope(post_breast_fin_cond);xx=xx+1
polyg_slope(post_breast_fin_5y)

xx=1
slope_fit(post_breast_fin_1y);xx=xx+1
slope_fit(post_breast_fin_cond);xx=xx+1
slope_fit(post_breast_fin_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### corpus  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points ifintification
xx=1
breakpo(post_corpus_fin_1y,0)
xx=xx+1
breakpo(post_corpus_fin_cond,1.5)
xx=xx+1
breakpo(post_corpus_fin_5y,-1.5)

# 95% credible interval for survival
xx=1
polyg_surv(post_corpus_fin_1y);xx=xx+1
polyg_surv(post_corpus_fin_cond);xx=xx+1
polyg_surv(post_corpus_fin_5y)

# data points of estimated survival
xx=1
points(corpus[corpus$shou=="fin_",]$surv_1y~corpus[corpus$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="fin_",]$surv_cond~corpus[corpus$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="fin_",]$surv_5y~corpus[corpus$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_corpus_fin_1y);xx=xx+1
surv_fit(post_corpus_fin_cond);xx=xx+1
surv_fit(post_corpus_fin_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"b",cex=2.2)

ypo=-64
xfl=33
rect(1955+xfl,range[1]+scal*0.965+ypo,1969+xfl,range[1]+scal*0.84+ypo,col="white",border="grey50")
rect(1955+xfl,range[1]+scal*0.9135+ypo,1969+xfl,range[1]+scal*0.8865+ypo,col="dodgerblue3",border=NA)
rect(1960+xfl,range[1]+scal*0.965+ypo,1962+xfl,range[1]+scal*0.84+ypo,col="dodgerblue3",border=NA)



##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_corpus_fin_1y);xx=xx+1
polyg_slope(post_corpus_fin_cond);xx=xx+1
polyg_slope(post_corpus_fin_5y)

xx=1
slope_fit(post_corpus_fin_1y);xx=xx+1
slope_fit(post_corpus_fin_cond);xx=xx+1
slope_fit(post_corpus_fin_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)








### ovary  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points ifintification
xx=1
breakpo(post_ovary_fin_1y,0.9)
xx=xx+1
breakpo(post_ovary_fin_cond,0)
xx=xx+1
breakpo(post_ovary_fin_5y,-0.9)

# 95% credible interval for survival
xx=1
polyg_surv(post_ovary_fin_1y);xx=xx+1
polyg_surv(post_ovary_fin_cond);xx=xx+1
polyg_surv(post_ovary_fin_5y)

# data points of estimated survival
xx=1
points(ovary[ovary$shou=="fin_",]$surv_1y~ovary[ovary$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="fin_",]$surv_cond~ovary[ovary$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="fin_",]$surv_5y~ovary[ovary$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_ovary_fin_1y);xx=xx+1
surv_fit(post_ovary_fin_cond);xx=xx+1
surv_fit(post_ovary_fin_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"c",cex=2.2)


##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_ovary_fin_1y);xx=xx+1
polyg_slope(post_ovary_fin_cond);xx=xx+1
polyg_slope(post_ovary_fin_5y)

xx=1
slope_fit(post_ovary_fin_1y);xx=xx+1
slope_fit(post_ovary_fin_cond);xx=xx+1
slope_fit(post_ovary_fin_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### cervix  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points ifintification
xx=1
breakpo(post_cervix_fin_1y,0)
xx=xx+1
breakpo(post_cervix_fin_cond,0)
xx=xx+1
breakpo(post_cervix_fin_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_cervix_fin_1y);xx=xx+1
polyg_surv(post_cervix_fin_cond);xx=xx+1
polyg_surv(post_cervix_fin_5y)

# data points of estimated survival
xx=1
points(cervix[cervix$shou=="fin_",]$surv_1y~cervix[cervix$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="fin_",]$surv_cond~cervix[cervix$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="fin_",]$surv_5y~cervix[cervix$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_cervix_fin_1y);xx=xx+1
surv_fit(post_cervix_fin_cond);xx=xx+1
surv_fit(post_cervix_fin_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_cervix_fin_1y);xx=xx+1
polyg_slope(post_cervix_fin_cond);xx=xx+1
polyg_slope(post_cervix_fin_5y)

xx=1
slope_fit(post_cervix_fin_1y);xx=xx+1
slope_fit(post_cervix_fin_cond);xx=xx+1
slope_fit(post_cervix_fin_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)









### vulva  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points ifintification
xx=1
breakpo(post_vulva_fin_1y,0)
xx=xx+1
breakpo(post_vulva_fin_cond,0)
xx=xx+1
breakpo(post_vulva_fin_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_vulva_fin_1y);xx=xx+1
polyg_surv(post_vulva_fin_cond);xx=xx+1
polyg_surv(post_vulva_fin_5y)

# data points of estimated survival
xx=1
points(vulva[vulva$shou=="fin_",]$surv_1y~vulva[vulva$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="fin_",]$surv_cond~vulva[vulva$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="fin_",]$surv_5y~vulva[vulva$shou=="fin_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_vulva_fin_1y);xx=xx+1
surv_fit(post_vulva_fin_cond);xx=xx+1
surv_fit(post_vulva_fin_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"e",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_vulva_fin_1y);xx=xx+1
polyg_slope(post_vulva_fin_cond);xx=xx+1
polyg_slope(post_vulva_fin_5y)

xx=1
slope_fit(post_vulva_fin_1y);xx=xx+1
slope_fit(post_vulva_fin_cond);xx=xx+1
slope_fit(post_vulva_fin_5y)
axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





## NOR figures ---------------------------- -----------------
m <- matrix(c(18,1,2, 3, 4, 5
              ,6,8,10,12,14,16
              ,7,9,11,13,15,17), nrow = 3, ncol =6 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6,0.37),
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

range_b<-c(10,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,1.5);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(c(1.3,1.3,1.3),c(range_b[1],50,range_b[2]),
     c(range_b[1],50,range_b[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_b[1]+scal_b*0.5,"Relative survival (%)", cex=1.5,srt=90)

range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(c(1.3,1.3,1.3),c(range_c[1],0,range_c[2]),
     c(range_c[1],0,range_c[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_c[1]+scal_c*0.5,expression(paste(delta, " (survival)")),
     cex=1.5,srt=90)

### breast  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points inortification
xx=1
breakpo(post_breast_nor_1y,0)
xx=xx+1
breakpo(post_breast_nor_cond,0)
xx=xx+1
breakpo(post_breast_nor_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_breast_nor_1y);xx=xx+1
polyg_surv(post_breast_nor_cond);xx=xx+1
polyg_surv(post_breast_nor_5y)

# data points of estimated survival
xx=1
points(breast[breast$shou=="nor_",]$surv_1y~breast[breast$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="nor_",]$surv_cond~breast[breast$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="nor_",]$surv_5y~breast[breast$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_breast_nor_1y);xx=xx+1
surv_fit(post_breast_nor_cond);xx=xx+1
surv_fit(post_breast_nor_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(1996+xpo,yy+0.035*scal,2017.9+xpo,yy-0.15*scal,col="white",border="grey50",lwd=0.8)
repeat{
  points(2000+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(1997.3+xpo,2002.7+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.058);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2010+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(1974,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_breast_nor_1y);xx=xx+1
polyg_slope(post_breast_nor_cond);xx=xx+1
polyg_slope(post_breast_nor_5y)

xx=1
slope_fit(post_breast_nor_1y);xx=xx+1
slope_fit(post_breast_nor_cond);xx=xx+1
slope_fit(post_breast_nor_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### corpus  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points inortification
xx=1
breakpo(post_corpus_nor_1y,0)
xx=xx+1
breakpo(post_corpus_nor_cond,0)
xx=xx+1
breakpo(post_corpus_nor_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_corpus_nor_1y);xx=xx+1
polyg_surv(post_corpus_nor_cond);xx=xx+1
polyg_surv(post_corpus_nor_5y)

# data points of estimated survival
xx=1
points(corpus[corpus$shou=="nor_",]$surv_1y~corpus[corpus$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="nor_",]$surv_cond~corpus[corpus$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="nor_",]$surv_5y~corpus[corpus$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_corpus_nor_1y);xx=xx+1
surv_fit(post_corpus_nor_cond);xx=xx+1
surv_fit(post_corpus_nor_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"b",cex=2.2)


ypo=-64
xfl=33
rect(1955+xfl,range[1]+scal*0.965+ypo,1969+xfl,range[1]+scal*0.84+ypo,col="red2",border="grey50")
rect(1955+xfl,range[1]+scal*0.9285+ypo,1969+xfl,range[1]+scal*0.8735+ypo,col="white",border=NA)
rect(1959+xfl,range[1]+scal*0.965+ypo,1963+xfl,range[1]+scal*0.84+ypo,col="white",border=NA)
rect(1955+xfl,range[1]+scal*0.9135+ypo,1969+xfl,range[1]+scal*0.8865+ypo,col="darkblue",border=NA)
rect(1960+xfl,range[1]+scal*0.965+ypo,1962+xfl,range[1]+scal*0.84+ypo,col="darkblue",border=NA)



##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_corpus_nor_1y);xx=xx+1
polyg_slope(post_corpus_nor_cond);xx=xx+1
polyg_slope(post_corpus_nor_5y)

xx=1
slope_fit(post_corpus_nor_1y);xx=xx+1
slope_fit(post_corpus_nor_cond);xx=xx+1
slope_fit(post_corpus_nor_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### ovary  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points inortification
xx=1
breakpo(post_ovary_nor_1y,0)
xx=xx+1
breakpo(post_ovary_nor_cond,0)
xx=xx+1
breakpo(post_ovary_nor_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_ovary_nor_1y);xx=xx+1
polyg_surv(post_ovary_nor_cond);xx=xx+1
polyg_surv(post_ovary_nor_5y)

# data points of estimated survival
xx=1
points(ovary[ovary$shou=="nor_",]$surv_1y~ovary[ovary$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="nor_",]$surv_cond~ovary[ovary$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="nor_",]$surv_5y~ovary[ovary$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_ovary_nor_1y);xx=xx+1
surv_fit(post_ovary_nor_cond);xx=xx+1
surv_fit(post_ovary_nor_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"c",cex=2.2)




##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_ovary_nor_1y);xx=xx+1
polyg_slope(post_ovary_nor_cond);xx=xx+1
polyg_slope(post_ovary_nor_5y)

xx=1
slope_fit(post_ovary_nor_1y);xx=xx+1
slope_fit(post_ovary_nor_cond);xx=xx+1
slope_fit(post_ovary_nor_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### cervix  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points inortification
xx=1
breakpo(post_cervix_nor_1y,0)
xx=xx+1
breakpo(post_cervix_nor_cond,0)
xx=xx+1
breakpo(post_cervix_nor_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_cervix_nor_1y);xx=xx+1
polyg_surv(post_cervix_nor_cond);xx=xx+1
polyg_surv(post_cervix_nor_5y)

# data points of estimated survival
xx=1
points(cervix[cervix$shou=="nor_",]$surv_1y~cervix[cervix$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="nor_",]$surv_cond~cervix[cervix$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="nor_",]$surv_5y~cervix[cervix$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_cervix_nor_1y);xx=xx+1
surv_fit(post_cervix_nor_cond);xx=xx+1
surv_fit(post_cervix_nor_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_cervix_nor_1y);xx=xx+1
polyg_slope(post_cervix_nor_cond);xx=xx+1
polyg_slope(post_cervix_nor_5y)

xx=1
slope_fit(post_cervix_nor_1y);xx=xx+1
slope_fit(post_cervix_nor_cond);xx=xx+1
slope_fit(post_cervix_nor_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)







### vulva  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points inortification
xx=1
breakpo(post_vulva_nor_1y,0)
xx=xx+1
breakpo(post_vulva_nor_cond,0)
xx=xx+1
breakpo(post_vulva_nor_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_vulva_nor_1y);xx=xx+1
polyg_surv(post_vulva_nor_cond);xx=xx+1
polyg_surv(post_vulva_nor_5y)

# data points of estimated survival
xx=1
points(vulva[vulva$shou=="nor_",]$surv_1y~vulva[vulva$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="nor_",]$surv_cond~vulva[vulva$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="nor_",]$surv_5y~vulva[vulva$shou=="nor_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_vulva_nor_1y);xx=xx+1
surv_fit(post_vulva_nor_cond);xx=xx+1
surv_fit(post_vulva_nor_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"e",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_vulva_nor_1y);xx=xx+1
polyg_slope(post_vulva_nor_cond);xx=xx+1
polyg_slope(post_vulva_nor_5y)

xx=1
slope_fit(post_vulva_nor_1y);xx=xx+1
slope_fit(post_vulva_nor_cond);xx=xx+1
slope_fit(post_vulva_nor_5y)
axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)








## SWE figures ----------------------------- -----------------
m <- matrix(c(18,1,2, 3, 4, 5
              ,6,8,10,12,14,16
              ,7,9,11,13,15,17), nrow = 3, ncol =6 ,byrow = TRUE)
layout(mat = m,heights = c(0.03,0.6,0.37),
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

range_b<-c(10,100);scal_b<-range_b[2]-range_b[1]
range_c<-c(-0.5,1.5);scal_c<-range_c[2]-range_c[1]

range<-range_b;scal=scal_b
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_b[1],range_b[2]))
text(c(1.3,1.3,1.3),c(range_b[1],50,range_b[2]),
     c(range_b[1],50,range_b[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_b[1]+scal_b*0.5,"Relative survival (%)", cex=1.5,srt=90)

range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))
plot(NULL, axes=FALSE,xlab="",ylab="",xlim=c(-1,1),
     ylim=c(range_c[1],range_c[2]))
text(c(1.3,1.3,1.3),c(range_c[1],0,range_c[2]),
     c(range_c[1],0,range_c[2]),cex=1.4,xpd=TRUE,pos=2)
text(-0.5,range_c[1]+scal_c*0.5,expression(paste(delta, " (survival)")),
     cex=1.5,srt=90)

### breast  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points iswetification
xx=1
breakpo(post_breast_swe_1y,1.2)
xx=xx+1
breakpo(post_breast_swe_cond,-1.1)
xx=xx+1
breakpo(post_breast_swe_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_breast_swe_1y);xx=xx+1
polyg_surv(post_breast_swe_cond);xx=xx+1
polyg_surv(post_breast_swe_5y)

# data points of estimated survival
xx=1
points(breast[breast$shou=="swe_",]$surv_1y~breast[breast$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="swe_",]$surv_cond~breast[breast$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(breast[breast$shou=="swe_",]$surv_5y~breast[breast$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_breast_swe_1y);xx=xx+1
surv_fit(post_breast_swe_cond);xx=xx+1
surv_fit(post_breast_swe_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))

# legend
xpo=0
ypo=0.25
xx=1;yy=range[1]+scal*ypo
rect(1996+xpo,yy+0.035*scal,2017.9+xpo,yy-0.15*scal,col="white",border="grey50",lwd=0.8)
repeat{
  points(2000+xpo,yy,pch=17,col=cola[xx],cex=1.2)
  lines(c(1997.3+xpo,2002.7+xpo),c(yy,yy),lwd=1.6,col=cola[xx],lend=1)
  xx<-xx+1;yy=yy-(scal*0.058);if(xx>3){break}}
xx=1;yy=range[1]+scal*ypo
text(2010+xpo,yy,"1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5/1-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(2010+xpo,yy,"5-year",col=cola[xx],cex=1.25);xx=xx+1;yy=yy-(scal*0.058)
text(1974,range[2]-0.05*scal,"a",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_breast_swe_1y);xx=xx+1
polyg_slope(post_breast_swe_cond);xx=xx+1
polyg_slope(post_breast_swe_5y)

xx=1
slope_fit(post_breast_swe_1y);xx=xx+1
slope_fit(post_breast_swe_cond);xx=xx+1
slope_fit(post_breast_swe_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### corpus  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points iswetification
xx=1
breakpo(post_corpus_swe_1y,-0.9)
xx=xx+1
breakpo(post_corpus_swe_cond,0)
xx=xx+1
breakpo(post_corpus_swe_5y,0.9)

# 95% credible interval for survival
xx=1
polyg_surv(post_corpus_swe_1y);xx=xx+1
polyg_surv(post_corpus_swe_cond);xx=xx+1
polyg_surv(post_corpus_swe_5y)

# data points of estimated survival
xx=1
points(corpus[corpus$shou=="swe_",]$surv_1y~corpus[corpus$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="swe_",]$surv_cond~corpus[corpus$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(corpus[corpus$shou=="swe_",]$surv_5y~corpus[corpus$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_corpus_swe_1y);xx=xx+1
surv_fit(post_corpus_swe_cond);xx=xx+1
surv_fit(post_corpus_swe_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"b",cex=2.2)


ypo=-64
xfl=33

rect(1955+xfl,range[1]+scal*0.965+ypo,1969+xfl,range[1]+scal*0.84+ypo,col="blue",border="grey50")
rect(1955+xfl,range[1]+scal*0.9135+ypo,1969+xfl,range[1]+scal*0.8865+ypo,col="yellow",border=NA)
rect(1960+xfl,range[1]+scal*0.965+ypo,1962+xfl,range[1]+scal*0.84+ypo,col="yellow",border=NA)


##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_corpus_swe_1y);xx=xx+1
polyg_slope(post_corpus_swe_cond);xx=xx+1
polyg_slope(post_corpus_swe_5y)

xx=1
slope_fit(post_corpus_swe_1y);xx=xx+1
slope_fit(post_corpus_swe_cond);xx=xx+1
slope_fit(post_corpus_swe_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### ovary  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points iswetification
xx=1
breakpo(post_ovary_swe_1y,0)
xx=xx+1
breakpo(post_ovary_swe_cond,0)
xx=xx+1
breakpo(post_ovary_swe_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_ovary_swe_1y);xx=xx+1
polyg_surv(post_ovary_swe_cond);xx=xx+1
polyg_surv(post_ovary_swe_5y)

# data points of estimated survival
xx=1
points(ovary[ovary$shou=="swe_",]$surv_1y~ovary[ovary$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="swe_",]$surv_cond~ovary[ovary$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(ovary[ovary$shou=="swe_",]$surv_5y~ovary[ovary$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_ovary_swe_1y);xx=xx+1
surv_fit(post_ovary_swe_cond);xx=xx+1
surv_fit(post_ovary_swe_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"c",cex=2.2)




##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_ovary_swe_1y);xx=xx+1
polyg_slope(post_ovary_swe_cond);xx=xx+1
polyg_slope(post_ovary_swe_5y)

xx=1
slope_fit(post_ovary_swe_1y);xx=xx+1
slope_fit(post_ovary_swe_cond);xx=xx+1
slope_fit(post_ovary_swe_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)


### cervix  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points iswetification
xx=1
breakpo(post_cervix_swe_1y,0)
xx=xx+1
breakpo(post_cervix_swe_cond,0)
xx=xx+1
breakpo(post_cervix_swe_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_cervix_swe_1y);xx=xx+1
polyg_surv(post_cervix_swe_cond);xx=xx+1
polyg_surv(post_cervix_swe_5y)

# data points of estimated survival
xx=1
points(cervix[cervix$shou=="swe_",]$surv_1y~cervix[cervix$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="swe_",]$surv_cond~cervix[cervix$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(cervix[cervix$shou=="swe_",]$surv_5y~cervix[cervix$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_cervix_swe_1y);xx=xx+1
surv_fit(post_cervix_swe_cond);xx=xx+1
surv_fit(post_cervix_swe_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"d",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_cervix_swe_1y);xx=xx+1
polyg_slope(post_cervix_swe_cond);xx=xx+1
polyg_slope(post_cervix_swe_5y)

xx=1
slope_fit(post_cervix_swe_1y);xx=xx+1
slope_fit(post_cervix_swe_cond);xx=xx+1
slope_fit(post_cervix_swe_5y)

axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)

lines(c(1971,2020),c(0,0),col="grey50")
axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)





### vulva  % ------------------------------------------------------
range<-range_b;scal=scal_b
par(mar=c(0,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-10
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+10;if(x>100){break}};lines(c(1971,2020),c(50,50),col="white",lwd=1.7)

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

# breaking points iswetification
xx=1
breakpo(post_vulva_swe_1y,0)
xx=xx+1
breakpo(post_vulva_swe_cond,0)
xx=xx+1
breakpo(post_vulva_swe_5y,0)

# 95% credible interval for survival
xx=1
polyg_surv(post_vulva_swe_1y);xx=xx+1
polyg_surv(post_vulva_swe_cond);xx=xx+1
polyg_surv(post_vulva_swe_5y)

# data points of estimated survival
xx=1
points(vulva[vulva$shou=="swe_",]$surv_1y~vulva[vulva$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="swe_",]$surv_cond~vulva[vulva$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1);xx=xx+1
points(vulva[vulva$shou=="swe_",]$surv_5y~vulva[vulva$shou=="swe_",]$year
       ,pch=17,col=colc[xx],cex=1)

# fitted lines for survival
xx=1
surv_fit(post_vulva_swe_1y);xx=xx+1
surv_fit(post_vulva_swe_cond);xx=xx+1
surv_fit(post_vulva_swe_5y);xx=xx+1

axis(2,las=2,cex.axis=1.4,at=seq(range[1],range[2],by=10),
     labels=c(rep("",length(seq(range[1],range[2],by=10)))),pos=1971)

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],
     labels = c(rep("",5)))
lines(c(1971,2020),c(range[1],range[1]))
text(1974,range[2]-0.05*scal,"e",cex=2.2)

##### slope plot --------------
range<-range_c;scal=scal_c
par(mar=c(2.5,0,0,0))

plot(NULL,xlim=c(1971,2020),ylim=c(range[1],range[2]),xlab="",ylab=""
     ,las=1, axes=FALSE)
rect(1971,range[2],2020,range[1],col="grey90",border=NA)
x<-range[1]
repeat{
  lines(c(1971,2020),c(x,x),col="white",lwd=0.7)
  x=x+0.5;if(x>100){break}}

x<-1980
repeat{
  lines(c(x,x),c(range[1],range[2]),col="white",lwd=0.7)
  x=x+10;if(x>2020){break}}

xx=1
polyg_slope(post_vulva_swe_1y);xx=xx+1
polyg_slope(post_vulva_swe_cond);xx=xx+1
polyg_slope(post_vulva_swe_5y)

xx=1
slope_fit(post_vulva_swe_1y);xx=xx+1
slope_fit(post_vulva_swe_cond);xx=xx+1
slope_fit(post_vulva_swe_5y)
axis(2,las=2,cex.axis=1.4,at=c(seq(range[1],range[2],by=0.5)),
     labels=c(rep("",length(seq(range[1],range[2],by=0.5)))),pos=1971)
lines(c(1971,2020),c(0,0),col="grey50")

axis(side=1,las=1,cex.axis=1.4,at=c(seq(1980,2020,by=10)),pos=range[1],labels=c(rep("",5)))
text(c(seq(1976,2020,by=20)),c(rep(range[1]-0.085*scal,3)),c(seq(1980,2020,by=20)),xpd=TRUE,cex=1.4);
lines(c(1971,2020),c(range[1],range[1]))
title(xlab="Year", line=1.4, cex.lab=1.4,xpd=TRUE)

