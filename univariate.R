setwd("Z:/YWS/boyd/11Jan2016 data")
yw.calibr.hormones = read.csv("yw_mri_01apr2016.csv")  

setwd("Z:/YWS/boyd/11Jan2016 data/homa")
yw.calibr.hormones = read.csv("yw_mri_20apr2016_homa2.csv")  
attach(yw.calibr.hormones)


# Crear new growth pattern
yw.calibr.hormones$grow9_d[yw.calibr.hormones$AntHt9_d==1]<--1
yw.calibr.hormones$grow9_d[yw.calibr.hormones$AntHt9_d==2]<--1
yw.calibr.hormones$grow9_d[yw.calibr.hormones$AntHt9_d==3]<- 0
yw.calibr.hormones$grow9_d[yw.calibr.hormones$AntHt9_d==4]<- 1
yw.calibr.hormones$grow9_d[yw.calibr.hormones$AntHt9_d==5]<- 1

yw.calibr.hormones$grow12_d[yw.calibr.hormones$AntHt12_d==1]<--1
yw.calibr.hormones$grow12_d[yw.calibr.hormones$AntHt12_d==2]<--1
yw.calibr.hormones$grow12_d[yw.calibr.hormones$AntHt12_d==3]<- 0
yw.calibr.hormones$grow12_d[yw.calibr.hormones$AntHt12_d==4]<- 1
yw.calibr.hormones$grow12_d[yw.calibr.hormones$AntHt12_d==5]<- 1

yw.calibr.hormones$grow15_d[yw.calibr.hormones$AntHt15_d==1]<--1
yw.calibr.hormones$grow15_d[yw.calibr.hormones$AntHt15_d==2]<--1
yw.calibr.hormones$grow15_d[yw.calibr.hormones$AntHt15_d==3]<- 0
yw.calibr.hormones$grow15_d[yw.calibr.hormones$AntHt15_d==4]<- 1
yw.calibr.hormones$grow15_d[yw.calibr.hormones$AntHt15_d==5]<- 1

yw.calibr.hormones$moral_ratio=IG1/BP3_new
yw.calibr.hormones$BP3_new=BP3/1000
attach(yw.calibr.hormones)
table(grow12_d)


#table(yw.calibr.hormones$grow15_m)
#check the trend of each covariates
## HORMONES:Glucose hGH Insulin IGFBP_3 IGF_1 SHBG   QUICKI IGF_ratio
plot(Height_d_mes,pwater_d_cal)
plot(Weight_d_mes,pwater_d_cal) #decreasing
plot(AgePerStart_d,pwater_d_cal) #increasing
plot(MRage_d,pwater_d_cal)
plot(AntDBWt_m,pwater_d_cal)
plot(AntDLength_m,pwater_d_cal)
plot(mar_age,pwater_d_cal) Height
plot(AntWtGain_m,pwater_d_cal) #decreasing
lsmult<-lm(pwater~Last_men+Height_d_mes+Weight_d_mes+, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult)

#

plot(pwater_d_cal~Edu_m_level)
plot(pwater_d_cal~q_income)
plot(pwater_d_cal~grow9_m)
plot(pwater_d_cal~grow12_m)
plot(pwater_d_cal~grow15_m)
lsmult<-lm(tfat_d_cal ~darea+Height_d_mes+Weight_d_mes+MRage_d, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult)
table(PreglenCode_m)

kruskal.test(pwater_d_cal ~ as.factor(birthorder), data=yw.calibr.hormones)
kruskal.test(pwater_d_cal ~ as.factor(Edu_m_level), data=yw.calibr.hormones) #different among groups
kruskal.test(pwater_d_cal ~ as.factor(q_income), data=yw.calibr.hormones) #different among groups
kruskal.test(pwater_d_cal ~ as.factor(AntHt9_m), data=yw.calibr.hormones) #different among groups
kruskal.test(pwater_d_cal ~ as.factor(AntHt12_m), data=yw.calibr.hormones) #different among groups
kruskal.test(pwater_d_cal ~ as.factor(AntHt15_m), data=yw.calibr.hormones) 
kruskal.test(pwater_d_cal ~ as.factor(BrFeedMed_m), data=yw.calibr.hormones) 


## QQ plot
lsmult<-lm(pwater_d_cal~IG1+Height_d_mes+Weight_d_mes+MRage_d+AgePerStart_d+Last_men, data=yw.calibr.hormones, na.action=na.exclude)
lsmult_log<-lm(pwater_d_cal~log(IG1)+Height_d_mes+Weight_d_mes+MRage_d+AgePerStart_d+Last_men, data=yw.calibr.hormones, na.action=na.exclude)
lsmult_log10<-lm(pwater_d_cal~log10(IG1)+Height_d_mes+Weight_d_mes+MRage_d+AgePerStart_d+Last_men, data=yw.calibr.hormones, na.action=na.exclude)
lsmult_sqrt<-lm(pwater_d_cal~sqrt(IG1)+Height_d_mes+Weight_d_mes+MRage_d+AgePerStart_d+Last_men, data=yw.calibr.hormones, na.action=na.exclude)

## residual plot(multiple regression(ei vs yi)):
plot(lsmult,1) # flat line aro       und 0 is better! 
plot(lsmult_log,1)
plot(lsmult_log10,1) 
plot(lsmult_sqrt,1)


## Normal QQ plot
plot(lsmult,2) # Same plot with title and theoretically
plot(lsmult_log,2)
plot(lsmult_log10,2) 
plot(lsmult_sqrt,2)



## quintile thing
quintile.Insulin<-quantile(yw.calibr.hormones$Insulin, probs=0:5/5, na.rm=T)
q.Insulin<-as.numeric(factor(cut(yw.calibr.hormones$Insulin, breaks=quintile.Insulin,include.lowest=T )))
table(q.Insulin)
yw.calibr.hormones<-cbind(yw.calibr.hormones,q.Insulin=q.Insulin)

quintile.hGH<-quantile(yw.calibr.hormones$hGH, probs=0:5/5, na.rm=T)
q.hGH<-as.numeric(factor(cut(yw.calibr.hormones$hGH, breaks=quintile.hGH,include.lowest=T )))
table(q.hGH)
yw.calibr.hormones<-cbind(yw.calibr.hormones,q.hGH=q.hGH)

quintile.SHBG<-quantile(yw.calibr.hormones$SHBG, probs=0:5/5, na.rm=T)
q.SHBG<-as.numeric(factor(cut(yw.calibr.hormones$SHBG, breaks=quintile.SHBG,include.lowest=T )))
table(q.SHBG)
yw.calibr.hormones<-cbind(yw.calibr.hormones,q.SHBG=q.SHBG)

quintile.QUICKI<-quantile(yw.calibr.hormones$QUICKI, probs=0:5/5, na.rm=T)
q.QUICKI<-as.numeric(factor(cut(yw.calibr.hormones$QUICKI, breaks=quintile.QUICKI,include.lowest=T )))
table(q.QUICKI)
yw.calibr.hormones<-cbind(yw.calibr.hormones,q.QUICKI=q.QUICKI)


##Regression model
lsmult<-glm(log(hGH)~tarea+Height_d_mes+Weight_d_mes+MRage_d, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult)



  
  +AgePerStart_d+Last_men

## Hormones Explore

plot(Glucose[!is.na(Glucose)],log(twater_d_cal[!is.na(Glucose)]))

lines(loess.smooth(Glucose[!is.na(Glucose)],log(twater_d_cal[!is.na(Glucose)])), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(Glucose[!is.na(Glucose)], log(twater_d_cal[!is.na(Glucose)]))
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of Glucose")
#################################################################

plot(hGH[!is.na(hGH)],twater_d_cal[!is.na(hGH)])
lines(loess.smooth(hGH[!is.na(hGH)],twater_d_cal[!is.na(hGH)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(hGH[!is.na(hGH)], twater_d_cal[!is.na(hGH)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of Growth Hormone")


##exp((-1)/(42*hGH))
plot(exp((-1)/(42*hGH[!is.na(hGH)])),pwater_d_cal[!is.na(hGH)])
lines(loess.smooth(exp((-1)/(42*hGH[!is.na(hGH)])),pwater_d_cal[!is.na(hGH)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(exp((-1)/(42*hGH[!is.na(hGH)])), pwater_d_cal[!is.na(hGH)])
lines(smoothingSpline,col="darkblue")

legend("topleft", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of exp((-1)/(42*Growth Hormone))")

##LOG
plot(hGH[!is.na(hGH)],twater_d_cal[!is.na(hGH)])
lines(loess.smooth(hGH[!is.na(hGH)],twater_d_cal[!is.na(hGH)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(log(hGH[!is.na(hGH)]), log(tfat_d_cal[!is.na(hGH)]))
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of log(Growth Hormone)")

##try
plot(sqrt(hGH[!is.na(hGH)]),pwater_d_cal[!is.na(hGH)])
lines(loess.smooth(sqrt(hGH[!is.na(hGH)]),pwater_d_cal[!is.na(hGH)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(sqrt(hGH[!is.na(hGH)]), pwater_d_cal[!is.na(hGH)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of sqrt(hGH)")

#################################################################

plot(Insulin[!is.na(Insulin)],pwater_d_cal[!is.na(Insulin)])
lines(loess.smooth(Insulin[!is.na(Insulin)],pwater_d_cal[!is.na(Insulin)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(Insulin[!is.na(Insulin)], pwater_d_cal[!is.na(Insulin)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of Insulin")

##LOG
plot(log(Insulin[!is.na(Insulin)]),sqrt(twater_d_cal[!is.na(Insulin)]))
lines(loess.smooth(log(Insulin[!is.na(Insulin)]),sqrt(twater_d_cal[!is.na(Insulin)])), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(log(Insulin[!is.na(Insulin)]), twater_d_cal[!is.na(Insulin)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of log(Insulin)")


##sqrt
plot(sqrt(Insulin[!is.na(Insulin)]),pwater_d_cal[!is.na(Insulin)])
lines(loess.smooth(sqrt(Insulin[!is.na(Insulin)]),pwater_d_cal[!is.na(Insulin)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(sqrt(Insulin[!is.na(Insulin)]), pwater_d_cal[!is.na(Insulin)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of sqrt(Insulin)")

#################################################################

plot(IGF_1[!is.na(IGF_1)],pwater_d_cal[!is.na(IGF_1)])
lines(loess.smooth(IGF_1[!is.na(IGF_1)],pwater_d_cal[!is.na(IGF_1)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(IGF_1[!is.na(IGF_1)], pwater_d_cal[!is.na(IGF_1)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of IGF_1")

#################################################################

plot(IGFBP_3[!is.na(IGFBP_3)],pwater_d_cal[!is.na(IGFBP_3)])
lines(loess.smooth(IGFBP_3[!is.na(IGFBP_3)],pwater_d_cal[!is.na(IGFBP_3)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(IGFBP_3[!is.na(IGFBP_3)], pwater_d_cal[!is.na(IGFBP_3)])
lines(smoothingSpline,col="darkblue")

legend("topleft", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of IGFBP_3")

#################################################################

plot(SHBG[!is.na(SHBG)],pwater_d_cal[!is.na(SHBG)])
lines(loess.smooth(SHBG[!is.na(SHBG)],pwater_d_cal[!is.na(SHBG)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(SHBG[!is.na(SHBG)], pwater_d_cal[!is.na(SHBG)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of SHBG")


##LOG
plot(log(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)])
lines(loess.smooth(log(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(log(SHBG[!is.na(SHBG)]), pwater_d_cal[!is.na(SHBG)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of log(SHBG)")

##sqrt
plot(sqrt(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)])
lines(loess.smooth(sqrt(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(sqrt(SHBG[!is.na(SHBG)]), pwater_d_cal[!is.na(SHBG)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of sqrt(SHBG)")

##sth
plot(1/(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)])
lines(loess.smooth(1/(SHBG[!is.na(SHBG)]),pwater_d_cal[!is.na(SHBG)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(1/(SHBG[!is.na(SHBG)]), pwater_d_cal[!is.na(SHBG)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of 1/SHBG")

#################################################################

plot(QUICKI[!is.na(QUICKI)],pwater_d_cal[!is.na(QUICKI)])
lines(loess.smooth(QUICKI[!is.na(QUICKI)],pwater_d_cal[!is.na(QUICKI)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(QUICKI[!is.na(QUICKI)], pwater_d_cal[!is.na(QUICKI)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of QUICKI")

#################################################################

plot(IGF_ratio[!is.na(IGF_ratio)],pwater_d_cal[!is.na(IGF_ratio)])
lines(loess.smooth(IGF_ratio[!is.na(IGF_ratio)],pwater_d_cal[!is.na(IGF_ratio)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(IGF_ratio[!is.na(IGF_ratio)], pwater_d_cal[!is.na(IGF_ratio)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of IGF_ratio")

##LOG
plot(log(IGF_ratio[!is.na(IGF_ratio)]),pwater_d_cal[!is.na(IGF_ratio)])
lines(loess.smooth(log(IGF_ratio[!is.na(IGF_ratio)]),pwater_d_cal[!is.na(IGF_ratio)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(log(IGF_ratio[!is.na(IGF_ratio)]), pwater_d_cal[!is.na(IGF_ratio)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of log(IGF_ratio)")

##sqrt
plot(sqrt(IGF_ratio[!is.na(IGF_ratio)]),pwater_d_cal[!is.na(IGF_ratio)])
lines(loess.smooth(sqrt(IGF_ratio[!is.na(IGF_ratio)]),pwater_d_cal[!is.na(IGF_ratio)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(sqrt(IGF_ratio[!is.na(IGF_ratio)]), pwater_d_cal[!is.na(IGF_ratio)])
lines(smoothingSpline,col="darkblue")

legend("topright", inset=.05, title="Smooth Line Type",
       c("Loess","Spline"), fill=c("red","darkblue"), horiz=TRUE)
title(main="Scatter plot and smooth lines of sqrt(IGF_ratio)")


#################################################
#Early life pattern
plot(Weight_m[!is.na(Weight_m)],SHBG[!is.na(Weight_m)])
lines(loess.smooth(Weight_m[!is.na(Weight_m)],SHBG[!is.na(Weight_m)]), col="red", lty=1, lwd=1)

smoothingSpline = smooth.spline(mar_age[!is.na(mar_age)], pwater_d_cal[!is.na(mar_age)])
lines(smoothingSpline,col="darkblue")

##Regression model
lsmult<-glm(Insulin ~tarea+Weight_d_mes+Height_d_mes+MRage_d, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult)


#Edu_m_level, med_income_house_m, AntWtGain_m, BrFeedMed_m,PreglenCode_m
# grow12_d

co <- function(v1, ... ){
  C1<-cor(v1[!is.na(v1)&!is.na(APO_A1)],APO_A1[!is.na(APO_A1)&!is.na(v1)])
  C2<-cor(v1[!is.na(v1)&!is.na(APO_B)],APO_B[!is.na(APO_B)&!is.na(v1)])
  C3<-cor(v1[!is.na(v1)&!is.na(CHOL)],CHOL[!is.na(CHOL)&!is.na(v1)])
  C4<-cor(v1[!is.na(v1)&!is.na(HDLC)],HDLC[!is.na(HDLC)&!is.na(v1)],method="spearman")
  C5<-cor(v1[!is.na(v1)&!is.na(TRIG)],TRIG[!is.na(TRIG)&!is.na(v1)],method="spearman")
  C6<-cor(v1[!is.na(v1)&!is.na(calc_LDL)],calc_LDL[!is.na(calc_LDL)&!is.na(v1)])
  CL<-data.frame(C1,C2,C3,C4,C5,C6)
  CL
  }

co(mar_age)
co(birthorder)
co(Weight_m)
co(med_income_house_m)
co(AntWtGain_m)

co(Weight_d_mes)
co(Height_d_mes)

co(pdarea)
co(MRage_d)
co(AgePerStart_d)
co(Last_men)

regg <- function(v1, ...){
  lsmult1<-glm(Insulin~as.factor(v1), data=yw.calibr.hormones, na.action=na.exclude)
  plot(v1,leg_d)
  boxplot(leg_d~v1)
  summary(lsmult1)
}

regg(grow12_d)

kruskal.test(log(Insulin)~ as.factor(grow12_d), data = yw.calibr.hormones) 

#Hip_d, SittingHT_d Triceps_d Subscap_d Suprai_d Waist_d leg_d
#APO_A1  APO_B CHOL  HDLC  TRIG  calc_LDL
## corr: wt ht age vs others
coth <- function(v1, ... ){
  C1<-cor(v1[!is.na(v1)&!is.na(darea)],darea[!is.na(darea)&!is.na(v1)])
  C2<-cor(v1[!is.na(v1)&!is.na(ndarea)],ndarea[!is.na(ndarea)&!is.na(v1)])
  C3<-cor(v1[!is.na(v1)&!is.na(tarea)],tarea[!is.na(tarea)&!is.na(v1)])
  C4<-cor(v1[!is.na(v1)&!is.na(pdarea)],pdarea[!is.na(pdarea)&!is.na(v1)])
  CL<-data.frame(C1,C2,C3,C4)
  CL
}

coth(mar_age)
coth(birthorder)
coth(med_income_house_m)
coth(AntWtGain_m)
coth(APO_B)

coth(AgePerStart_d)
coth(Last_men)

coth(Weight_m)
coth(Last_men)
coth(Hip_d)



lsmult1<-glm(log(tfat_d_cal)~log(Homa)+Weight_d_mes+Height_d_mes, data=homa_data, na.action=na.exclude)
summary(lsmult1) 


lsmeans(lsmult1,"grow9_d")



newd<-data.frame(grow15_d=c(-1,0,1),Height_d_mes=rep(mean(yw.calibr.hormones[,"Height_d_mes"],na.rm=T),3),
                         Weight_d_mes=rep(mean(yw.calibr.hormones[,"Weight_d_mes"],,na.rm=T),3),
                         MRage_d=rep(mean(yw.calibr.hormones[,"MRage_d"],,na.rm=T),3))

lsm.pd<-predict(lm(tfat_d_cal ~ as.factor(grow15_d)+Height_d_mes+Weight_d_mes+MRage_d, data=yw.calibr.hormones, na.action=na.exclude),newdata=newd,ci.fit=T)
lsm.pd

#count length
yw.calibr.hormones$g9_1[grow9_d==-1 & grow12_d==-1]<- -1
yw.calibr.hormones$g9_1[grow9_d==0 & grow12_d==-1]<- 0
yw.calibr.hormones$g9_1[grow9_d==1 & grow12_d==-1]<- 1
table(yw.calibr.hormones$g9_1)

yw.calibr.hormones$g9_2[grow9_d==-1 & grow12_d==0]<- -1
yw.calibr.hormones$g9_2[grow9_d==0 & grow12_d==0]<- 0
yw.calibr.hormones$g9_2[grow9_d==1 & grow12_d==0]<- 1
table(yw.calibr.hormones$g9_2)

yw.calibr.hormones$g9_3[grow9_d==-1 & grow12_d==1]<- -1
yw.calibr.hormones$g9_3[grow9_d==0 & grow12_d==1]<- 0
yw.calibr.hormones$g9_3[grow9_d==1 & grow12_d==1]<- 1
table(yw.calibr.hormones$g9_3)


yw.calibr.hormones$g12_1[grow12_d==-1 & grow15_d==-1]<- -1
yw.calibr.hormones$g12_1[grow12_d==0 & grow15_d==-1]<- 0
yw.calibr.hormones$g12_1[grow12_d==1 & grow15_d==-1]<- 1
table(yw.calibr.hormones$g12_1)

yw.calibr.hormones$g12_2[grow12_d==-1 & grow15_d==0]<- -1
yw.calibr.hormones$g12_2[grow12_d==0 & grow15_d==0]<- 0
yw.calibr.hormones$g12_2[grow12_d==1 & grow15_d==0]<- 1
table(yw.calibr.hormones$g12_2)

yw.calibr.hormones$g12_3[grow12_d==-1 & grow15_d==1]<- -1
yw.calibr.hormones$g12_3[grow12_d==0 & grow15_d==1]<- 0
yw.calibr.hormones$g12_3[grow12_d==1 & grow15_d==1]<- 1
table(yw.calibr.hormones$g12_3)

## HOMA VS QUICKI
plot(QUICKI,log(HOMA1_B))
title(main="Scatter plot of Insulin Sensitivity Score and log(HOMA1_B)")

plot(QUICKI,log(HOMA1_IR))
title(main="Scatter plot of Insulin Sensitivity Score and log(HOMA1_IR)")

homa_data<-data.frame(yw.calibr.hormones[!is.na(Glucose) & Glucose>=70 & Glucose<=99 & !is.na(Insulin) & Insulin<25,])
attach(homa_data)
homa_data$FPI <-homa_data$Insulin
homa_data$FPG <-homa_data$Glucose/18
attach(homa_data)

homa_data$HOMA1_IR<-(FPI*FPG)/22.5
homa_data$HOMA1_B<-(20*FPI)/(FPG-3.5)
attach(homa_data)

lsmult1<-glm(log(tfat_d_cal)~log(HOMA1_B), data=homa_data, na.action=na.exclude)
summary(lsmult1) 

lsmult1<-lm(pwater_d_cal~Insulin-1, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult1) 


##Homa2
setwd("Z:/YWS/boyd/11Jan2016 data/homa")
homa_data = read.csv("yw_mri_20apr2016_homa2.csv")  
attach(homa_data)

## body fat
attach(yw.calibr.hormones)
sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yw.calibr.hormones$log10_skin<-log10_skin
yw.calibr.hormones$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
yw.calibr.hormones$BMI<-Weight_d_mes/((Height_d_mes/100)^2)
attach(yw.calibr.hormones)

lsmult1<-lm(pwater_d_cal~as.factor(grow9_d)+MRage_d, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult1)

library(aod)
wald.test(b = coef(lsmult1), Sigma = vcov(lsmult1), Terms = 1:3)

##prediction of rank's effect, on holding the mean of gre and gpa
newdata1 <- with(yw.calibr.hormones,data.frame( MRage_d = mean(MRage_d,na.rm=T), grow9_d = factor(c(-1,0,1))))

newdata1$rankP <- predict(lsmult1, newdata = newdata1, type = "response")
newdata1

#multinomial logistic regression:
library(nnet)

yw.calibr.hormones$grow15_d<-relevel(as.factor(yw.calibr.hormones$grow15_d), ref = "-1")
test <- multinom(as.factor(grow15_d) ~ log(HOMA2__B), data = yw.calibr.hormones)
summary(test)

#calculate p value
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#show real relative risk
exp(coef(test))


##prediction
newd<-data.frame(grow12_d=c(-1,0,1),
                      Weight_d_mes=rep(mean(yw.calibr.hormones[,"Weight_d_mes"],na.rm=T),3),
                      Height_d_mes=rep(mean(yw.calibr.hormones[,"Height_d_mes"],na.rm=T),3),
                      MRage_d=rep(mean(yw.calibr.hormones[,"MRage_d"],na.rm=T),3))

lsm.pd.birthwt.ad<-predict(lm(log(tfat_d_cal)~as.factor(grow12_d)+Weight_d_mes+Height_d_mes+MRage_d, data=yw.calibr.hormones,na.action=na.exclude),newdata=newd,interval="confidence") 

##ggplot2
library(ggplot2)
log_Insulin<-log(yw.calibr.hormones$Insulin)
log_SHBG<-log(yw.calibr.hormones$SHBG)
log_hGH<-log(yw.calibr.hormones$hGH)

log_HOMAB<-log(yw.calibr.hormones$HOMA2..B)
log_HOMAS<-log(yw.calibr.hormones$HOMA2..S)
log_HOMAIR<-log(yw.calibr.hormones$HOMA2.IR)

qplot(log_HOMAB, geom="histogram") 
qplot(log_HOMAS, geom="histogram") 
qplot(log_HOMAIR, geom="histogram")

##
yw.calibr.hormones$EntryDate_d<-as.Date(EntryDate_d, format="%d-%b-%y") #convert date, if month is show in Letter(Jan), use %b rather than %m

phone<-data.frame(yw.calibr.hormones[yw.calibr.hormones$EntryDate_d < "2010-09-01",])
phtwo<-data.frame(yw.calibr.hormones[yw.calibr.hormones$EntryDate_d >= "2010-09-01",])

lsmult1<-lm(log(tfat_d_cal)~log(HOMA2..B), data=phtwo, na.action=na.exclude)
summary(lsmult1)

##height at 9
lsmult1<-lm(log(twater_d_cal)~as.factor(grow15_d)+MRage_d, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult1)

library(aod)
wald.test(b = coef(lsmult1), Sigma = vcov(lsmult1), Terms = 1:3)

newdata1 <- with(yw.calibr.hormones,data.frame( MRage_d = mean(MRage_d,na.rm=T), Weight_d_mes = mean(Weight_d_mes,na.rm=T), Height_d_mes = mean(Height_d_mes,na.rm=T),grow15_d = factor(c(-1,0,1))))

newdata1$rankP <- predict(lsmult1, newdata = newdata1, type = "response")
newdata1$rankP<-exp(newdata1$rankP)
newdata1


################
attach(yw.calibr.hormones)
lsmult1<-lm(pwater_d_cal~log(Insulin)+Height_d_mes+Weight_d_mes+MRage_d+pdarea, data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult1)

#######
yw.calibr.hormones$agetrend[grow9_d<grow12_d]<--1
yw.calibr.hormones$agetrend[grow9_d==grow12_d]<-0
yw.calibr.hormones$agetrend[grow9_d>grow12_d]<-1
table(yw.calibr.hormones$agetrend)

attach(yw.calibr.hormones)
lsmult1<-lm(pwater_d_cal~as.factor(agetrend), data=yw.calibr.hormones, na.action=na.exclude)
summary(lsmult1)

newdata1 <- with(yw.calibr.hormones,data.frame( MRage_d = mean(MRage_d,na.rm=T), Weight_d_mes = mean(Weight_d_mes,na.rm=T), Height_d_mes = mean(Height_d_mes,na.rm=T),agetrend = factor(c(-1,0,1))))
newdata1$rankP <- predict(lsmult1, newdata = newdata1, type = "response")
newdata1$rankP<-exp(newdata1$rankP)
newdata1


########################
######2016/09/06########
########################
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data/homa")
homa_data = read.csv("yw_mri_20apr2016_homa2.csv")  

setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data")
homa_data = read.csv("yw_mri_01apr2016.csv") 
attach(homa_data)


lsmult1<-lm(pwater_d_cal~Height_d_mes+Weight_d_mes+MRage_d+log(HOMA2.IR), data=homa_data, na.action=na.exclude)
summary(lsmult1)

#lsmult1<-lm(log(tfat_d_cal)~Weight_d_mes+Height_d_mes+MRage_d, data=homa_data, na.action=na.exclude)
#summary(lsmult1)

##
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data")
phase1_data = read.csv("yw_mri_12sep2016_ph1.csv") 
attach(phase1_data)

lsmult1<-lm(log(tfat_d_cal)~Height_d_mes+Weight_d_mes+MRage_d+log(SHBG), data=phase1_data, na.action=na.exclude)
summary(lsmult1)


##
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data")
phase2_data = read.csv("yw_mri_12sep2016_ph2.csv") 
attach(phase2_data)

lsmult1<-lm(log(tfat_d_cal)~Height_d_mes+Weight_d_mes+MRage_d+Glucose, data=phase2_data, na.action=na.exclude)
summary(lsmult1)


###
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data/homa")
phase1_homa_data = read.csv("yw_mri_12sep2016_ph1.csv") 
attach(phase1_homa_data)

lsmult1<-lm(pwater_d_cal~Height_d_mes+Weight_d_mes+MRage_d+log(HOMA2.IR), data=phase1_homa_data, na.action=na.exclude)
summary(lsmult1)



##
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data/homa")
phase2_homa_data = read.csv("yw_mri_12sep2016_ph2.csv") 
attach(phase2_homa_data)

lsmult1<-lm(log(tfat_d_cal)~Height_d_mes+Weight_d_mes+MRage_d+log(HOMA2..S), data=phase2_homa_data, na.action=na.exclude)
summary(lsmult1)


##2016/10/6
setwd("E:/Jie/YWS/YWS/boyd/11Jan2016 data/homa")
yws_ho = read.csv("yw_mri_03may2016_homa2.csv") 
yws = read.csv("yw_mri_01apr2016.csv") 
attach(yws)

yws_ph1=read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2=read.csv("yw_mri_12sep2016_ph2.csv") 

yws_ph1_ho=read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2_ho=read.csv("yw_mri_12sep2016_ph2.csv") 

mean(yws_ph2_ho$HOMA2.IR,na.rm=T)
sd(yws_ph2_ho$HOMA2.IR,na.rm=T)

hist(yws_ph2_ho$HOMA2.IR)


eruption.lm = lm(pwater_d_cal ~ (HOMA2..S), data=yws_ph2_ho) 
eruption.stdres = rstandard(eruption.lm)

qqnorm(eruption.stdres,main="Ph2 HOMA2..S") 
qqline(eruption.stdres)


##2016/10/14
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws_ph1 = read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2 = read.csv("yw_mri_12sep2016_ph2.csv") 
yws=read.csv("yw_mri_01apr2016.csv") 
  
yws_in25_ph1=yws_ph1[yws_ph1$Insulin<=25,]
yws_in25_ph2=yws_ph2[yws_ph2$Insulin<=25,]
yws_in25=yws[yws$Insulin<=25,]

yws_in25$EntryDate_d<-as.Date(yws_in25$EntryDate_d, format="%d-%b-%y") #convert date, if month is show in Letter(Jan), use %b rather than %m
yws_in25$phase=ifelse(yws_in25$EntryDate_d < "2010-09-01",0,1)
table(yws_in25$phase)

lsmult1<-lm(pwater_d_cal~Height_d_mes+Weight_d_mes+MRage_d+phase+Insulin+Insulin*phase, data=yws_in25, na.action=na.exclude)
summary(lsmult1)

##2016/10/25
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws_ph1 = read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2 = read.csv("yw_mri_12sep2016_ph2.csv") 
yws=read.csv("yw_mri_01apr2016.csv") 

yws_in25_ph1=subset(yws_ph1, (yws_ph1$Insulin<=25|is.na(yws_ph1$Insulin)) & (yws_ph1$Glucose<=99 | is.na(yws_ph1$Glucose)))
yws_in25_ph2=subset(yws_ph2, (yws_ph2$Insulin<=25|is.na(yws_ph2$Insulin)) & (yws_ph2$Glucose<=99 | is.na(yws_ph2$Glucose)))
yws_in25=subset(yws, (yws$Insulin<=25|is.na(yws$Insulin)) & (yws$Glucose<=99 | is.na(yws$Glucose)))

yws_in25$EntryDate_d<-as.Date(yws_in25$EntryDate_d, format="%d-%b-%y") #convert date, if month is show in Letter(Jan), use %b rather than %m
yws_in25$phase=ifelse(yws_in25$EntryDate_d < "2010-09-01",0,1)
table(yws_in25$phase)
summary(yws_in25$phase)

attach(yws_in25)
#AntDLength_m is body length at birth, AntDBWt_m is weight at birth
lsmult1<-lm(log(tfat_d_cal)~Height_d_mes+Weight_d_mes+MRage_d+Glucose, data=yws_in25_ph1, na.action=na.exclude)
summary(lsmult1)



mean(yws_in25_ph2$IGF_1,na.rm=T)
sd(yws_in25_ph2$IGF_1,na.rm=T)

mean(yws_in25_ph2$IGFBP_3,na.rm=T)
sd(yws_in25_ph2$IGFBP_3,na.rm=T)

mean(yws_in25_ph2$Insulin,na.rm=T)
sd(yws_in25_ph2$Insulin,na.rm=T)

mean(yws_in25_ph2$Glucose,na.rm=T)
sd(yws_in25_ph2$Glucose,na.rm=T)

mean(yws_in25_ph2$SHBG,na.rm=T)
sd(yws_in25_ph2$SHBG,na.rm=T)

mean(yws_in25_ph2$hGH,na.rm=T)
sd(yws_in25_ph2$hGH,na.rm=T)

mean(yws_in25_ph2$pwater_d_cal,na.rm=T)
sd(yws_in25_ph2$pwater_d_cal,na.rm=T)

mean(yws_in25_ph2$twater_d_cal,na.rm=T)
sd(yws_in25_ph2$twater_d_cal,na.rm=T)

mean(yws_in25_ph2$tfat_d_cal,na.rm=T)
sd(yws_in25_ph2$tfat_d_cal,na.rm=T)

summary(yws_in25_ph2$SHBG)


##2016/10/28
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data/homa")
yws_ho = read.csv("yw_mri_03may2016_homa2.csv") 
yws_ho_in25=subset(yws_ho, (yws_ho$Insulin<=25|is.na(yws_ho$Insulin)) & (yws_ho$Glucose<=99 | is.na(yws_ho$Glucose)))


##2016/11/2
attach(yws_in25_ph2)

lsmult1<-lm(log(tfat_d_cal)~Weight_d_mes+Height_d_mes+MRage_d+log(HOMA2__S), data=yws_ho_in25_ph2, na.action=na.exclude)
summary(lsmult1)

##2016/11/3
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data/homa")
yws_ho_ph1 = read.csv("yw_mri_20apr2016_ph1.csv") 
yws_ho_ph2 = read.csv("yw_mri_20apr2016_ph2.csv") 

yws_ho_in25_ph1=subset(yws_ho_ph1, (yws_ho_ph1$Insulin<=25|is.na(yws_ho_ph1$Insulin)) & (yws_ho_ph1$Glucose<=99 | is.na(yws_ho_ph1$Glucose)))
yws_ho_in25_ph2=subset(yws_ho_ph2, (yws_ho_ph2$Insulin<=25|is.na(yws_ho_ph2$Insulin)) & (yws_ho_ph2$Glucose<=99 | is.na(yws_ho_ph2$Glucose)))

lsmult1<-lm(log(tfat_d_cal)~MRage_d+Weight_d_mes+Height_d_mes+log(HOMA2__S), data=yws_ho_in25_ph2, na.action=na.exclude)
summary(lsmult1)

####2016/11/15
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data/homa")
yws_ho = read.csv("yw_mri_03may2016_homa2.csv") 
yws_ho_in25=subset(yws_ho, (yws_ho$Insulin<=25|is.na(yws_ho$Insulin)) & (yws_ho$Glucose<=99 | is.na(yws_ho$Glucose)))

yws_ho_in25$wh_ratio=yws_ho_in25$Waist_d/yws_ho_in25$Hip_d

attach(yws_ho_in25)
sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws_ho_in25$log10_skin<-log10_skin
yws_ho_in25$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342


lsmult1<-lm(log(tfat_d_cal)~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+DW_bodyfat+log(HOMA2__S), data=yws_ho_in25, na.action=na.exclude)
summary(lsmult1)

attach(yws_ho_in25)
d<-data.frame(Weight_d_mes,Height_d_mes,wh_ratio,DW_bodyfat,Insulin,HOMA2__S,SHBG,hGH)
cor(d)

####2016/11/21
attach(yws_ho_in25)
d<-data.frame(log(Triceps_d),log(Subscap_d),log(Suprai_d),pwater_d_cal,twater_d_cal,tfat_d_cal)
cor(d)

####2016/11/28
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws_ph1 = read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2 = read.csv("yw_mri_12sep2016_ph2.csv") 
yws=read.csv("yw_mri_01apr2016.csv") 

yws_in25_ph1=subset(yws_ph1, (yws_ph1$Insulin<=25|is.na(yws_ph1$Insulin)) & (yws_ph1$Glucose<=99 | is.na(yws_ph1$Glucose)))
yws_in25_ph2=subset(yws_ph2, (yws_ph2$Insulin<=25|is.na(yws_ph2$Insulin)) & (yws_ph2$Glucose<=99 | is.na(yws_ph2$Glucose)))
yws_in25=subset(yws, (yws$Insulin<=25|is.na(yws$Insulin)) & (yws$Glucose<=99 | is.na(yws$Glucose)))

##Weight gained:AntWtGain_m, median income:med_income_house_m, Education_m, Age at menarche: AgePerStart_m, Weight_m
##Median in each Breast Feeding Duration:BrFeedMed_m, Height_m,  Mother's age at mammogram: Mammage_m

mean(yws_in25$AntWtGain_m,na.rm=T)
sd(yws_in25$AntWtGain_m,na.rm=T)

mean(yws_in25$Weight_m,na.rm=T)
sd(yws_in25$Weight_m,na.rm=T)

mean(yws_in25$Height_m,na.rm=T)
sd(yws_in25$Height_m,na.rm=T)

mean(yws_in25$med_income_house_m,na.rm=T)
sd(yws_in25$med_income_house_m,na.rm=T)

mean(yws_in25$AgePerStart_m,na.rm=T)
sd(yws_in25$AgePerStart_m,na.rm=T)

mean(yws_in25$Mammage_m,na.rm=T)
sd(yws_in25$Mammage_m,na.rm=T)



mean(yws_in25_ph1$pdarea,na.rm=T)
sd(yws_in25_ph1$pdarea,na.rm=T)

mean(yws_in25_ph1$darea,na.rm=T)
sd(yws_in25_ph1$darea,na.rm=T)

mean(yws_in25_ph1$ndarea,na.rm=T)
sd(yws_in25_ph1$ndarea,na.rm=T)

table(yws_in25_ph1$Edu_m_level)
summary(yws_in25_ph1$Edu_m_level)

table(yws_in25$BrFeedCode_m)
summary(yws_in25$BrFeedCode_m)


lsmult1<-lm(Weight_d~darea, data=yws_in25, na.action=na.exclude)
summary(lsmult1)


quintile.income<-quantile(yws_in25$med_income_house_m, probs=0:5/5, na.rm=T)
q.income<-as.numeric(factor(cut(yws_in25$med_income_house_m, breaks=quintile.income,include.lowest=T )))
table(q.income)
yws_in25<-cbind(yws_in25,q.income=q.income)

lsmult1<-lm(pwater_d_cal~MRage_d+as.factor(q.income), data=yws_in25, na.action=na.exclude)
summary(lsmult1)

results<-aov(Height_d~as.factor(q.income), data=yws_in25)
summary(results)


newd<-data.frame(q.income=c(1,2,3,4,5))
lsm.pd.birthwt.ad<-predict(lm(Height_d~as.factor(q.income), data=yws_in25, na.action=na.exclude),newdata=newd,interval="confidence")

###########
dict = list('1' = 'CAT1', '2' = 'CAT1', '3' = 'CAT1', '4'='CAT1','7'='CAT1','5'='CAT2','6'='CAT2')

Education_m_re = as.character(yws_in25$Education_m)
for (i in 1:7){Education_m_re <- replace(Education_m_re, Education_m_re == names(dict[i]), dict[i])}
###########
Education_m_re=factor(yws_in25$Education_m)
levels(Education_m_re)<-c("1","1","1","1","2","3","1")
yws_in25<-cbind(yws_in25,Education_m_re=Education_m_re)


results<-aov(Height_d~as.factor(Education_m_re), data=yws_in25)
summary(results)

newd<-data.frame(Education_m_re=c(1,2,3))
lsm.pd.birthwt.ad<-predict(lm(Height_d~as.factor(Education_m_re), data=yws_in25, na.action=na.exclude),newdata=newd,interval="confidence")


results<-aov(Height_d~as.factor(BrFeedCode_m), data=yws_in25)
summary(results)

newd<-data.frame(BrFeedCode_m=c(0,1,2,3,4,5))
lsm.pd.birthwt.ad<-predict(lm(Height_d~as.factor(BrFeedCode_m), data=yws_in25, na.action=na.exclude),newdata=newd,interval="confidence")


####2016/12/6
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data/homa")
yws_ho = read.csv("yw_mri_03may2016_homa2.csv") 
yws_ho_in25=subset(yws_ho, (yws_ho$Insulin<=25|is.na(yws_ho$Insulin)) & (yws_ho$Glucose<=99 | is.na(yws_ho$Glucose)))

lsmult1<-lm(log(tfat_d_cal)~log(HOMA2_IR)+Weight_d+Height_d+MRage_d, data=yws_ho, na.action=na.exclude)
summary(lsmult1)


####2016/12/20
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws_ph1 = read.csv("yw_mri_12sep2016_ph1.csv") 
yws_ph2 = read.csv("yw_mri_12sep2016_ph2.csv") 
yws=read.csv("yw_mri_01apr2016.csv") 

##Weight gained:AntWtGain_m, median income:med_income_house_m, Education_m, Age at menarche: AgePerStart_m, Weight_m
##Median in each Breast Feeding Duration:BrFeedMed_m, Height_m,  Mother's age at mammogram: Mammage_m

mean(yws_ph2$AgePerStart_m,na.rm=T)
sd(yws_ph2$AgePerStart_m,na.rm=T)


mean(yws_ph2$Mammage_m,na.rm=T)
sd(yws_ph2$Mammage_m,na.rm=T)

mean(yws_ph1$AntWtGain_m,na.rm=T)
sd(yws_ph1$AntWtGain_m,na.rm=T)

mean(yws_ph2$Weight_m,na.rm=T)
sd(yws_ph2$Weight_m,na.rm=T)

mean(yws_ph1$Height_m,na.rm=T)
sd(yws_ph1$Height_m,na.rm=T)

yws$med_income_house_m<-yws$med_income_house_m/10000
yws_ph1$med_income_house_m<-yws_ph1$med_income_house_m/10000
yws_ph2$med_income_house_m<-yws_ph2$med_income_house_m/10000

mean(yws_ph2$med_income_house_m,na.rm=T)
sd(yws_ph2$med_income_house_m,na.rm=T)


table(yws_ph2$Edu_m_level)
summary(yws_ph2$Edu_m_level)

table(yws_ph1$BrFeedCode_m)
summary(yws_ph1$BrFeedCode_m)

attach(yws)

lsmult1<-lm(Weight_d~pdarea, data=yws, na.action=na.exclude)
summary(lsmult1)

results<-aov(Height_d~as.factor(BrFeedCode_m), data=yws)
summary(results)

newd<-data.frame(BrFeedCode_m=c(0,1,2,3,4,5))
lsm.pd.birthwt.ad<-predict(lm(Weight_d~as.factor(Edu_m_level), data=yws, na.action=na.exclude),newdata=newd,interval="confidence")



results<-aov(Height_d~as.factor(Edu_m_level), data=yws)
summary(results)

newd<-data.frame(Edu_m_level=c(1,2,3))
lsm.pd.birthwt.ad<-predict(lm(Height_d~as.factor(Edu_m_level), data=yws, na.action=na.exclude),newdata=newd,interval="confidence")


##2017/01/24 TEST Table3 result
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
attach(yws)
yws_in25=subset(yws, (yws$Insulin<=25|is.na(yws$Insulin)) & (yws$Glucose<=99 | is.na(yws$Glucose)) & (!is.na(yws$HOMA2__S)))
attach(yws_in25)

lsmult1<-lm(log(tfat_d_cal)~QUICKI+Weight_d_mes+Height_d_mes+MRage_d, data=yws, na.action=na.exclude)
summary(lsmult1)

##2017/01/24 Analysis: pwater=mother's variable
##Weight gained:AntWtGain_m, median income:med_income_house_m, Education_m, Age at menarche: AgePerStart_m, Weight_m
##Median in each Breast Feeding Duration:BrFeedMed_m, Height_m,  Mother's age at mammogram: Mammage_m
yws$med_income_house_m<-yws$med_income_house_m/10000
attach(yws)

lsmult1<-lm(pwater_d_cal~AntWtGain_m+med_income_house_m+as.factor(Edu_m_level)+Weight_m+ndarea, data=yws, na.action=na.exclude)
summary(lsmult1)

lsmult1<-lm(twater_d_cal~Weight_m+Height_m+ndarea, data=yws, na.action=na.exclude)
summary(lsmult1)

lsmult1<-lm(tfat_d_cal~med_income_house_m+as.factor(Edu_m_level)+Weight_m+Height_m+ndarea, data=yws, na.action=na.exclude)
summary(lsmult1)


###2017/01/26
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
yws$med_income_house_m<-yws$med_income_house_m/10000

attach(yws)

d<-data.frame(AntWtGain_m,med_income_house_m,Weight_m,Height_m,pdarea,darea,ndarea)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

d_new<-completeFun(d, c("AntWtGain_m","med_income_house_m","Weight_m","Height_m","Height_m","pdarea","darea","ndarea"))
cor(d_new,dec=2)

results<-aov(ndarea~as.factor(Edu_m_level), data=yws)
summary(results)

newd<-data.frame(Edu_m_level=c(1,2,3))
lsm.pd.birthwt.ad<-predict(lm(ndarea~as.factor(Edu_m_level), data=yws, na.action=na.exclude),newdata=newd,interval="confidence")


yws$wh_ratio=yws$Waist_d/yws$Hip_d
attach(yws)
sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)

dau<-data.frame(Weight_d,Height_d,wh_ratio,DW_bodyfat,pwater_d_cal,twater_d_cal,tfat_d_cal)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

dau_new<-completeFun(dau, c("Weight_d","Height_d","wh_ratio","DW_bodyfat","pwater_d_cal","twater_d_cal","tfat_d_cal"))
cor(dau_new)


###2017/02/07
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
yws$med_income_house_m<-yws$med_income_house_m/10000
attach(yws)

lsmult1<-lm(tfat_d_cal~pdarea+Height_m, data=yws, na.action=na.exclude)
summary(lsmult1)


###2017/03/14
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 

yws$wh_ratio=yws$Waist_d/yws$Hip_d

sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)

lsmult1<-lm(tfat_d_cal~MRage_d+Weight_d_mes+leg_d+wh_ratio+DW_bodyfat, data=yws, na.action=na.exclude)
summary(lsmult1)

####2017/04/05
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_442017.csv") 
attach(yws)

yws$EntryDate_d<-as.Date(EntryDate_d, format="%d-%b-%y")
yws$phase<-ifelse(yws$EntryDate_d < "2010-09-01",0,1)
table(yws$phase)


setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_phase_442017.csv") 
attach(yws)

cor(yws$leg_d,yws$phase)
t.test(yws$leg_d~yws$phase)



####4/12/2017
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_phase_442017.csv") 
attach(yws)

yws$wh_ratio=yws$Waist_d/yws$Hip_d

sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)


lsmult1<-lm(tfat_d_cal~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+DW_bodyfat, data=yws, na.action=na.exclude)
summary(lsmult1)


####5/11/2017## leg length 2
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_phase_442017.csv") 
attach(yws)

measure=read.csv("DBodyMeasure_leg length.csv")
measure$StudyID_new=measure$StudyID
measure$StudyID<-gsub("YD", "", measure$StudyID)
measure$StudyID<-strtoi(measure$StudyID)

new<-data.frame(measure$StudyID,measure$DNum,measure$leg1floor,measure$leg2floor)
colnames(new) <- c("StudyID","DNum","leg1floor","leg2floor")


total <- merge(x = yws, y = new, by = c("StudyID","DNum"), all.x = TRUE)
total<-total[!duplicated(total), ]  
write.csv(total,"C:/Users/jodie.zhu/Desktop/total.csv")


#####5/12
yws=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(yws)

yws$legfloor_avg<-(leg1floor+leg2floor)/2
attach(yws)

lsm<-lm(tfat_d_cal~legfloor_avg+Weight_d+MRage_d, data=yws, na.action=na.exclude)
summary(lsm)

cor(legfloor_avg,leg_d,na.rm=TRUE)

cor(legfloor_avg,leg_d, use="complete.obs")
cor(legfloor_avg,Height_d_mes,use="complete.obs")


######5/24/2017
yws$wh_ratio=yws$Waist_d/yws$Hip_d

sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)


yws_leg<-data.frame(yws[!is.na(legfloor_avg),])
attach(yws_leg)

lsm<-lm(tfat_d_cal~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+DW_bodyfat+Insulin, data=yws_leg, na.action=na.exclude)
summary(lsm)


cor(legfloor_avg,DW_bodyfat, use="complete.obs")


##########################
######9/18/2017
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(yws)

yws$wh_ratio=yws$Waist_d/yws$Hip_d

sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)

yws$EntryDate_d<-as.character.Date(yws$EntryDate_d)
#yws$EntryDate_d<-as.Date(EntryDate_d, format="%d-%b-%y") #convert date, if month is show in Letter(Jan), use %b rather than %m

phone<-data.frame(yws[yws$EntryDate_d < "2010-09-01",])
phtwo<-data.frame(yws[yws$EntryDate_d >= "2010-09-01",])

###################
##calculate mean###
mean(phone[,"Height_d_mes"],na.rm=T)
sd(phone$Height_d_mes,na.rm=T)

mean(phone[,"Weight_d_mes"],na.rm=T)
sd(phone$Weight_d_mes,na.rm=T)

mean(phone[,"MRage_d"],na.rm=T)
sd(phone$MRage_d,na.rm=T)

mean(phone[,"AgePerStart_d"],na.rm=T)
sd(phone$AgePerStart_d,na.rm=T)

mean(phone[,"wh_ratio"],na.rm=T)
sd(phone$wh_ratio,na.rm=T)

mean(phone[,"DW_bodyfat"],na.rm=T)
sd(phone$DW_bodyfat,na.rm=T)

#
attach(phone) 
summary(phone$IGF_1)
mean(phone[,"IGF_1"],na.rm=T)
sd(phone$IGF_1,na.rm=T)


summary(phone$IGFBP_3)
mean(phone[,"IGFBP_3"],na.rm=T)
sd(phone$IGFBP_3,na.rm=T)



summary(phone$Insulin)
mean(phone[,"Insulin"],na.rm=T)
sd(phone$Insulin,na.rm=T)


summary(phone$Glucose)
mean(phone[,"Glucose"],na.rm=T)
sd(phone$Glucose,na.rm=T)


summary(phone$SHBG)
mean(phone[,"SHBG"],na.rm=T)
sd(phone$SHBG,na.rm=T)


summary(phone$hGH)
mean(phone[,"hGH"],na.rm=T)
sd(phone$hGH,na.rm=T)

##
summary(phone$pwater_d_cal)
mean(phone[,"pwater_d_cal"],na.rm=T)
sd(phone$pwater_d_cal,na.rm=T)


summary(phone$twater_d_cal)
mean(phone[,"twater_d_cal"],na.rm=T)
sd(phone$twater_d_cal,na.rm=T)


summary(phone$tfat_d_cal)
mean(phone[,"tfat_d_cal"],na.rm=T)
sd(phone$tfat_d_cal,na.rm=T)


lsm<-lm(log(tfat_d_cal)~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+DW_bodyfat+log(HOMA2__S), data=yws, na.action=na.exclude)
summary(lsm)

####9/26/2017 ###check distribution of whaist hip ratio and %body fat
##wh_ratio
myhist <- hist(yws$wh_ratio)
multiplier <- myhist$counts / myhist$density
mydensity <- density(yws$wh_ratio)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)

##%body fat
myhist <- hist(yws$DW_bodyfat)
multiplier <- myhist$counts / myhist$density
mydensity <- density(yws$DW_bodyfat)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)


## QQ plot
lsmult<-lm(pwater_d_cal~wh_ratio, data=yws, na.action=na.exclude)
lsmult_log<-lm(pwater_d_cal~log(wh_ratio), data=yws, na.action=na.exclude)

## residual plot(multiple regression(ei vs yi)):
plot(lsmult,1) # flat line around 0 is better! 
plot(lsmult_log,1)

#Edu_m_level, med_income_house_m, AntWtGain_m, BrFeedMed_m,PreglenCode_m
co <- function(v1,  ... ){
  C1<-cor(v1[!is.na(v1)],med_income_house_m[!is.na(med_income_house_m)])
  C2<-cor(v1[!is.na(v1)],AntWtGain_m[!is.na(AntWtGain_m)])
  C3<-cor(v1[!is.na(v1)],Weight_m[!is.na(Weight_m)])
  C4<-cor(v1[!is.na(v1)],Height_m[!is.na(Height_m)])
  
  CL<-data.frame(C1,C2,C3,C4)
  CL
}

d<-data.frame(med_income_house_m,AntWtGain_m,Weight_m,Height_m)
cor(d, use="complete.obs")


lsmult<-lm(Height_m~as.factor(Edu_m_level), data=yws, na.action=na.exclude)


results<-aov(Height_m~as.factor(Edu_m_level), data=yws)
summary(results)

#MODEL TRY:
yws$income_in_k=yws$med_income_house_m/1000
attach(yws)

lsmult<-lm(log(tfat_d_cal)~as.factor(Edu_m_level), data=yws, na.action=na.exclude)
summary(lsmult)



######################################
#############10/13/2017
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(yws)

yws$wh_ratio=yws$Waist_d/yws$Hip_d

sum_skin = Triceps_d + Subscap_d + Suprai_d
log10_skin = log10(sum_skin) 
yws$log10_skin<-log10_skin
yws$DW_bodyfat =22.3366*(log10_skin)+0.0533*(MRage_d)+0.18405*(Weight_d_mes)-0.1625*(Height_d_mes)+0.156*(Waist_d)-11.13335342
attach(yws)

lsmult<-lm(log(tfat_d_cal)~Weight_d_mes+Height_d_mes+MRage_d+wh_ratio+DW_bodyfat+
             IGF_1, data=yws, na.action=na.exclude)
summary(lsmult)


lsmult<-lm(log(tfat_d_cal)~as.factor(Edu_m_level)+income_in_k+AntWtGain_m+Weight_m+Height_m+ndarea, data=yws, na.action=na.exclude)
summary(lsmult)


######################################
#############10/25/2017
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(yws)
yws$wh_ratio=yws$Waist_d/yws$Hip_d

yws$grow9_d[yws$AntHt9_d==1]<--1
yws$grow9_d[yws$AntHt9_d==2]<--1
yws$grow9_d[yws$AntHt9_d==3]<- 0
yws$grow9_d[yws$AntHt9_d==4]<- 1
yws$grow9_d[yws$AntHt9_d==5]<- 1

yws$grow12_d[yws$AntHt12_d==1]<--1
yws$grow12_d[yws$AntHt12_d==2]<--1
yws$grow12_d[yws$AntHt12_d==3]<- 0
yws$grow12_d[yws$AntHt12_d==4]<- 1
yws$grow12_d[yws$AntHt12_d==5]<- 1

yws$grow15_d[yws$AntHt15_d==1]<--1
yws$grow15_d[yws$AntHt15_d==2]<--1
yws$grow15_d[yws$AntHt15_d==3]<- 0
yws$grow15_d[yws$AntHt15_d==4]<- 1
yws$grow15_d[yws$AntHt15_d==5]<- 1
attach(yws)

yws$income_in_k=yws$med_income_house_m/1000
attach(yws)

##added: weight and length at birth
lsmult<-lm(DW_bodyfat~as.factor(grow9_d)+MRage_d, data=yws, na.action=na.exclude)
summary(lsmult)

results<-aov(AntDBWt_m~as.factor(grow15_d), data=yws)
summary(results)


lsmult<-lm(log(tfat_d_cal)~income_in_k+AntWtGain_m+Weight_m+Height_m+as.factor(Edu_m_level)+ndarea, data=yws, na.action=na.exclude)
summary(lsmult)

##11/1/2017
newd<-data.frame(grow15_d=c(-1,0,1),MRage_d=rep(mean(yws[,"MRage_d"],na.rm=T),3))
lsm.pd<-predict(lm(wh_ratio~as.factor(grow15_d)+MRage_d, data=yws, na.action=na.exclude),newdata=newd,interval="confidence")

lsmult<-lm(Weight_d_mes~AntDLength_m+MRage_d, data=yws, na.action=na.exclude)
summary(lsmult)



###2017/11/21
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
yws$wh_ratio=yws$Waist_d/yws$Hip_d

#calculate new body fat:
attach(yws)
yws$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
attach(yws)


yws_ph1=read.csv("yw_mri_12sep2016_ph1.csv")
attach(yws_ph1)
yws_ph1$wh_ratio=yws_ph1$Waist_d/yws_ph1$Hip_d
yws_ph1$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
attach(yws_ph1)


yws_ph2=read.csv("yw_mri_12sep2016_ph2.csv") 
attach(yws_ph2)
yws_ph2$wh_ratio=yws_ph2$Waist_d/yws_ph2$Hip_d
yws_ph2$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
attach(yws_ph2)

#yws_t1=yws_ph2[,c("Height_d_mes","Weight_d_mes","MRage_d","AgePerStart_d","wh_ratio","bodyfat_new")]
yws_t1=yws[,c("IGF_1","IGFBP_3","Insulin","Glucose","SHBG","hGH","pwater_d_cal","twater_d_cal","tfat_d_cal")]


sapply(yws_t1, mean, na.rm=TRUE)

lsmult1<-lm(pwater_d_cal~Weight_d_mes+Height_d_mes+MRage_d+wh_ratio+bodyfat_new+log(HOMA2_S), data=yws, na.action=na.exclude)
summary(lsmult1)


yws$grow9_d[yws$AntHt9_d==1]<--1
yws$grow9_d[yws$AntHt9_d==2]<--1
yws$grow9_d[yws$AntHt9_d==3]<- 0
yws$grow9_d[yws$AntHt9_d==4]<- 1
yws$grow9_d[yws$AntHt9_d==5]<- 1

yws$grow12_d[yws$AntHt12_d==1]<--1
yws$grow12_d[yws$AntHt12_d==2]<--1
yws$grow12_d[yws$AntHt12_d==3]<- 0
yws$grow12_d[yws$AntHt12_d==4]<- 1
yws$grow12_d[yws$AntHt12_d==5]<- 1

yws$grow15_d[yws$AntHt15_d==1]<--1
yws$grow15_d[yws$AntHt15_d==2]<--1
yws$grow15_d[yws$AntHt15_d==3]<- 0
yws$grow15_d[yws$AntHt15_d==4]<- 1
yws$grow15_d[yws$AntHt15_d==5]<- 1
attach(yws)


newd<-data.frame(grow12_d=c(-1,0,1),MRage_d=rep(mean(yws[,"MRage_d"],na.rm=T),3))

lsm.pd<-predict(lm(log(tfat_d_cal)~as.factor(grow12_d)+MRage_d, data=yws, na.action=na.exclude),newdata=newd,interval="confidence")

results<-aov(log(bodyfat_new)~MRage_d+as.factor(grow9_d), data=yws)
summary(results)


##load data for HOMA
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
HOMA=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(HOMA)
HOMA$wh_ratio=HOMA$Waist_d/HOMA$Hip_d
HOMA$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
HOMA$bodyfat_new_log=log(HOMA$bodyfat_new)
attach(HOMA)

yws$income_in_k=yws$med_income_house_m/1000
yws$bodyfat_new_log=log(yws$bodyfat_new)
attach(yws)

newd<-data.frame(grow12_d=c(-1,0,1),MRage_d=rep(mean(yws[,"MRage_d"],na.rm=T),3))
                 
newd<-data.frame(grow12_d=c(-1,0,1),MRage_d=rep(mean(yws[,"MRage_d"],na.rm=T),3),
                 Weight_d_mes=rep(mean(yws[,"Weight_d_mes"],na.rm=T),3),
                 Height_d_mes=rep(mean(yws[,"Height_d_mes"],na.rm=T),3),
                 wh_ratio=rep(mean(yws[,"wh_ratio"],na.rm=T),3),
                 bodyfat_new_log=rep(mean(yws[,"bodyfat_new_log"],na.rm=T),3))

lsm.pd<-predict(lm(pwater_d_cal~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+bodyfat_new_log+as.factor(grow12_d), data=yws, na.action=na.exclude),newdata=newd,interval="confidence")

results<-aov(pwater_d_cal~MRage_d+Weight_d_mes+Height_d_mes+wh_ratio+bodyfat_new_log+as.factor(grow9_d), data=yws)
summary(results)


lsmult1<-lm(log(tfat_d_cal)~log(SHBG), data=HOMA, na.action=na.exclude)
summary(lsmult1)


##12/14/2017

setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
yws$wh_ratio=yws$Waist_d/yws$Hip_d

#calculate new body fat:
attach(yws)
yws$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
yws$tvolume=twater_d_cal+tfat_d_cal
attach(yws)


co <- function(v1, ... ){
  C1<-cor(v1[!is.na(v1)&!is.na(pwater_d_cal)],pwater_d_cal[!is.na(pwater_d_cal)&!is.na(v1)])
  C2<-cor(v1[!is.na(v1)&!is.na(twater_d_cal)],twater_d_cal[!is.na(twater_d_cal)&!is.na(v1)])
  C3<-cor(v1[!is.na(v1)&!is.na(tfat_d_cal)],tfat_d_cal[!is.na(tfat_d_cal)&!is.na(v1)])
  C4<-cor(v1[!is.na(v1)&!is.na(tvolume)],tvolume[!is.na(tvolume)&!is.na(v1)],method="spearman")
  C5<-cor(v1[!is.na(v1)&!is.na(MRage_d)],MRage_d[!is.na(MRage_d)&!is.na(v1)],method="spearman")
  C6<-cor(v1[!is.na(v1)&!is.na(Weight_d_mes)],Weight_d_mes[!is.na(Weight_d_mes)&!is.na(v1)])
  C7<-cor(v1[!is.na(v1)&!is.na(Height_d_mes)],Height_d_mes[!is.na(Height_d_mes)&!is.na(v1)])
  
  C8<-cor(v1[!is.na(v1)&!is.na(wh_ratio)],wh_ratio[!is.na(wh_ratio)&!is.na(v1)])
  C9<-cor(v1[!is.na(v1)&!is.na(bodyfat_new)],bodyfat_new[!is.na(bodyfat_new)&!is.na(v1)])
  C10<-cor(v1[!is.na(v1)&!is.na(AntDBWt_m)],AntDBWt_m[!is.na(AntDBWt_m)&!is.na(v1)])
  C11<-cor(v1[!is.na(v1)&!is.na(AntDLength_m)],AntDLength_m[!is.na(AntDLength_m)&!is.na(v1)])
  
  CL<-data.frame(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11)
  CL
}

co(pwater_d_cal)

co(twater_d_cal)

co(tfat_d_cal)

co(tvolume)

co(MRage_d)

co(Weight_d_mes)

co(Height_d_mes)

co(wh_ratio)

co(bodyfat_new)

co(AntDBWt_m)

results<-aov(AntDBWt_m~as.factor(grow9_d), data=yws)
summary(results)


##
lsmult1<-lm(bodyfat_new~MRage_d+log(HOMA2_IR), data=HOMA, na.action=na.exclude)
summary(lsmult1)

##1/17/2018
#number check
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
HOMA=read.csv("yws_nomiss_noextreme_5112017_leg.csv") 
attach(HOMA)
HOMA$wh_ratio=HOMA$Waist_d/HOMA$Hip_d
HOMA$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
HOMA$bodyfat_new_log=log(HOMA$bodyfat_new)
attach(HOMA)

summary(SHBG)


setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_01apr2016.csv") 
attach(yws)

summary(IGFBP_3)

IGF_NA<-yws[which(is.na(yws$IGF_1)),]
write.csv(IGF_NA,"C:/Users/jodie.zhu/Desktop/IGF_NA.csv")



#######2/16/2018##########
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_02feb2018.csv") 
attach(yws)

mean(yws$N_hGH, na.rm=TRUE)
sd(yws$N_hGH, na.rm=TRUE)
summary(yws$N_hGH)

##PH1
yws_ph1=read.csv("yw_mri_02feb2018_ph1.csv") 
attach(yws_ph1)

mean(yws_ph1$N_IGF_1, na.rm=TRUE)
sd(yws_ph1$N_IGF_1, na.rm=TRUE)
summary(yws_ph1$N_IGF_1)

##PH2
yws_ph2=read.csv("yw_mri_02feb2018_ph2.csv") 
attach(yws_ph2)

mean(yws_ph2$N_hGH, na.rm=TRUE)
sd(yws_ph2$N_hGH, na.rm=TRUE)
summary(yws_ph2$N_hGH)


##03/08/2018
#Redo analysis on the new data
#Analysis for table 3
setwd("C:/Jie/self/UHN/boyd/11Jan2016 data/homa")
homa=read.csv("yw_mri_28feb2018_homa2.csv") 
attach(homa)

homa_in25=subset(homa, (homa$Insulin<=25|is.na(homa$Insulin)) & (homa$Glucose<=99 | is.na(homa$Glucose)) & (!is.na(homa$HOMA2__S)))
attach(homa_in25)

homa_in25$wh_ratio=homa_in25$Waist_d/homa_in25$Hip_d
homa_in25$bodyfat_new=ifelse((Triceps_d+Subscap_d)<=35, 1.33*(Triceps_d+Subscap_d)-0.013*(Triceps_d+Subscap_d)^2-2.5, 0.546*(Triceps_d+Subscap_d)+0.97)
homa_in25$bodyfat_new_log=log(homa_in25$bodyfat_new)
attach(homa_in25)

lsmult1<-lm(bodyfat_new~log(hGH)+MRage_d, data=homa_in25, na.action=na.exclude)
summary(lsmult1)

## 5/2/2018 
#to repeat the result
setwd("/Users/jie/Documents/UHN/boyd/11Jan2016 data/homa")
homa=read.csv("yw_mri_28feb2018_homa2.csv") 
attach(homa)

homa_in25=subset(homa, (homa$Insulin<=25 & homa$Glucose<=99 ))
attach(homa_in25)


setwd("/Users/jie/Documents/UHN/boyd/11Jan2016 data")
yws=read.csv("yw_mri_02feb2018.csv") 
attach(yws)

summary(yws$N_insulin)
summary(yws$N_Glucose)

yws_in_gl=subset(yws, (!is.na(yws$N_insulin) & !is.na(yws$N_Glucose) ))

sub_yws_in_gl <- yws_in_gl[c("StudyID","DNum")]
sub_homa <- homa[c("StudyID","DNum")]

library(dplyr)
anti_join(sub_yws_in_gl, sub_homa, by = c("StudyID","DNum"))







