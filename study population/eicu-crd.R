library(readxl)
library(tidyverse)
library(lubridate)
library(factoextra)
library(DataExplorer)
library(missForest)
library(cluster)
library(ggalluvial)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggsci)
library(CBCgrps)
library(Publish)
library(tidyLPA)
library(data.table)
library(openxlsx)
####1.0 sepsis diagnosis
patient <- fread(input="zcat < /Volumes/F/eicu/patient.csv.gz",
                 sep = ',',header = T,
                 stringsAsFactors=T,
                 na.strings=c("",'Unknown','Other','NA'))

colnames(dtsepsis)

dtsepsis <-patient %>%
  filter(str_detect(apacheadmissiondx,"Sepsis")&hospitaldischargestatus!=''&unitdischargestatus!=''
         ) %>%
  filter(unitvisitnumber==1)%>%
  mutate(age=gsub(pattern = "> ",replacement = "",age))%>%
  mutate(age=as.numeric(age))%>%
  filter(age>=18)%>%
  filter(unitdischargeoffset>24*60)%>%
  
  dplyr::select(patientunitstayid,patienthealthsystemstayid,uniquepid,hospitalid,gender,age,ethnicity,apacheadmissiondx,
                admissionheight,hospitaladmittime24,hospitaldischargeyear,hospitaldischargeoffset,hospitaldischargestatus,
                unitadmitsource,unitdischargeoffset,unitdischargestatus)

####weight
dtsepsis<-dtsepsis%>%
  mutate(admissionweight=ifelse(admissionweight<=30,30,admissionweight))

dtsepsis$admissionweight[which(is.na(dtsepsis$admissionweight))]<-77

summary(dtsepsis$admissionweight)

####2.0 vasopressor
nurseCharting <- fread(input="zcat < /Volumes/F/eicu/nurseCharting.csv.gz",
                       sep = ',',header = T)#to extract GCS score
lab<-fread(input="zcat < /Volumes/F/eicu/lab.csv.gz",sep = ',',header = T)
apachePatientResult <- fread(input="zcat </Volumes/F/eicu/apachePatientResult.csv.gz",
                             sep = ',',header = T)

infusionDrug <- fread(input="zcat < /Volumes/F/eicu//infusionDrug.csv.gz",sep = ',',header = T)
infusionDrug[,drugrate:=as.numeric(drugrate)]
infusionDrug<-infusionDrug%>%
  filter(infusionoffset<=60*24*4)%>%
  mutate(infusionoffset=ifelse(infusionoffset<=0,0.1,infusionoffset))
meanWeight <- infusionDrug[,mean(patientweight,na.rm = T)]
######dopamine
Dopamine <- infusionDrug[grep('Dopamine',infusionDrug$drugname),]
Dopamine <- Dopamine[,.(patientunitstayid,infusionoffset,drugname,drugrate)]

Dopamine$drugname<-factor(Dopamine$drugname)
summary(Dopamine$drugname)

Dopamine<-Dopamine%>%
  filter(drugname%in%c("Dopamine (mcg/kg/min)","Dopamine (ml/hr)"))%>%
  right_join(dtsepsis[,c(1,23)],by="patientunitstayid")


Dopamine[,drugrateStand:=ifelse(drugname=="Dopamine (ml/hr)",drugrate*400*1000/(250*60*admissionweight),drugrate)]
Dopamine[,infusiondays:=cut(infusionoffset,breaks = seq(0,60*24*4,60*24),
                            labels = seq(1,4))]
colnames(Dopamine)
Dopamine <- Dopamine[is.finite(drugrate),]%>%
  dplyr::select(patientunitstayid,drugrateStand,infusiondays)%>%
  set_names("patientunitstayid","dopa_rate","days")
Dopamine<-Dopamine%>%
  mutate(dopa_rate=ifelse(dopa_rate>=30,30,dopa_rate))

Dopamine <- Dopamine[,.(dopa_ratemax=max(dopa_rate,na.rm = T)),
                     by = .(patientunitstayid, days)]

#norepinephrine
Norepi <- infusionDrug[grep('Norepinephrine',infusionDrug$drugname),]
Norepi <- Norepi[,.(patientunitstayid,infusionoffset,drugname,drugrate)]
Norepi<-Norepi%>%
  filter(drugrate!="")
Norepi$drugname<-factor(Norepi$drugname)

Norepi<-Norepi%>%
  filter(drugname%in%c("Norepinephrine (mcg/kg/min)","Norepinephrine (mcg/min)","Norepinephrine (ml/hr)",
                       "Norepinephrine MAX 32 mg Dextrose 5% 250 ml (mcg/min)",
                       "Norepinephrine STD 4 mg Dextrose 5% 250 ml (mcg/min)"))%>%
  right_join(dtsepsis[,c(1,23)],by="patientunitstayid")
  


Norepi[,drugrateStand:=ifelse(grepl('(mcg/min)',drugname),drugrate/admissionweight,
                              ifelse(drugname=='Norepinephrine (ml/hr)',drugrate*4*1000/(250*60*admissionweight),drugrate))]

Norepi[,infusiondays:=cut(infusionoffset,breaks = seq(0,60*24*4,60*24),labels = seq(1,4))]
Norepi <- Norepi[is.finite(drugrateStand),]%>%
  dplyr::select(patientunitstayid,drugrateStand,infusiondays)%>%
  set_names("patientunitstayid","norepi_rate","days")

Norepi<-Norepi%>%
  mutate(norepi_rate=ifelse(norepi_rate>=5,5,norepi_rate))

Norepi <- Norepi[,.(norepi_ratemax=max(norepi_rate,na.rm = T)),
                     by = .(patientunitstayid, days)]

########Epinephrine
Epinephrine <- infusionDrug[grep('Epinephrine',infusionDrug$drugname),]
Epinephrine <- Epinephrine[,.(patientunitstayid,infusionoffset,drugname,drugrate)]
Epinephrine$drugname<-factor(Epinephrine$drugname)

Epinephrine<-Epinephrine%>%
  filter(drugname%in%c("Epinephrine (mcg/kg/min)","Epinephrine (mcg/min)","Epinephrine (ml/hr)"))%>%
  right_join(dtsepsis[,c(1,23)],by="patientunitstayid")



Epinephrine[,drugrateStand:=ifelse(drugname=='Epinephrine (ml/hr)',drugrate*4*1000/(250*60*admissionweight),
                                   ifelse(drugname=='Epinephrine (mcg/min)',
                                          drugrate/admissionweight,drugrate))]
Epinephrine[,infusiondays:=cut(infusionoffset,breaks =
                                 seq(0,60*24*4,60*24),
                               labels = seq(1,4))]

Epinephrine <- Epinephrine[is.finite(drugrateStand),]%>%
  dplyr::select(patientunitstayid,drugrateStand,infusiondays)%>%
  set_names("patientunitstayid","epine_rate","days")

summary(Epinephrine)

Epinephrine<-Epinephrine%>%
  mutate(epine_rate=ifelse(epine_rate>=5,5,epine_rate))


Epinephrine <- Epinephrine[,.(epine_ratemax=max(epine_rate,na.rm = T)),
                 by = .(patientunitstayid, days)]


#####vasopressin（Vasopressin (units/min)）

Vasopressin <- infusionDrug[grep('Vasopressin',infusionDrug$drugname),]

Vasopressin <- Vasopressin[,.(patientunitstayid,infusionoffset,drugname,drugrate)]
Vasopressin$drugname<-factor(Vasopressin$drugname)
summary(Vasopressin$drugname)

Vasopressin<-Vasopressin%>%
  filter(drugname%in%c("Vasopressin (units/min)","Vasopressin (ml/hr)"))%>%
  right_join(dtsepsis[,c(1,23)],by="patientunitstayid")


Vasopressin[,drugrateStand:=ifelse(drugname=='Vasopressin (ml/hr)',drugrate/400,drugrate)]
Vasopressin[,infusiondays:=cut(infusionoffset,breaks =
                                 seq(0,60*24*4,60*24),
                               labels = seq(1,4))]

Vasopressin <- Vasopressin[is.finite(drugrateStand),]%>%
  dplyr::select(patientunitstayid,drugrateStand,infusiondays)%>%
  set_names("patientunitstayid","vasopressin_rate","days")

summary(Vasopressin)

Vasopressin<-Vasopressin%>%
  mutate(vasopressin_rate=ifelse(vasopressin_rate>=1.0,1.0,vasopressin_rate))


Vasopressin <- Vasopressin[,.(vasopressin_ratemax=max(vasopressin_rate,na.rm = T)),
                           by = .(patientunitstayid, days)]

######Phenylephrine (mcg/min)

Pheny<-infusionDrug[grep('Phenylephrine',infusionDrug$drugname),]

Pheny<-Pheny[,.(patientunitstayid,infusionoffset,drugname,drugrate)]
Pheny$drugname<-factor(Pheny$drugname)
summary(Pheny$drugname)

Pheny<-Pheny%>%
  filter(drugname%in%c("Phenylephrine  STD 20 mg Sodium Chloride 0.9% 250 ml (mcg/min)","Phenylephrine (mcg/kg/min)",
                       "Phenylephrine (mcg/min)","Phenylephrine (ml/hr)","Phenylephrine  MAX 100 mg Sodium Chloride 0.9% 250 ml (mcg/min)"))%>%
  right_join(dtsepsis[,c(1,23)],by="patientunitstayid")

Pheny[,drugrateStand:=ifelse(drugname=="Phenylephrine  STD 20 mg Sodium Chloride 0.9% 250 ml (mcg/min)",drugrate/admissionweight,
                             ifelse(drugname=="Phenylephrine (mcg/min)",drugrate/admissionweight,
                                    ifelse(drugname=="Phenylephrine  MAX 100 mg Sodium Chloride 0.9% 250 ml (mcg/min)",drugrate/admissionweight,
                                           ifelse(drugname=="Phenylephrine (ml/hr)",drugrate*40*1000/(250*60*admissionweight),drugrate))))]

Pheny[,infusiondays:=cut(infusionoffset,breaks =
                                 seq(0,60*24*4,60*24),
                               labels = seq(1,4))]

Pheny <- Pheny[is.finite(drugrateStand),]%>%
  dplyr::select(patientunitstayid,drugrateStand,infusiondays)%>%
  set_names("patientunitstayid","phen_rate","days")

summary(Pheny)

Pheny<-Pheny%>%
  mutate(phen_rate=ifelse(phen_rate>=10,10,phen_rate))

Pheny <- Pheny[,.(phen_ratemax=max(phen_rate,na.rm = T)),
                           by = .(patientunitstayid, days)]

###vasopressor
colnames(dtsepsis)
days<-dtsepsis[,c(1,26)]%>%
  mutate(days=ifelse(unitdischargeoffset<=4*24*60,unitdischargeoffset/60/24,4))%>%
           select(patientunitstayid,days)

days[,days:=ceiling(days)]
days <- days[ ,.(days=1:days),by=c("patientunitstayid")]

days$days<-as.numeric(days$days)
Dopamine$days<-as.numeric(Dopamine$days)
Norepi$days<-as.numeric(Norepi$days)
Epinephrine$days<-as.numeric(Epinephrine$days)
Vasopressin$days<-as.numeric(Vasopressin$days)
Pheny$days<-as.numeric(Pheny$days)


dyna_vaso<-days%>%
  left_join(Dopamine,by=c("patientunitstayid"="patientunitstayid","days"="days"))%>%
  left_join(Norepi,by=c("patientunitstayid"="patientunitstayid","days"="days"))%>%
  left_join(Epinephrine,by=c("patientunitstayid"="patientunitstayid","days"="days"))%>%
  left_join(Vasopressin,by=c("patientunitstayid"="patientunitstayid","days"="days"))%>%
  left_join(Pheny,by=c("patientunitstayid"="patientunitstayid","days"="days"))

dyn_vaso_1<-sqldf("select patientunitstayid,days,ROUND(COALESCE(norepi_ratemax, 0)
  +COALESCE(epine_ratemax, 0)
  +COALESCE(phen_ratemax/10, 0)
  +COALESCE(dopa_ratemax/150, 0)
                  +COALESCE(vasopressin_ratemax*2.5, 0),4) as nee from dyna_vaso
                  where norepi_ratemax IS NOT NULL
OR epine_ratemax IS NOT NULL
OR phen_ratemax IS NOT NULL
OR dopa_ratemax IS NOT NULL
OR vasopressin_ratemax IS NOT NULL")
dyn_vaso_1<-as.data.table(dyn_vaso_1)

dyn_vaso_2<-dyn_vaso_1%>%
  filter(days==1)%>%
  filter(nee>0)

##########group of patients
####1.rapid death
finalpatient<-dtsepsis%>%
  right_join(dyn_vaso_2[,c(1,3)],by="patientunitstayid")

summary(finalpatient$unitdischargestatus)

group_1<-sqldf("select * from finalpatient where unitdischargestatus in ('Expired')
               and unitdischargeoffset<=4320")
group_1<-group_1%>%
  mutate(Group="Raipd_death")

#####2.recovery
dyn_vaso_3<-dyn_vaso_1%>%
  spread(days,nee)%>%
  set_names("patientunitstayid","d1_nee","d2_nee","d3_nee","d4_nee")%>%
  filter(d1_nee!="")%>%
  filter(d1_nee>0)

dyn_vaso_3$d2_nee[which(is.na(dyn_vaso_3$d2_nee))]<-0
dyn_vaso_3$d3_nee[which(is.na(dyn_vaso_3$d3_nee))]<-0
dyn_vaso_3$d4_nee[which(is.na(dyn_vaso_3$d4_nee))]<-0


dyn_vaso_4<-sqldf("select * from dyn_vaso_3 where 
                  (d2_nee =0 and d3_nee=0)
                  or (d3_nee=0 and d4_nee=0)")

group_2<-sqldf("select * from finalpatient where patientunitstayid in (select patientunitstayid from dyn_vaso_4)
               and patientunitstayid not in (select patientunitstayid from group_1)")
group_2$Group="recovery"


####3.0persistent ill
group_3<-sqldf("select * from finalpatient where patientunitstayid not in (select patientunitstayid from group_1)
                      and patientunitstayid not in (select patientunitstayid from group_2)")
group_3$Group="persistent_ill"

summary(group_2$unitdischargestatus)

###data
finalpatient_2<-rbind(group_1,group_2,group_3)

#########date extraction
####Invasive-SBP,DBP,MAP,Non-invasive SBP,DBP,MAP,
##Temp,Respir rate,Heartrate,SPO2
colnames(nurseCharting)

sepsis_vital<-nurseCharting%>%
  select(patientunitstayid,nursingchartoffset,nursingchartcelltypevalname,nursingchartvalue)%>%
  filter(nursingchartoffset>=-6*60)%>%
  filter(nursingchartoffset<=24*60)%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")
  
####hr
sepsis_hr<-sepsis_vital[grep('Heart Rate',sepsis_vital$nursingchartcelltypevalname),]
sepsis_hr[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_hr<-sepsis_hr%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=200,200,nursingchartvalue)))

sepsis_hr<-sepsis_hr[,.(heartrate_max=max(nursingchartvalue,na.rm=T),heartrate_min=min(nursingchartvalue,na.rm=T),
                           heartrate_mean=mean(nursingchartvalue,na.rm=T),heartrate_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_hr<-sepsis_hr%>%
  mutate(heartrate_vr=heartrate_sd/heartrate_mean)


####isbp
sepsis_isbp<-sepsis_vital[grep('Invasive BP Systolic',sepsis_vital$nursingchartcelltypevalname),]
sepsis_isbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_isbp<-sepsis_isbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=250,250,nursingchartvalue)))

sepsis_isbp<-sepsis_isbp[,.(isbp_max=max(nursingchartvalue,na.rm=T),isbp_min=min(nursingchartvalue,na.rm=T),
                             isbp_mean=mean(nursingchartvalue,na.rm=T),isbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_isbp<-sepsis_isbp[is.finite(isbp_max),]
sepsis_isbp<-sepsis_isbp%>%
  mutate(isbp_vr=isbp_sd/isbp_mean)

####idbp(Invasive BP Diastolic)

sepsis_idbp<-sepsis_vital[grep('Invasive BP Diastolic',sepsis_vital$nursingchartcelltypevalname),]
sepsis_idbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_idbp<-sepsis_idbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=150,150,nursingchartvalue)))

sepsis_idbp<-sepsis_idbp[,.(idbp_max=max(nursingchartvalue,na.rm=T),idbp_min=min(nursingchartvalue,na.rm=T),
                             idbp_mean=mean(nursingchartvalue,na.rm=T),idbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_idbp<-sepsis_idbp[is.finite(idbp_max),]
sepsis_idbp<-sepsis_idbp%>%
  mutate(idbp_vr=idbp_sd/idbp_mean)


####imap
sepsis_imbp<-sepsis_vital[grep('Invasive BP Mean',sepsis_vital$nursingchartcelltypevalname),]
sepsis_imbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_imbp<-sepsis_imbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=150,150,nursingchartvalue)))

sepsis_imbp<-sepsis_imbp[,.(imbp_max=max(nursingchartvalue,na.rm=T),imbp_min=min(nursingchartvalue,na.rm=T),
                             imbp_mean=mean(nursingchartvalue,na.rm=T),imbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]

sepsis_imbp<-sepsis_imbp[is.finite(imbp_max),]
sepsis_imbp<-sepsis_imbp%>%
  mutate(imbp_vr=imbp_sd/imbp_mean)



####nisbp
sepsis_nisbp<-sepsis_vital[grep('Non-Invasive BP Systolic',sepsis_vital$nursingchartcelltypevalname),]
sepsis_nisbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_nisbp<-sepsis_nisbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=250,250,nursingchartvalue)))

sepsis_nisbp<-sepsis_nisbp[,.(nisbp_max=max(nursingchartvalue,na.rm=T),nisbp_min=min(nursingchartvalue,na.rm=T),
                            nisbp_mean=mean(nursingchartvalue,na.rm=T),nisbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_nisbp<-sepsis_nisbp[is.finite(nisbp_max),]
sepsis_nisbp<-sepsis_nisbp%>%
  mutate(nisbp_vr=nisbp_sd/nisbp_mean)

####idbp(Invasive BP Diastolic)

sepsis_nidbp<-sepsis_vital[grep('Non-Invasive BP Diastolic',sepsis_vital$nursingchartcelltypevalname),]
sepsis_nidbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_nidbp<-sepsis_nidbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=150,150,nursingchartvalue)))

sepsis_nidbp<-sepsis_nidbp[,.(nidbp_max=max(nursingchartvalue,na.rm=T),nidbp_min=min(nursingchartvalue,na.rm=T),
                            nidbp_mean=mean(nursingchartvalue,na.rm=T),nidbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_nidbp<-sepsis_nidbp[is.finite(nidbp_max),]
sepsis_nidbp<-sepsis_nidbp%>%
  mutate(nidbp_vr=nidbp_sd/nidbp_mean)


####imap
sepsis_nimbp<-sepsis_vital[grep('Non-Invasive BP Mean',sepsis_vital$nursingchartcelltypevalname),]
sepsis_nimbp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_nimbp<-sepsis_nimbp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=150,150,nursingchartvalue)))

sepsis_nimbp<-sepsis_nimbp[,.(nimbp_max=max(nursingchartvalue,na.rm=T),nimbp_min=min(nursingchartvalue,na.rm=T),
                            nimbp_mean=mean(nursingchartvalue,na.rm=T),nimbp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]

sepsis_nimbp<-sepsis_nimbp[is.finite(nimbp_max),]
sepsis_nimbp<-sepsis_nimbp%>%
  mutate(nimbp_vr=nimbp_sd/nimbp_mean)

#####temp
sepsis_temp<-sepsis_vital[grep('Temperature \\(F\\)|Temperature \\(C\\)',sepsis_vital$nursingchartcelltypevalname),]
sepsis_temp[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_temp<-sepsis_temp[,nursingchartvalue :=
                   ifelse(nursingchartcelltypevalname=="Temperature (F)",
                          (nursingchartvalue-32)/1.8,
                          nursingchartvalue)]


sepsis_temp<-sepsis_temp%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=45,45,nursingchartvalue)))

sepsis_temp<-sepsis_temp[,.(temp_max=max(nursingchartvalue,na.rm=T),temp_min=min(nursingchartvalue,na.rm=T),
                             temp_mean=mean(nursingchartvalue,na.rm=T),temp_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]
sepsis_temp<-sepsis_temp[is.finite(temp_max),]

sepsis_temp<-sepsis_temp%>%
  mutate(temp_vr=temp_sd/temp_mean)

####RR
sepsis_rr<-sepsis_vital[grep('Respiratory Rate',sepsis_vital$nursingchartcelltypevalname),]
sepsis_rr[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_rr<-sepsis_rr%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=1,1,
                                  ifelse(nursingchartvalue>=60,60,nursingchartvalue)))

sepsis_rr<-sepsis_rr[,.(rr_max=max(nursingchartvalue,na.rm=T),rr_min=min(nursingchartvalue,na.rm=T),
                              rr_mean=mean(nursingchartvalue,na.rm=T),rr_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]

sepsis_rr<-sepsis_rr[is.finite(rr_max),]
sepsis_rr<-sepsis_rr%>%
  mutate(rr_vr=rr_sd/rr_mean)


###spo2
sepsis_spo2<-sepsis_vital[grep('O2 Saturation',sepsis_vital$nursingchartcelltypevalname),]
sepsis_spo2[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_spo2<-sepsis_spo2%>%
  mutate(nursingchartvalue=ifelse(nursingchartvalue<=30,30,
                                  ifelse(nursingchartvalue>=100,100,nursingchartvalue)))

sepsis_spo2<-sepsis_spo2[,.(spo2_max=max(nursingchartvalue,na.rm=T),spo2_min=min(nursingchartvalue,na.rm=T),
                        spo2_mean=mean(nursingchartvalue,na.rm=T),spo2_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]

sepsis_spo2<-sepsis_spo2[is.finite(spo2_max),]
sepsis_spo2<-sepsis_spo2%>%
  mutate(spo2_vr=spo2_sd/spo2_mean)


#####abg(pH, lac,HCO3-,BE,PO2,PCO2,PF ratio,FiO2)


sepsis_lab<-lab%>%
  filter(labresultoffset>=-24*60)%>%
  filter(labresultoffset<=48*60)%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")

####ph
sepsis_ph<-sepsis_lab[grep('pH',sepsis_lab$labname),]
sepsis_ph[,labresult:=as.numeric(labresult)]

sepsis_ph<-sepsis_ph%>%
  mutate(labresult=ifelse(labresult<=6.0,6.0,
                                  ifelse(labresult>=8.0,8.0,labresult)))

sepsis_ph<-sepsis_ph[,.(ph_max=max(labresult,na.rm=T),ph_min=min(labresult,na.rm=T),
                            ph_mean=mean(labresult,na.rm=T),ph_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_ph<-sepsis_ph[is.finite(ph_max),]
sepsis_ph<-sepsis_ph%>%
  mutate(ph_vr=ph_sd/ph_mean)

####lac
sepsis_lac<-sepsis_lab[grep('lactate',sepsis_lab$labname),]
sepsis_lac[,labresult:=as.numeric(labresult)]


sepsis_lac<-sepsis_lac[,.(lac_max=max(labresult,na.rm=T),lac_min=min(labresult,na.rm=T),
                        lac_mean=mean(labresult,na.rm=T),lac_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_lac<-sepsis_lac[is.finite(lac_max),]
sepsis_lac<-sepsis_lac%>%
  mutate(lac_vr=lac_sd/lac_mean)

###pO2

sepsis_po2<-sepsis_lab[grep('paO2',sepsis_lab$labname),]
sepsis_po2[,labresult:=as.numeric(labresult)]

sepsis_po2<-sepsis_po2%>%
  mutate(labresult=ifelse(labresult<=30,30,
                          ifelse(labresult>=500,500,labresult)))

sepsis_po2<-sepsis_po2[,.(po2_max=max(labresult,na.rm=T),po2_min=min(labresult,na.rm=T),
                        po2_mean=mean(labresult,na.rm=T),po2_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_po2<-sepsis_po2[is.finite(po2_max),]
sepsis_po2<-sepsis_po2%>%
  mutate(po2_vr=po2_sd/po2_mean)

####pco2
sepsis_pco2<-sepsis_lab[grep('paCO2',sepsis_lab$labname),]
sepsis_pco2[,labresult:=as.numeric(labresult)]

sepsis_pco2<-sepsis_pco2%>%
  mutate(labresult=ifelse(labresult<=10,10,
                          ifelse(labresult>=150,150,labresult)))

sepsis_pco2<-sepsis_pco2[,.(pco2_max=max(labresult,na.rm=T),pco2_min=min(labresult,na.rm=T),
                          pco2_mean=mean(labresult,na.rm=T),pco2_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_pco2<-sepsis_pco2[is.finite(pco2_max),]
sepsis_pco2<-sepsis_pco2%>%
  mutate(pco2_vr=pco2_sd/pco2_mean)


####fio2
sepsis_fio2<-sepsis_lab[grep('FiO2',sepsis_lab$labname),]
sepsis_fio2[,labresult:=as.numeric(labresult)]

sepsis_fio2<-sepsis_fio2%>%
  mutate(labresult=ifelse(labresult<=21,21,
                          ifelse(labresult>=100,100,labresult)))

sepsis_fio2<-sepsis_fio2[,.(fio2_max=max(labresult,na.rm=T),fio2_min=min(labresult,na.rm=T),
                            fio2_mean=mean(labresult,na.rm=T),fio2_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_fio2<-sepsis_fio2[is.finite(fio2_max),]
sepsis_fio2<-sepsis_fio2%>%
  mutate(fio2_vr=fio2_sd/fio2_mean)

####Base Excess
sepsis_baseexcess<-sepsis_lab[grep('Base Excess',sepsis_lab$labname),]
sepsis_baseexcess[,labresult:=as.numeric(labresult)]

sepsis_baseexcess<-sepsis_baseexcess%>%
  mutate(labresult=ifelse(labresult<=-30,30,
                          ifelse(labresult>=30,30,labresult)))

sepsis_baseexcess<-sepsis_baseexcess[,.(baseexcess_max=max(labresult,na.rm=T),baseexcess_min=min(labresult,na.rm=T),
                            baseexcess_mean=mean(labresult,na.rm=T),baseexcess_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_baseexcess<-sepsis_baseexcess[is.finite(baseexcess_max),]
sepsis_baseexcess<-sepsis_baseexcess%>%
  mutate(baseexcess_vr=baseexcess_sd/baseexcess_mean)


#####sepsis_bicarbonate
sepsis_bicarbonate<-sepsis_lab[grep('bicarbonate',sepsis_lab$labname),]
sepsis_bicarbonate[,labresult:=as.numeric(labresult)]

sepsis_bicarbonate<-sepsis_bicarbonate%>%
  mutate(labresult=ifelse(labresult<=5,5,
                          ifelse(labresult>=50,50,labresult)))

sepsis_bicarbonate<-sepsis_bicarbonate[,.(bicarbonate_max=max(labresult,na.rm=T),bicarbonate_min=min(labresult,na.rm=T),
                                          bicarbonate_mean=mean(labresult,na.rm=T),bicarbonate_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_bicarbonate<-sepsis_bicarbonate[is.finite(bicarbonate_max),]
sepsis_bicarbonate<-sepsis_bicarbonate%>%
  mutate(bicarbonate_vr=bicarbonate_sd/bicarbonate_mean)

#####sepsis_pf
sepsis_pf<-pf%>%
  mutate(fio2=ifelse(fio2<=21,21,ifelse(fio2>=100,100,fio2)))%>%
  mutate(pao2=ifelse(pao2<=30,30,ifelse(pao2>=500,500,pao2)))%>%
  mutate(pf=pao2/fio2)%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")%>%
  select(patientunitstayid,pf)
sepsis_pf<-as.data.table(sepsis_pf)

sepsis_pf<-sepsis_pf[,.(pf_max=max(pf,na.rm=T),pf_min=min(pf,na.rm=T),
                        pf_mean=mean(pf,na.rm=T),pf_sd=sd(pf,na.rm=T)),by=.(patientunitstayid)]

sepsis_pf<-sepsis_pf[is.finite(pf_max),]
sepsis_pf<-sepsis_pf%>%
  mutate(pf_vr=pf_sd/pf_mean)

########（ALT,AST,LDH,TBIL）
sepsis_alt<-sepsis_lab[grep('ALT',sepsis_lab$labname),]
sepsis_alt[,labresult:=as.numeric(labresult)]

sepsis_alt<-sepsis_alt%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=3000,3000,labresult)))

sepsis_alt<-sepsis_alt[,.(alt_max=max(labresult,na.rm=T),alt_min=min(labresult,na.rm=T),
                                          alt_mean=mean(labresult,na.rm=T),alt_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_alt<-sepsis_alt[is.finite(alt_max),]
sepsis_alt<-sepsis_alt%>%
  mutate(alt_vr=alt_sd/alt_mean)


#####
sepsis_ast<-sepsis_lab[grep('AST',sepsis_lab$labname),]
sepsis_ast[,labresult:=as.numeric(labresult)]

sepsis_ast<-sepsis_ast%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=3000,3000,labresult)))

sepsis_ast<-sepsis_ast[,.(ast_max=max(labresult,na.rm=T),ast_min=min(labresult,na.rm=T),
                          ast_mean=mean(labresult,na.rm=T),ast_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_ast<-sepsis_ast[is.finite(ast_max),]
sepsis_ast<-sepsis_ast%>%
  mutate(ast_vr=ast_sd/ast_mean)

#####
sepsis_bili<-sepsis_lab[grep('total bilirubin',sepsis_lab$labname),]
sepsis_bili[,labresult:=as.numeric(labresult)]

sepsis_bili<-sepsis_bili%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=20,20,labresult)))

sepsis_bili<-sepsis_bili[,.(bili_max=max(labresult,na.rm=T),bili_min=min(labresult,na.rm=T),
                          bili_mean=mean(labresult,na.rm=T),bili_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_bili<-sepsis_bili[is.finite(bili_max),]
sepsis_bili<-sepsis_bili%>%
  mutate(bili_vr=bili_sd/bili_mean)

#########4.8（albumin,bun,calcium,chloride,creatinine,glucose,sodium,potassium）
sepsis_albumin<-sepsis_lab[grep('albumin',sepsis_lab$labname),]
sepsis_albumin[,labresult:=as.numeric(labresult)]

sepsis_albumin<-sepsis_albumin%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=7,7,labresult)))

sepsis_albumin<-sepsis_albumin[,.(albumin_max=max(labresult,na.rm=T),albumin_min=min(labresult,na.rm=T),
                                  albumin_mean=mean(labresult,na.rm=T),albumin_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_albumin<-sepsis_albumin[is.finite(albumin_max),]
sepsis_albumin<-sepsis_albumin%>%
  mutate(albumin_vr=albumin_sd/albumin_mean)
######

sepsis_bun<-sepsis_lab[grep('BUN',sepsis_lab$labname),]
sepsis_bun[,labresult:=as.numeric(labresult)]

sepsis_bun<-sepsis_bun%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=115,115,labresult)))

sepsis_bun<-sepsis_bun[,.(bun_max=max(labresult,na.rm=T),bun_min=min(labresult,na.rm=T),
                          bun_mean=mean(labresult,na.rm=T),bun_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_bun<-sepsis_bun[is.finite(bun_max),]
sepsis_bun<-sepsis_bun%>%
  mutate(bun_vr=bun_sd/bun_mean)

####
sepsis_calcium<-sepsis_lab[grep('calcium',sepsis_lab$labname),]
sepsis_calcium[,labresult:=as.numeric(labresult)]

sepsis_calcium<-sepsis_calcium%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=50,50,labresult)))

sepsis_calcium<-sepsis_calcium[,.(calcium_max=max(labresult,na.rm=T),calcium_min=min(labresult,na.rm=T),
                                  calcium_mean=mean(labresult,na.rm=T),calcium_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_calcium<-sepsis_calcium[is.finite(calcium_max),]
sepsis_calcium<-sepsis_calcium%>%
  mutate(calcium_vr=calcium_sd/calcium_mean)

#####
sepsis_chloride<-sepsis_lab[grep('chloride',sepsis_lab$labname),]
sepsis_chloride[,labresult:=as.numeric(labresult)]

summary(sepsis_chloride)


sepsis_chloride<-sepsis_chloride[,.(chloride_max=max(labresult,na.rm=T),chloride_min=min(labresult,na.rm=T),
                                    chloride_mean=mean(labresult,na.rm=T),chloride_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_chloride<-sepsis_chloride[is.finite(chloride_max),]
sepsis_chloride<-sepsis_chloride%>%
  mutate(chloride_vr=chloride_sd/chloride_mean)


####
sepsis_creatinine<-sepsis_lab[grep('creatinine',sepsis_lab$labname),]
sepsis_creatinine[,labresult:=as.numeric(labresult)]

sepsis_creatinine<-sepsis_creatinine%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=8,8,labresult)))

sepsis_creatinine<-sepsis_creatinine[,.(creatinine_max=max(labresult,na.rm=T),creatinine_min=min(labresult,na.rm=T),
                                        creatinine_mean=mean(labresult,na.rm=T),creatinine_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_creatinine<-sepsis_creatinine[is.finite(creatinine_max),]
sepsis_creatinine<-sepsis_creatinine%>%
  mutate(creatinine_vr=creatinine_sd/creatinine_mean)

####
sepsis_glucose<-sepsis_lab[grep('bedside glucose',sepsis_lab$labname),]
sepsis_glucose[,labresult:=as.numeric(labresult)]

sepsis_glucose<-sepsis_glucose%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=800,800,labresult)))

sepsis_glucose<-sepsis_glucose[,.(glucose_max=max(labresult,na.rm=T),glucose_min=min(labresult,na.rm=T),
                                  glucose_mean=mean(labresult,na.rm=T),glucose_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_glucose<-sepsis_glucose[is.finite(glucose_max),]
sepsis_glucose<-sepsis_glucose%>%
  mutate(glucose_vr=glucose_sd/glucose_mean)

####
sepsis_sodium<-sepsis_lab[grep('sodium',sepsis_lab$labname),]
sepsis_sodium[,labresult:=as.numeric(labresult)]

sepsis_sodium<-sepsis_sodium%>%
  mutate(labresult=ifelse(labresult<=80,80,
                          ifelse(labresult>=200,200,labresult)))

sepsis_sodium<-sepsis_sodium[,.(sodium_max=max(labresult,na.rm=T),sodium_min=min(labresult,na.rm=T),
                                sodium_mean=mean(labresult,na.rm=T),sodium_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_sodium<-sepsis_sodium[is.finite(sodium_max),]
sepsis_sodium<-sepsis_sodium%>%
  mutate(sodium_vr=sodium_sd/sodium_mean)

####
sepsis_potassium<-sepsis_lab[grep('potassium',sepsis_lab$labname),]
sepsis_potassium[,labresult:=as.numeric(labresult)]

sepsis_potassium<-sepsis_potassium%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=12,12,labresult)))

sepsis_potassium<-sepsis_potassium[,.(potassium_max=max(labresult,na.rm=T),potassium_min=min(labresult,na.rm=T),
                                      potassium_mean=mean(labresult,na.rm=T),potassium_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_potassium<-sepsis_potassium[is.finite(potassium_max),]
sepsis_potassium<-sepsis_potassium%>%
  mutate(potassium_vr=potassium_sd/potassium_mean)

##### (INR,PT,APTT)
sepsis_inr<-sepsis_lab[grep('INR',sepsis_lab$labname),]
sepsis_inr[,labresult:=as.numeric(labresult)]

sepsis_inr<-sepsis_inr%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=15,15,labresult)))

sepsis_inr<-sepsis_inr[,.(inr_max=max(labresult,na.rm=T),inr_min=min(labresult,na.rm=T),
                          inr_mean=mean(labresult,na.rm=T),inr_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_inr<-sepsis_inr[is.finite(inr_max),]
sepsis_inr<-sepsis_inr%>%
  mutate(inr_vr=inr_sd/inr_mean)

#####
sepsis_pt<-sepsis_lab[grep('PT',sepsis_lab$labname),]
sepsis_pt[,labresult:=as.numeric(labresult)]

sepsis_pt<-sepsis_pt%>%
  mutate(labresult=ifelse(labresult<=5,5,
                          ifelse(labresult>=100,100,labresult)))

sepsis_pt<-sepsis_pt[,.(pt_max=max(labresult,na.rm=T),pt_min=min(labresult,na.rm=T),
                        pt_mean=mean(labresult,na.rm=T),pt_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_pt<-sepsis_pt[is.finite(pt_max),]
sepsis_pt<-sepsis_pt%>%
  mutate(pt_vr=pt_sd/pt_mean)

###
sepsis_aptt<-sepsis_lab[grep('PTT',sepsis_lab$labname),]
sepsis_aptt[,labresult:=as.numeric(labresult)]

sepsis_aptt<-sepsis_aptt%>%
  mutate(labresult=ifelse(labresult<=20,20,
                          ifelse(labresult>=200,200,labresult)))

sepsis_aptt<-sepsis_aptt[,.(aptt_max=max(labresult,na.rm=T),aptt_min=min(labresult,na.rm=T),
                            aptt_mean=mean(labresult,na.rm=T),aptt_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_aptt<-sepsis_aptt[is.finite(aptt_max),]
sepsis_aptt<-sepsis_aptt%>%
  mutate(aptt_vr=aptt_sd/aptt_mean)

####（WBC，HCT，HB，PLT）
sepsis_hematocrit<-sepsis_lab[grep('Hct',sepsis_lab$labname),]
sepsis_hematocrit[,labresult:=as.numeric(labresult)]

sepsis_hematocrit<-sepsis_hematocrit%>%
  mutate(labresult=ifelse(labresult<=10,10,
                          ifelse(labresult>=55,55,labresult)))

sepsis_hematocrit<-sepsis_hematocrit[,.(hematocrit_max=max(labresult,na.rm=T),hematocrit_min=min(labresult,na.rm=T),
                                        hematocrit_mean=mean(labresult,na.rm=T),hematocrit_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_hematocrit<-sepsis_hematocrit[is.finite(hematocrit_max),]
sepsis_hematocrit<-sepsis_hematocrit%>%
  mutate(hematocrit_vr=hematocrit_sd/hematocrit_mean)

#######
sepsis_hemoglobin<-sepsis_lab[grep('Hgb',sepsis_lab$labname),]
sepsis_hemoglobin[,labresult:=as.numeric(labresult)]

sepsis_hemoglobin<-sepsis_hemoglobin%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=20,20,labresult)))

sepsis_hemoglobin<-sepsis_hemoglobin[,.(hemoglobin_max=max(labresult,na.rm=T),hemoglobin_min=min(labresult,na.rm=T),
                                        hemoglobin_mean=mean(labresult,na.rm=T),hemoglobin_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_hemoglobin<-sepsis_hemoglobin[is.finite(hemoglobin_max),]
sepsis_hemoglobin<-sepsis_hemoglobin%>%
  mutate(hemoglobin_vr=hemoglobin_sd/hemoglobin_mean)

#####
sepsis_platelet<-sepsis_lab[grep('platelets x 1000',sepsis_lab$labname),]
sepsis_platelet[,labresult:=as.numeric(labresult)]

sepsis_platelet<-sepsis_platelet%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=1000,1000,labresult)))

sepsis_platelet<-sepsis_platelet[,.(platelet_max=max(labresult,na.rm=T),platelet_min=min(labresult,na.rm=T),
                                    platelet_mean=mean(labresult,na.rm=T),platelet_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_platelet<-sepsis_platelet[is.finite(platelet_max),]
sepsis_platelet<-sepsis_platelet%>%
  mutate(platelet_vr=platelet_sd/platelet_mean)

####
sepsis_wbc<-sepsis_lab[grep('WBC x 1000',sepsis_lab$labname),]
sepsis_wbc[,labresult:=as.numeric(labresult)]

sepsis_wbc<-sepsis_wbc%>%
  mutate(labresult=ifelse(labresult<=0,0,
                          ifelse(labresult>=60,60,labresult)))

sepsis_wbc<-sepsis_wbc[,.(wbc_max=max(labresult,na.rm=T),wbc_min=min(labresult,na.rm=T),
                          wbc_mean=mean(labresult,na.rm=T),wbc_sd=sd(labresult,na.rm=T)),by=.(patientunitstayid)]

sepsis_wbc<-sepsis_wbc[is.finite(wbc_max),]
sepsis_wbc<-sepsis_wbc%>%
  mutate(wbc_vr=wbc_sd/wbc_mean)

#####gcs(GCS Total)
sepsis_gcs<-sepsis_vital[grep('GCS Total',sepsis_vital$nursingchartcelltypevalname),]
sepsis_gcs[,nursingchartvalue:=as.numeric(nursingchartvalue)]

sepsis_gcs<-sepsis_gcs[,.(gcs_max=max(nursingchartvalue,na.rm=T),gcs_min=min(nursingchartvalue,na.rm=T),
                            gcs_mean=mean(nursingchartvalue,na.rm=T),gcs_sd=sd(nursingchartvalue,na.rm=T)),by=.(patientunitstayid)]

sepsis_gcs<-sepsis_gcs[is.finite(gcs_max),]
sepsis_gcs<-sepsis_gcs%>%
  mutate(gcs_vr=gcs_sd/gcs_mean)

#########input and outputs
intakeOutput <- fread(input="zcat < /Volumes/F/eicu/intakeOutput.csv.gz",
                      sep = ',',header = T)

intakeOutput_1<-intakeOutput%>%
  filter(intakeoutputoffset>=0)%>%
  filter(intakeoutputoffset<=1440)%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")

summary(intakeOutput_1)

intakeOutput_2<-sqldf("select * from intakeOutput_1 where intaketotal>=0")
intakeOutput_2<-as.data.table(intakeOutput_2)

sepsis_intakeoutput<-unique(intakeOutput_2,by =c("patientunitstayid","intakeoutputoffset",
                           "intaketotal","outputtotal"))
sepsis_intakeoutput<-sepsis_intakeoutput[,.(intaketotal = sum(intaketotal,na.rm = T),
                                outputtotal=sum(outputtotal,na.rm = T),
                                nettotal = sum(nettotal,na.rm = T),
                                urineOutput =
                                  sum(outputtotal[grep("Urine",celllabel)],na.rm = T)),
                             by = .(patientunitstayid)]%>%
  select(patientunitstayid,intaketotal)%>%
  set_names("patientunitstayid","input_total")

summary(sepsis_intakeoutput$input_total)

####
uop_new<-eicu_uop%>%
  filter(chartoffset>=0)%>%
  filter(chartoffset<=1440)%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")
summary(uop_new_1)

uop_new_1<-sqldf("select * from uop_new where urineoutput>=0")

sepsis_intakeoutput<-unique(uop_new,by =c("patientunitstayid","chartoffset",
                                                 "urineoutput"))%>%
  as.data.table()

sepsis_output<-sepsis_intakeoutput[,.(urineoutput=sum(urineoutput)),by = .(patientunitstayid)]%>%
  select(patientunitstayid,urineoutput)%>%
  set_names("patientunitstayid","urine_total")


summary(sepsis_output$urine_total)


#####MV
colnames(eicu_mv)
eicu_mv_new<-eicu_mv[,c(1,8)]%>%
  filter(vent_start_first<=60*24)%>%
  mutate(mv=1)

names(eicu_mv_new)[1]<-"patientunitstayid"

sepsis_vent<-eicu_mv_new[,c(1,3)]%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")%>%
  as.data.table()

sepsis_vent$mv[which(is.na(sepsis_vent$mv))]<-0

#####CRRT
crrt<-crrt%>%
  mutate(rrt=1)
sepsis_rrt<-crrt%>%
  right_join(finalpatient_2[,c(1,2)],by="patientunitstayid")%>%
  as.data.table()

sepsis_rrt<-sepsis_rrt[is.finite(rrt),]

################infection site, race，admissiontype,bmi
colnames(finalpatient_2)
summary(finalpatient_3$bmi)
finalpatient_2<-as.data.table(finalpatient_2)

finalpatient_2[,infectionsite:=ifelse(apacheadmissiondx=="Sepsis, pulmonary",1,2)]

finalpatient_2[,admissiontype:=ifelse(unitadmitsource=="Emergency Department",1,2)]

finalpatient_2[,ethni:=ifelse(ethnicity=="Caucasian",1,2)]

sepsis_bmi<-finalpatient_2%>%
  mutate(admissionheight=ifelse(admissionheight<=100,100,
                                ifelse(admissionheight>=200,200,admissionheight)))%>%
  mutate(admissionweight=ifelse(admissionweight>=150,150,admissionweight))%>%
  mutate(bmi=admissionweight*100*100/admissionheight/admissionheight)%>%
  select(patientunitstayid,bmi)%>%
  mutate(bmi=ifelse(bmi<=10,10,
                    ifelse(bmi>=50,50,bmi)))

sepsis_bmi<-sepsis_bmi[is.finite(bmi),]



finalpatient_3<-finalpatient_2%>%
  select(patientunitstayid,age,gender,ethni,admissiontype,infectionsite,hospitalid,hospitaladmitoffset,
         hospitaldischargeoffset,hospitaldischargestatus,admissionweight,admissionheight,unitdischargeoffset,
         unitdischargestatus,nee,Group)%>%
  mutate(ethni=factor(ethni,levels = c(1,2),labels = c("white","other")))%>%
  mutate(infectionsite=factor(infectionsite,levels = c(1,2),labels = c("pulmonary","other")))%>%
  mutate(admissiontype=factor(admissiontype,levels = c(1,2),labels = c("emergency","other")))


#########
apsiii<-apachePatientResult[,.(apsiii=max(acutephysiologyscore,na.rm=T)),by=.(patientunitstayid)]




#######
finaldata_eicu<-finalpatient_3%>%
  left_join(sepsis_bmi,by="patientunitstayid")%>%
  left_join(apsiii,by="patientunitstayid")%>%
  left_join(sepsis_vent,by="patientunitstayid")%>%
  left_join(sepsis_rrt[,c(1,2)],by="patientunitstayid")%>%
  left_join(sepsis_hr,by="patientunitstayid")%>%
  left_join(sepsis_isbp,by="patientunitstayid")%>%
  left_join(sepsis_idbp,by="patientunitstayid")%>%
  left_join(sepsis_imbp,by="patientunitstayid")%>%
  left_join(sepsis_nisbp,by="patientunitstayid")%>%
  left_join(sepsis_nidbp,by="patientunitstayid")%>%
  left_join(sepsis_nimbp,by="patientunitstayid")%>%
  left_join(sepsis_rr,by="patientunitstayid")%>%
  left_join(sepsis_temp,by="patientunitstayid")%>%
  left_join(sepsis_spo2,by="patientunitstayid")%>%
  left_join(sepsis_ph,by="patientunitstayid")%>%
  left_join(sepsis_lac,by="patientunitstayid")%>%
  left_join(sepsis_po2,by="patientunitstayid")%>%
  left_join(sepsis_fio2,by="patientunitstayid")%>%
  left_join(sepsis_pco2,by="patientunitstayid")%>%
  left_join(sepsis_pf,by="patientunitstayid")%>%
  left_join(sepsis_baseexcess,by="patientunitstayid")%>%
  left_join(sepsis_alt,by="patientunitstayid")%>%
  left_join(sepsis_ast,by="patientunitstayid")%>%
  left_join(sepsis_bili,by="patientunitstayid")%>%
  left_join(sepsis_albumin,by="patientunitstayid")%>%
  left_join(sepsis_bicarbonate,by="patientunitstayid")%>%
  left_join(sepsis_bun,by="patientunitstayid")%>%
  left_join(sepsis_calcium,by="patientunitstayid")%>%
  left_join(sepsis_chloride,by="patientunitstayid")%>%
  left_join(sepsis_creatinine,by="patientunitstayid")%>%
  left_join(sepsis_glucose,by="patientunitstayid")%>%
  left_join(sepsis_sodium,by="patientunitstayid")%>%
  left_join(sepsis_potassium,by="patientunitstayid")%>%
  left_join(sepsis_inr,by="patientunitstayid")%>%
  left_join(sepsis_pt,by="patientunitstayid")%>%
  left_join(sepsis_aptt,by="patientunitstayid")%>%
  left_join(sepsis_hematocrit,by="patientunitstayid")%>%
  left_join(sepsis_hemoglobin,by="patientunitstayid")%>%
  left_join(sepsis_platelet,by="patientunitstayid")%>%
  left_join(sepsis_wbc,by="patientunitstayid")%>%
  left_join(sepsis_gcs,by="patientunitstayid")%>%
  left_join(sepsis_intakeoutput,by="patientunitstayid")%>%
  left_join(eicu_sofa[,c(1,9)],by="patientunitstayid")

finaldata_eicu$mv[which(is.na(finaldata_eicu$mv))]<-0 
finaldata_eicu$rrt[which(is.na(finaldata_eicu$rrt))]<-0 