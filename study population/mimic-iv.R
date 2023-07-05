set.seed(20220917)
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
#####1.Import Data
path <-"/Users/2022/sepsisdata/"
files<-list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}
#####2.identification of Sepsis patients
sepsis_total<-icustay%>%
  filter(icustay_seq==1)%>%
  filter(los_icu>=1)%>%
  filter(los_hospital>=los_icu)

##2.1 identification of timezero

sepsis_total_1<-sepsis_total%>%
  left_join(admissions[,c(2,5)],by='hadm_id')%>%
  mutate(sepsis_time=if_else(sofa_time<suspected_infection_time,sofa_time,suspected_infection_time))%>%
  mutate(sepsisonset_hour=difftime(sepsis_time,icu_intime,units="hours"))%>%
  mutate(sepsisonset_hour=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",sepsisonset_hour))))%>%
  filter(sepsisonset_hour>=-24)%>%
  filter(sepsisonset_hour<=24)%>%
  mutate(timezero=if_else(sepsisonset_hour<=0,icu_intime,sepsis_time))%>%
  mutate(death_time=difftime(deathtime,timezero,units="hours"))%>%
  mutate(death_time=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",death_time))))%>%
  mutate(death_time=round(death_time/24,2))%>%
  mutate(time=ifelse(death_time<=28,death_time,28))%>%
  mutate(event=ifelse(death_time<=28,1,0))

summary(sepsis_total_1$sepsisonset_hour)

sepsis_total_1$time[which(is.na(sepsis_total_1$time))]<-28
sepsis_total_1$event[which(is.na(sepsis_total_1$event))]<-0
####2.2 
sepsis_total_2<-sepsis_total_1%>%
  filter(time>=1)

summary(sepsis_total_2$event)

####3.0receiving vasopressor
colnames(vaso_vasopressor)
vaso_vasopressor<-as.data.frame(vasopressor[,-c(9,10)])%>%
  right_join(sepsis_total_2[,c(3,29)],by="stay_id")%>%
  mutate(start_time=difftime(starttime,timezero,units="hours"))%>%
  mutate(start_time=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",start_time))))%>%
  mutate(end_time=difftime(endtime,timezero,units="hours"))%>%
  mutate(end_time=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",end_time))))%>%
  dplyr::select(stay_id,start_time,end_time,dopamine,epinephrine,norepinephrine,phenylephrine,vasopressin)

##3.1 use of vasopressor
dyn_vaso<-sqldf("select T.* from (select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<0 then 0 else a1.start_time end as starttime,
                case when a1.end_time>3 then 3 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<3 and a1.end_time>=0
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<3 then 3 else a1.start_time end as starttime,
                case when a1.end_time>6 then 6 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<6 and a1.end_time>=3
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<6 then 6 else a1.start_time end as starttime,
                case when a1.end_time>12 then 12 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<12 and a1.end_time>=6
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<12 then 12 else a1.start_time end as starttime,
                case when a1.end_time>24 then 24 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<24 and a1.end_time>=12
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<24 then 24 else a1.start_time end as starttime,
                case when a1.end_time>48 then 48 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<48 and a1.end_time>=24
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<48 then 48 else a1.start_time end as starttime,
                case when a1.end_time>72 then 72 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<72 and a1.end_time>=48
                union all
                select a1.stay_id,a1.dopamine,a1.epinephrine,a1.norepinephrine,a1.phenylephrine,a1.vasopressin,
                case when a1.start_time<72 then 72 else a1.start_time end as starttime,
                case when a1.end_time>96 then 96 else a1.end_time end as endtime
                from vaso_vasopressor a1
                where a1.start_time<96 and a1.end_time>=72)T")

  

dyn_vaso<-dyn_vaso%>%
  mutate(days=ifelse(endtime<=24,1,
                     ifelse(endtime<=48,2,
                            ifelse(endtime<=72,3,
                                   ifelse(endtime<=96,4,4)))))

#####3.2nee
summary(dyn_vaso)
colnames(dyn_vaso)

dyn_vaso<-dyn_vaso%>%
  mutate(dopamine=ifelse(dopamine>=50,50,dopamine))%>%
  mutate(epinephrine=ifelse(epinephrine>=0.2,0.2,epinephrine))%>%
  mutate(norepinephrine=ifelse(norepinephrine>=5,5,norepinephrine))%>%
  mutate(phenylephrine=ifelse(phenylephrine<=0,0.1,
                              ifelse(phenylephrine>=50,50,phenylephrine)))%>%
  mutate(vasopressin=ifelse(vasopressin>=10,10,vasopressin))%>%
  mutate(vasopressin=ifelse(vasopressin<0.1,vasopressin,vasopressin/60))

dyn_vaso_1<-sqldf("select stay_id,endtime,days,ROUND(COALESCE(norepinephrine, 0)
  +COALESCE(epinephrine, 0)
  +COALESCE(phenylephrine/10, 0)
  +COALESCE(dopamine/150, 0)
                  +COALESCE(vasopressin*2.5, 0),4) as nee from dyn_vaso
                  where norepinephrine IS NOT NULL
OR epinephrine IS NOT NULL
OR phenylephrine IS NOT NULL
OR dopamine IS NOT NULL
OR vasopressin IS NOT NULL")
dyn_vaso_1<-as.data.table(dyn_vaso_1)

dyn_vaso_1<-dyn_vaso_1[,.(nee_max=max(nee,na.rm=T),nee_min=min(nee,na.rm=T),
                          nee_mean=mean(nee,na.rm=T),nee_sd=sd(nee,na.rm=T)),by=.(stay_id,days)]

dyn_vaso_1_1<-dyn_vaso_1%>%
  filter(nee_max<=5)
summary(dyn_vaso_1_1)

dyn_vaso_new<-dyn_vaso_1%>%
  filter(endtime<=24)



####3.3 nee in the first 24h
dyn_vaso_2<-dyn_vaso_1%>%
  mutate(nee_vr=nee_sd/nee_mean)%>%
  filter(days==1)
dyn_vaso_2$nee_vr[which(is.na(dyn_vaso_2$nee_vr))]<-0

sepsis_total_3<-sepsis_total_2%>%
  right_join(dyn_vaso_2[,c(1,3,4,5,7)],by="stay_id")

####3.4 rapid death
sepsis_total_4<-sepsis_total_3%>%
  filter(time<=3)%>%
  filter(event==1)%>%
  mutate(Group="Raipd_death")

##3.5 persistent ill
days<-sepsis_total_3%>%
  filter(time>3)%>%
  mutate(days=4)%>%
  select(stay_id,days)%>%
  as.data.table()

days<-days[,.(days=1:days),by=c("stay_id")]

dyn_vaso_3<-sqldf("select * from dyn_vaso_1 where stay_id in (select stay_id from sepsis_total_3)
                  and stay_id not in (select stay_id from sepsis_total_4)")

####3.6 
dyn_vaso_4<-days%>%
  left_join(dyn_vaso_3,by=c("stay_id"="stay_id","days"="days"))

###3.7 recovery
dyn_vaso_4$nee_max[which(is.na(dyn_vaso_4$nee_max))]<-0

dyn_vaso_5<-dyn_vaso_4%>%
  spread(days,nee_max)%>%
  set_names("stay_id","d1_nee","d2_nee","d3_nee","d4_nee")

dyn_vaso_6<-sqldf("select * from dyn_vaso_5 where 
                  (d2_nee =0 and d3_nee=0)
                  or (d3_nee=0 and d4_nee=0)")

sepsis_total_5<-sqldf("select * from sepsis_total_3 where stay_id in (select stay_id from dyn_vaso_6)")
sepsis_total_5$Group="recovery"

sepsis_total_6<-sqldf("select * from sepsis_total_3 where stay_id not in (select stay_id from sepsis_total_5)
                      and stay_id not in (select stay_id from sepsis_total_4)")
sepsis_total_6$Group="persistent_ill"

######3.8 date combine
new_sepsis<-rbind(sepsis_total_4,sepsis_total_5,sepsis_total_6)

####4.0 date extraction
##4.1 race
new_sepsis_1<-sqldf("select new_sepsis.*,case when race like '%ASIAN%' then 'ASIAN'
                    when race like '%WHITE%' then 'WHITE'
                    when race like '%HISPANIC%' then 'HISPANIC'
                    when race like '%BLACK%' then 'BLACK'
                    else 'other' end as ethni
                    from new_sepsis")
new_sepsis_1$ethni<-factor(new_sepsis_1$ethni)
summary(new_sepsis_1$ethni)

#4.2admissiontype and infectionsite
colnames(new_sepsis_1)
new_sepsis_2<-new_sepsis_1%>%
  left_join(admissions[,c(2,6)],by="hadm_id")%>%
  left_join(infection_site[,c(3,26:35)],by="stay_id")
new_sepsis_2<-new_sepsis_2%>%
  mutate(infectionsite=ifelse(lung_infection==1,1,2))
new_sepsis_2$infectionsite<-factor(new_sepsis_2$infectionsite)
new_sepsis_2$infectionsite[which(is.na(new_sepsis_2$infectionsite))]<-2

##admissiontype
new_sepsis_2$admission_type<-factor(new_sepsis_2$admission_type)
summary(new_sepsis_3$admissiontype)
new_sepsis_3<-sqldf("select new_sepsis_2.*,case when admission_type like '%EMER%' then 'emergency'
                    when admission_type like '%ELECTIVE%' then 'elective'
                    when  admission_type like '%URGENT%' then 'urgent'
                    else 'other' end as admissiontype
                    from new_sepsis_2")
new_sepsis_3$admissiontype<-factor(new_sepsis_3$admissiontype)

######4.1 antibiotic within 1 hour
colnames(new_sepsis_4)
new_sepsis_4<-new_sepsis_3%>%
  mutate(antibiotictime=difftime(antibiotic_time,timezero,units="hours"))%>%
  mutate(antibiotictime=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",antibiotictime))))%>%
  mutate(early_antibiotic=ifelse(antibiotictime<=1,1,0))%>%
  dplyr::select(subject_id,hadm_id,stay_id,gender,admission_age,ethni,admissiontype,
                hospital_expire_flag,los_hospital,los_icu,sofa_score,respiration,coagulation,
                liver,cardiovascular,cns,renal,deathtime,death_time,timezero,time,
                event,nee_max,nee_min,nee_mean,nee_vr,Group,infectionsite,early_antibiotic)

###4.2 mv
vent<-ventilation%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(vent_start=difftime(starttime,timezero,units="hours"))%>%
  mutate(vent_start=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",vent_start))))%>%
  mutate(vent_end=difftime(endtime,timezero,units="hours"))%>%
  mutate(vent_end=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",vent_end))))

vent_1<-sqldf("select T.* from (select a1.stay_id,a1.ventilation_status,
case when a1.vent_start<0 then 0 else a1.vent_start end as start_time,
                case when a1.vent_end>24 then 24 else a1.vent_end end as end_time
                from vent a1
                where a1.vent_start<24 and a1.vent_end>=0)T")
vent_1$ventilation_status<-factor(vent_1$ventilation_status,levels = c("None","SupplementalOxygen",
                                                                       "Tracheostomy","HFNC","NonInvasiveVent",
                                                                       "InvasiveVent"))
vent_2<-vent_1%>%
  mutate(ventilation_status=ifelse(ventilation_status=="InvasiveVent",1,
                                   ifelse(ventilation_status=="NonInvasiveVent",2,
                                          ifelse(ventilation_status=="HFNC",3,4))))
vent_3<-sqldf("select stay_id, min(ventilation_status) as ventilation_status from vent_2
              group by stay_id")
vent_3<-vent_3%>%
  mutate(invasVent=ifelse(ventilation_status==1,1,0))


###4.3 CRRT
sepsis_rrt<-crrt%>%
  filter(dialysis_type%in%(c('CVVH','CVVHD','CVVHDF','SCUF',"IHD")))%>%
  select(stay_id,charttime,dialysis_present,dialysis_active)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(rrtdays=difftime(charttime,timezero,units="hours"))%>%
  mutate(rrtdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",rrtdays))))%>%
  filter(rrtdays>=0)%>%
  filter(rrtdays<=24)

sepsis_rrt_1<-sqldf("select stay_id, max(dialysis_present) as rrt_firstday from sepsis_rrt
              group by stay_id")

##4.4 Invasive-SBP,DBP,MAP,Non-invasive SBP,DBP,MAP,
##Temp,Respir rate,Heartrate,SPO2
colnames(vitalsigns)
sepsis_vital<-vitalsigns%>%
  select(stay_id,charttime,heart_rate,sbp,dbp,mbp,sbp_ni,dbp_ni,mbp_ni,resp_rate,temperature,spo2)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(vitaldays=difftime(charttime,timezero,units="hours"))%>%
  mutate(vitaldays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",vitaldays))))%>%
  filter(vitaldays>=-6)%>%
  filter(vitaldays<=24)%>%
  as.data.table()
summary(sepsis_vital)

sepsis_vital<-sepsis_vital%>%
  mutate(heart_rate=ifelse(heart_rate<=30,30,
                           ifelse(heart_rate>=200,200,heart_rate)))%>%
  mutate(sbp=ifelse(sbp<=30,30,ifelse(sbp>=250,250,sbp)))%>%
  mutate(dbp=ifelse(dbp<=30,30,ifelse(dbp>=150,150,dbp)))%>%
  mutate(mbp=ifelse(mbp<=30,30,ifelse(mbp>=150,150,mbp)))%>%
  mutate(sbp_ni=ifelse(sbp_ni<=30,30,ifelse(sbp_ni>=250,250,sbp_ni)))%>%
  mutate(dbp_ni=ifelse(dbp_ni<=30,30,ifelse(dbp_ni>=150,150,dbp_ni)))%>%
  mutate(mbp_ni=ifelse(mbp_ni<=30,30,ifelse(mbp_ni>=150,150,mbp_ni)))%>%
  mutate(resp_rate=ifelse(resp_rate>=60,60,resp_rate))%>%
  mutate(spo2=ifelse(spo2<=30,30,spo2))

#######
summary(sepsis_hr)
sepsis_hr<-sepsis_vital[,.(heartrate_max=max(heart_rate,na.rm=T),heartrate_min=min(heart_rate,na.rm=T),
                           heartrate_mean=mean(heart_rate,na.rm=T),heartrate_sd=sd(heart_rate,na.rm=T)),by=.(stay_id)]
sepsis_hr<-sepsis_hr%>%
  mutate(heartrate_vr=heartrate_sd/heartrate_mean)
########
sepsis_isbp<-sepsis_vital[,.(isbp_max=max(sbp,na.rm=T),isbp_min=min(sbp,na.rm=T),
                             isbp_mean=mean(sbp,na.rm=T),isbp_sd=sd(sbp,na.rm=T)),by=.(stay_id)]
sepsis_isbp<-sepsis_isbp[is.finite(isbp_max),]
sepsis_isbp<-sepsis_isbp%>%
  mutate(isbp_vr=isbp_sd/isbp_mean)
#######
sepsis_idbp<-sepsis_vital[,.(idbp_max=max(dbp,na.rm=T),idbp_min=min(dbp,na.rm=T),
                             idbp_mean=mean(dbp,na.rm=T),idbp_sd=sd(dbp,na.rm=T)),by=.(stay_id)]
sepsis_idbp<-sepsis_idbp[is.finite(idbp_max),]
sepsis_idbp<-sepsis_idbp%>%
  mutate(idbp_vr=idbp_sd/idbp_mean)

#######
sepsis_imbp<-sepsis_vital[,.(imbp_max=max(mbp,na.rm=T),imbp_min=min(mbp,na.rm=T),
                             imbp_mean=mean(mbp,na.rm=T),imbp_sd=sd(mbp,na.rm=T)),by=.(stay_id)]
sepsis_imbp<-sepsis_imbp[is.finite(imbp_max),]

sepsis_imbp<-sepsis_imbp%>%
  mutate(imbp_vr=imbp_sd/imbp_mean)
########
sepsis_nisbp<-sepsis_vital[,.(nisbp_max=max(sbp_ni,na.rm=T),nisbp_min=min(sbp_ni,na.rm=T),
                              nisbp_mean=mean(sbp_ni,na.rm=T),nisbp_sd=sd(sbp_ni,na.rm=T)),by=.(stay_id)]
sepsis_nisbp<-sepsis_nisbp[is.finite(nisbp_max),]

sepsis_nisbp<-sepsis_nisbp%>%
  mutate(nisbp_vr=nisbp_sd/nisbp_mean)

######
sepsis_nidbp<-sepsis_vital[,.(nidbp_max=max(dbp_ni,na.rm=T),nidbp_min=min(dbp_ni,na.rm=T),
                              nidbp_mean=mean(dbp_ni,na.rm=T),nidbp_sd=sd(dbp_ni,na.rm=T)),by=.(stay_id)]
sepsis_nidbp<-sepsis_nidbp[is.finite(nidbp_max),]

sepsis_nidbp<-sepsis_nidbp%>%
  mutate(nidbp_vr=nidbp_sd/nidbp_mean)

######
sepsis_nimbp<-sepsis_vital[,.(nimbp_max=max(mbp_ni,na.rm=T),nimbp_min=min(mbp_ni,na.rm=T),
                              nimbp_mean=mean(mbp_ni,na.rm=T),nimbp_sd=sd(mbp_ni,na.rm=T)),by=.(stay_id)]
sepsis_nimbp<-sepsis_nimbp[is.finite(nimbp_max),]

sepsis_nimbp<-sepsis_nimbp%>%
  mutate(nimbp_vr=nimbp_sd/nimbp_mean)

#####
sepsis_rr<-sepsis_vital[,.(resprate_max=max(resp_rate,na.rm=T),resprate_min=min(resp_rate,na.rm=T),
                           resprate_mean=mean(resp_rate,na.rm=T),resprate_sd=sd(resp_rate,na.rm=T)),by=.(stay_id)]
sepsis_rr<-sepsis_rr[is.finite(resprate_max),]

sepsis_rr<-sepsis_rr%>%
  mutate(rr_vr=resprate_sd/resprate_mean)

#####
sepsis_temp<-sepsis_vital[,.(temp_max=max(temperature,na.rm=T),temp_min=min(temperature,na.rm=T),
                             temp_mean=mean(temperature,na.rm=T),temp_sd=sd(temperature,na.rm=T)),by=.(stay_id)]
sepsis_temp<-sepsis_temp[is.finite(temp_max),]

sepsis_temp<-sepsis_temp%>%
  mutate(temp_vr=temp_sd/temp_mean)

#####
sepsis_spo2<-sepsis_vital[,.(spo2_max=max(spo2,na.rm=T),spo2_min=min(spo2,na.rm=T),
                             spo2_mean=mean(spo2,na.rm=T),spo2_sd=sd(spo2,na.rm=T)),by=.(stay_id)]
sepsis_spo2<-sepsis_spo2[is.finite(spo2_max),]

sepsis_spo2<-sepsis_spo2%>%
  mutate(spo2_vr=spo2_sd/spo2_mean)

#####4.5 abg（pH, lac,HCO3-,BE,PO2,PCO2,PF ratio,FiO2）
summary(bg_art)
str(new_sepsis_4$timezero)
sepsis_bg<-bg_art%>%
  select(stay_id,charttime,po2,pco2,pao2fio2ratio,ph,baseexcess,lactate,fio2_chartevents)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(bgdays=difftime(charttime,timezero,units="hours"))%>%
  mutate(bgdays=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",bgdays))))%>%
  filter(bgdays>=-24)%>%
  filter(bgdays<=24)%>%
  as.data.table()

sepsis_bg<-sepsis_bg%>%
  mutate(po2=ifelse(po2<=30,30,
                    ifelse(po2>=500,500,po2)))%>%
  mutate(pco2=ifelse(pco2<=10,10,
                     ifelse(pco2>=150,150,pco2)))%>%
  mutate(pao2fio2ratio=ifelse(pao2fio2ratio<=30,30,
                     ifelse(pao2fio2ratio>=650,650,pao2fio2ratio)))%>%
  mutate(baseexcess=ifelse(baseexcess<=-30,-30,
                           ifelse(baseexcess>=30,30,baseexcess)))%>%
  mutate(fio2_chartevents=ifelse(fio2_chartevents<=21,21,
                                 ifelse(fio2_chartevents>=100,100,fio2_chartevents)))
  

#####
sepsis_ph<-sepsis_bg[,.(ph_max=max(ph,na.rm=T),ph_min=min(ph,na.rm=T),
                        ph_mean=mean(ph,na.rm=T),ph_sd=sd(ph,na.rm=T)),by=.(stay_id)]

sepsis_ph<-sepsis_ph%>%
  mutate(ph_vr=ph_sd/ph_mean)
#####
sepsis_lac<-sepsis_bg[,.(lac_max=max(lactate,na.rm=T),lac_min=min(lactate,na.rm=T),
                        lac_mean=mean(lactate,na.rm=T),lac_sd=sd(lactate,na.rm=T)),by=.(stay_id)]
sepsis_lac<-sepsis_lac[is.finite(lac_max),]
sepsis_lac<-sepsis_lac%>%
  mutate(lac_vr=lac_sd/lac_mean)

####
sepsis_po2<-sepsis_bg[,.(po2_max=max(po2,na.rm=T),po2_min=min(po2,na.rm=T),
                         po2_mean=mean(po2,na.rm=T),po2_sd=sd(po2,na.rm=T)),by=.(stay_id)]
summary(sepsis_po2)
sepsis_po2<-sepsis_po2%>%
  mutate(po2_vr=po2_sd/po2_mean)

####
sepsis_pco2<-sepsis_bg[,.(pco2_max=max(pco2,na.rm=T),pco2_min=min(pco2,na.rm=T),
                         pco2_mean=mean(pco2,na.rm=T),pco2_sd=sd(pco2,na.rm=T)),by=.(stay_id)]

sepsis_pco2<-sepsis_pco2%>%
  mutate(pco2_vr=pco2_sd/pco2_mean)
####
sepsis_fio2<-sepsis_bg[,.(fio2_max=max(fio2_chartevents,na.rm=T),fio2_min=min(fio2_chartevents,na.rm=T),
                          fio2_mean=mean(fio2_chartevents,na.rm=T),fio2_sd=sd(fio2_chartevents,na.rm=T)),by=.(stay_id)]
sepsis_fio2<-sepsis_fio2[is.finite(fio2_max),]
sepsis_fio2<-sepsis_fio2%>%
  mutate(fio2_vr=fio2_sd/fio2_mean)


####
sepsis_pf<-sepsis_bg[,.(pf_max=max(pao2fio2ratio,na.rm=T),pf_min=min(pao2fio2ratio,na.rm=T),
                         pf_mean=mean(pao2fio2ratio,na.rm=T),pf_sd=sd(pao2fio2ratio,na.rm=T)),by=.(stay_id)]
sepsis_pf<-sepsis_pf[is.finite(pf_max),]
sepsis_pf<-sepsis_pf%>%
  mutate(pf_vr=pf_sd/pf_mean)

####
sepsis_baseexcess<-sepsis_bg[,.(baseexcess_max=max(baseexcess,na.rm=T),baseexcess_min=min(baseexcess,na.rm=T),
                               baseexcess_mean=mean(baseexcess,na.rm=T),baseexcess_sd=sd(baseexcess,na.rm=T)),by=.(stay_id)]

sepsis_baseexcess<-sepsis_baseexcess%>%
  mutate(baseexcess_vr=baseexcess_sd/baseexcess_mean)

#####4.6 heart rate
colnames(new_sepsis_4)
sepsis_rhy<-sepsis_rhythm%>%
  select(subject_id,charttime,heart_rhythm)%>%
  right_join(new_sepsis_4[,c(1,20)],by="subject_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=0)%>%
  filter(days<=24)%>%
  as.data.table()


sepsis_rhy_1<-sqldf("select distinct subject_id from sepsis_rhy where 
                    heart_rhythm in ('VT (Ventricular Tachycardia)','VF (Ventricular Fibrillation)',
                    'Asystole','JR (Junctional Rhythm)','RBBB (Right Bundle Branch Block)',
                    '3rd AV (Complete Heart Block)','SVT (Supra Ventricular Tachycardia)',
                    'LBBB (Left Bundle Branch Block)',
                    'JT (Junctional Tachycardia)',
                    'A Flut (Atrial Flutter)','AF (Atrial Fibrillation)','1st AV (First degree AV Block)',
                    '2nd AV M2 (Second degree AV Block - Mobitz 2)',
                    'WAP (Wandering atrial pacemaker)',
                    '2nd AV W-M1 (Second degree AV Block Wenckebach - Mobitz1)',
                    'PAT (Paroxysmal Atrial Tachycardia)',
                    'PJT (Paroxysmal Junctional Tachycardia)',
                    'MAT (Multifocal atrial tachycardia)','Idioventricular')")

sepsis_rhy_1$heart_rtythm<-1

######4.7 （ALP,ALT,AST,LDH,TBIL）
str(sepsis_enzyme$charttime)
str(new_sepsis_4$timezero)
sepsis_enzyme<-enzyme%>%
  select(stay_id,charttime,alt,alp,ast,bilirubin_total,ld_ldh)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-12)%>%
  filter(days<=48)%>%
  as.data.table()

summary(sepsis_enzyme)
sepsis_enzyme<-sepsis_enzyme%>%
  mutate(alt=ifelse(alt>=3000,3000,alt))%>%
  mutate(ast=ifelse(ast>=3000,3000,ast))%>%
  mutate(alp=ifelse(alp>=3000,3000,alp))%>%
  mutate(ld_ldh=ifelse(ld_ldh>=5000,5000,ld_ldh))%>%
  mutate(bilirubin_total=ifelse(bilirubin_total>=20,20,bilirubin_total))
  


#####
sepsis_alt<-sepsis_enzyme[,.(alt_max=max(alt,na.rm=T),alt_min=min(alt,na.rm=T),
                        alt_mean=mean(alt,na.rm=T),alt_sd=sd(alt,na.rm=T)),by=.(stay_id)]
sepsis_alt<-sepsis_alt[is.finite(alt_max),]
sepsis_alt<-sepsis_alt%>%
  mutate(alt_vr=alt_sd/alt_mean)

#####
sepsis_alp<-sepsis_enzyme[,.(alp_max=max(alp,na.rm=T),alp_min=min(alp,na.rm=T),
                             alp_mean=mean(alp,na.rm=T),alp_sd=sd(alp,na.rm=T)),by=.(stay_id)]
sepsis_alp<-sepsis_alp[is.finite(alp_max),]
sepsis_alp<-sepsis_alp%>%
  mutate(alp_vr=alp_sd/alp_mean)

#####
sepsis_ast<-sepsis_enzyme[,.(ast_max=max(ast,na.rm=T),ast_min=min(ast,na.rm=T),
                             ast_mean=mean(ast,na.rm=T),ast_sd=sd(ast,na.rm=T)),by=.(stay_id)]
sepsis_ast<-sepsis_ast[is.finite(ast_max),]
sepsis_ast<-sepsis_ast%>%
  mutate(ast_vr=ast_sd/ast_mean)

#####
sepsis_bili<-sepsis_enzyme[,.(bili_max=max(bilirubin_total,na.rm=T),bili_min=min(bilirubin_total,na.rm=T),
                              bili_mean=mean(bilirubin_total,na.rm=T),bili_sd=sd(bilirubin_total,na.rm=T)),by=.(stay_id)]
sepsis_bili<-sepsis_bili[is.finite(bili_max),]
sepsis_bili<-sepsis_bili%>%
  mutate(bili_vr=bili_sd/bili_mean)

#####
sepsis_ldh<-sepsis_enzyme[,.(ldh_max=max(ld_ldh,na.rm=T),ldh_min=min(ld_ldh,na.rm=T),
                              ldh_mean=mean(ld_ldh,na.rm=T),ldh_sd=sd(ld_ldh,na.rm=T)),by=.(stay_id)]
sepsis_ldh<-sepsis_ldh[is.finite(ldh_max),]
sepsis_ldh<-sepsis_ldh%>%
  mutate(ldh_vr=ldh_sd/ldh_mean)

############4.8（albumin,bun,calcium,chloride,creatinine,glucose,sodium,potassium）
summary(chmeistry)
sepsis_chmeistry<-chmeistry%>%
  select(stay_id,charttime,albumin,bicarbonate,bun,calcium,chloride,creatinine,glucose,sodium,potassium)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-6)%>%
  filter(days<=24)%>%
  as.data.table()

sepsis_chmeistry<-sepsis_chmeistry%>%
  mutate(albumin=ifelse(albumin>=7,7,albumin))%>%
  mutate(bicarbonate=ifelse(bicarbonate<=5,5,
                            ifelse(bicarbonate>=50,50,bicarbonate)))%>%
  mutate(bun=ifelse(bun>=115,115,bun))%>%
  mutate(calcium=ifelse(calcium>=50,50,calcium))%>%
  mutate(creatinine=ifelse(creatinine>=8,8,creatinine))%>%
  mutate(glucose=ifelse(glucose>=800,800,glucose))%>%
  mutate(sodium=ifelse(sodium<=80,80,
                       ifelse(sodium>=200,200,sodium)))%>%
  mutate(potassium=ifelse(potassium>=12,12,potassium))
  


#####
sepsis_albumin<-sepsis_chmeistry[,.(albumin_max=max(albumin,na.rm=T),albumin_min=min(albumin,na.rm=T),
                                    albumin_mean=mean(albumin,na.rm=T),albumin_sd=sd(albumin,na.rm=T)),by=.(stay_id)]
sepsis_albumin<-sepsis_albumin[is.finite(albumin_max),]
sepsis_albumin<-sepsis_albumin%>%
  mutate(albumin_vr=albumin_sd/albumin_mean)

#####
sepsis_bicarbonate<-sepsis_chmeistry[,.(bicarbonate_max=max(bicarbonate,na.rm=T),bicarbonate_min=min(bicarbonate,na.rm=T),
                                        bicarbonate_mean=mean(bicarbonate,na.rm=T),bicarbonate_sd=sd(bicarbonate,na.rm=T)),by=.(stay_id)]
sepsis_bicarbonate<-sepsis_bicarbonate[is.finite(bicarbonate_max),]
sepsis_bicarbonate<-sepsis_bicarbonate%>%
  mutate(bicarbonate_vr=bicarbonate_sd/bicarbonate_mean)

#####
sepsis_bun<-sepsis_chmeistry[,.(bun_max=max(bun,na.rm=T),bun_min=min(bun,na.rm=T),
                                bun_mean=mean(bun,na.rm=T),bun_sd=sd(bun,na.rm=T)),by=.(stay_id)]
sepsis_bun<-sepsis_bun[is.finite(bun_max),]
sepsis_bun<-sepsis_bun%>%
  mutate(bun_vr=bun_sd/bun_mean)

#####
sepsis_calcium<-sepsis_chmeistry[,.(calcium_max=max(calcium,na.rm=T),calcium_min=min(calcium,na.rm=T),
                                    calcium_mean=mean(calcium,na.rm=T),calcium_sd=sd(calcium,na.rm=T)),by=.(stay_id)]
sepsis_calcium<-sepsis_calcium[is.finite(calcium_max),]
sepsis_calcium<-sepsis_calcium%>%
  mutate(calcium_vr=calcium_sd/calcium_mean)

#####
sepsis_chloride<-sepsis_chmeistry[,.(chloride_max=max(chloride,na.rm=T),chloride_min=min(chloride,na.rm=T),
                                     chloride_mean=mean(chloride,na.rm=T),chloride_sd=sd(chloride,na.rm=T)),by=.(stay_id)]
sepsis_chloride<-sepsis_chloride[is.finite(chloride_max),]
sepsis_chloride<-sepsis_chloride%>%
  mutate(chloride_vr=chloride_sd/chloride_mean)

#####
sepsis_creatinine<-sepsis_chmeistry[,.(creatinine_max=max(creatinine,na.rm=T),creatinine_min=min(creatinine,na.rm=T),
                                       creatinine_mean=mean(creatinine,na.rm=T),creatinine_sd=sd(creatinine,na.rm=T)),by=.(stay_id)]
sepsis_creatinine<-sepsis_creatinine[is.finite(creatinine_max),]
sepsis_creatinine<-sepsis_creatinine%>%
  mutate(creatinine_vr=creatinine_sd/creatinine_mean)

#####
sepsis_glucose<-sepsis_chmeistry[,.(glucose_max=max(glucose,na.rm=T),glucose_min=min(glucose,na.rm=T),
                                    glucose_mean=mean(glucose,na.rm=T),glucose_sd=sd(glucose,na.rm=T)),by=.(stay_id)]
sepsis_glucose<-sepsis_glucose[is.finite(glucose_max),]
sepsis_glucose<-sepsis_glucose%>%
  mutate(glucose_vr=glucose_sd/glucose_mean)

####
sepsis_sodium<-sepsis_chmeistry[,.(sodium_max=max(sodium,na.rm=T),sodium_min=min(sodium,na.rm=T),
                                   sodium_mean=mean(sodium,na.rm=T),sodium_sd=sd(sodium,na.rm=T)),by=.(stay_id)]
sepsis_sodium<-sepsis_sodium[is.finite(sodium_max),]
sepsis_sodium<-sepsis_sodium%>%
  mutate(sodium_vr=sodium_sd/sodium_mean)

####
sepsis_potassium<-sepsis_chmeistry[,.(potassium_max=max(potassium,na.rm=T),potassium_min=min(potassium,na.rm=T),
                                      potassium_mean=mean(potassium,na.rm=T),potassium_sd=sd(potassium,na.rm=T)),by=.(stay_id)]
sepsis_potassium<-sepsis_potassium[is.finite(potassium_max),]
sepsis_potassium<-sepsis_potassium%>%
  mutate(potassium_vr=potassium_sd/potassium_mean)

#######4.9 (INR,PT,APTT)
summary(coagulation)
sepsis_coagulation<-coagulation%>%
  select(stay_id,charttime,inr,pt,ptt)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-6)%>%
  filter(days<=24)%>%
  as.data.table()

sepsis_coagulation<-sepsis_coagulation%>%
  mutate(inr=ifelse(inr>=15,15,inr))%>%
  mutate(ptt[ifelse(ptt<=20,20,ptt)])%>%
  mutate(pt=ifelse(pt<=5,5,
                   ifelse(pt>=100,100,pt)))

###
sepsis_inr<-sepsis_coagulation[,.(inr_max=max(inr,na.rm=T),inr_min=min(inr,na.rm=T),
                                      inr_mean=mean(inr,na.rm=T),inr_sd=sd(inr,na.rm=T)),by=.(stay_id)]
sepsis_inr<-sepsis_inr[is.finite(inr_max),]
sepsis_inr<-sepsis_inr%>%
  mutate(inr_vr=inr_sd/inr_mean)

###
sepsis_pt<-sepsis_coagulation[,.(pt_max=max(pt,na.rm=T),pt_min=min(pt,na.rm=T),
                                  pt_mean=mean(pt,na.rm=T),pt_sd=sd(pt,na.rm=T)),by=.(stay_id)]
sepsis_pt<-sepsis_pt[is.finite(pt_max),]
sepsis_pt<-sepsis_pt%>%
  mutate(pt_vr=pt_sd/pt_mean)

###
sepsis_aptt<-sepsis_coagulation[,.(ptt_max=max(ptt,na.rm=T),ptt_min=min(ptt,na.rm=T),
                                 ptt_mean=mean(ptt,na.rm=T),ptt_sd=sd(ptt,na.rm=T)),by=.(stay_id)]
sepsis_aptt<-sepsis_aptt[is.finite(ptt_max),]
sepsis_aptt<-sepsis_aptt%>%
  mutate(ptt_vr=ptt_sd/ptt_mean)

#####4.10 （WBC，HCT，HB，PLT）
summary(sepsis_cbc)
sepsis_cbc<-complete_blood_count%>%
  select(stay_id,charttime,hematocrit,hemoglobin,platelet,wbc,mcv,rdw)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-6)%>%
  filter(days<=24)%>%
  as.data.table()

sepsis_cbc<-sepsis_cbc%>%
  mutate(hematocrit=ifelse(hematocrit<=10,10,
                           ifelse(hematocrit>=55,55,hematocrit)))%>%
  mutate(platelet>=1000,1000,platelet)%>%
  mutate(wbc=ifelse(wbc>=60,60,wbc))
###
sepsis_hematocrit<-sepsis_cbc[,.(hematocrit_max=max(hematocrit,na.rm=T),hematocrit_min=min(hematocrit,na.rm=T),
                          hematocrit_mean=mean(hematocrit,na.rm=T),hematocrit_sd=sd(hematocrit,na.rm=T)),by=.(stay_id)]
sepsis_hematocrit<-sepsis_hematocrit[is.finite(hematocrit_max),]
sepsis_hematocrit<-sepsis_hematocrit%>%
  mutate(hematocrit_vr=hematocrit_sd/hematocrit_mean)

####
sepsis_hemoglobin<-sepsis_cbc[,.(hemoglobin_max=max(hemoglobin,na.rm=T),hemoglobin_min=min(hemoglobin,na.rm=T),
                                 hemoglobin_mean=mean(hemoglobin,na.rm=T),hemoglobin_sd=sd(hemoglobin,na.rm=T)),by=.(stay_id)]
sepsis_hemoglobin<-sepsis_hemoglobin[is.finite(hemoglobin_max),]
sepsis_hemoglobin<-sepsis_hemoglobin%>%
  mutate(hemoglobin_vr=hemoglobin_sd/hemoglobin_mean)

####
sepsis_platelet<-sepsis_cbc[,.(platelet_max=max(platelet,na.rm=T),platelet_min=min(platelet,na.rm=T),
                               platelet_mean=mean(platelet,na.rm=T),platelet_sd=sd(platelet,na.rm=T)),by=.(stay_id)]
sepsis_platelet<-sepsis_platelet[is.finite(platelet_max),]
sepsis_platelet<-sepsis_platelet%>%
  mutate(platelet_vr=platelet_sd/platelet_mean)

####
sepsis_wbc<-sepsis_cbc[,.(wbc_max=max(wbc,na.rm=T),wbc_min=min(wbc,na.rm=T),
                               wbc_mean=mean(wbc,na.rm=T),wbc_sd=sd(wbc,na.rm=T)),by=.(stay_id)]
sepsis_wbc<-sepsis_wbc[is.finite(wbc_max),]
sepsis_wbc<-sepsis_wbc%>%
  mutate(wbc_vr=wbc_sd/wbc_mean)

####
sepsis_mcv<-sepsis_cbc[,.(mcv_max=max(mcv,na.rm=T),mcv_min=min(mcv,na.rm=T),
                          mcv_mean=mean(mcv,na.rm=T)),by=.(stay_id)]
sepsis_mcv<-sepsis_mcv[is.finite(mcv_max),]

####
sepsis_rdw<-sepsis_cbc[,.(rdw_max=max(rdw,na.rm=T),rdw_min=min(rdw,na.rm=T),
                          rdw_mean=mean(rdw,na.rm=T)),by=.(stay_id)]
sepsis_rdw<-sepsis_rdw[is.finite(rdw_max),]


#########4.11 GCS
colnames(gcs)
sepsis_gcs<-gcs%>%
  select(stay_id,charttime,gcs)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-6)%>%
  filter(days<=24)%>%
  as.data.table()

sepsis_gcs<-sepsis_gcs[,.(gcs_max=max(gcs,na.rm=T),gcs_min=min(gcs,na.rm=T),
                          gcs_mean=mean(gcs,na.rm=T),gcs_sd=sd(gcs,na.rm=T)),by=.(stay_id)]
sepsis_gcs<-sepsis_gcs%>%
  mutate(gcs_vr=gcs_sd/gcs_mean)

#######4.12 inputs
colnames(inputevents)
inputtype<-sqldf("select distinct ordercategoryname from inputevents")


sepsis_inputs<-inputevents%>%
  select(stay_id,starttime,endtime,amount,amountuom,rate,rateuom,ordercategoryname,ordercategorydescription)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(start=difftime(starttime,timezero,units="hours"))%>%
  mutate(start=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",start))))%>%
  mutate(end=difftime(endtime,timezero,units="hours"))%>%
  mutate(end=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",end))))%>%
  as.data.table()

colnames(sepsis_inputs)
sepsis_inputs_1<-sqldf("select T.* from (select a1.stay_id,a1.amount,a1.amountuom,a1.rate,a1.rateuom,a1.ordercategoryname,a1.ordercategorydescription,
                case when a1.start<0 then 0 else a1.start end as start_time,
                case when a1.end>24 then 24 else a1.end end as end_time
                from sepsis_inputs a1
                where a1.start<24 and a1.end>=0)T")


sepsis_inputs_2<-sqldf("select * from sepsis_inputs_1
                       where rateuom in ('mL/min','mL/hour')")

sepsis_inputs_3<-sepsis_inputs_2%>%
  mutate(amount=ifelse(amountuom=="L",amount*1000,amount))%>%
  mutate(rate=ifelse(rateuom=="mL/min",rate*60,rate))%>%
  mutate(amount=ifelse(ordercategorydescription=="Continuous Med",rate*(end_time-start_time),
                       ifelse(ordercategorydescription=="Continuous IV",rate*(end_time-start_time),amount)))%>%
  as.data.table()

######
sepsis_dailyinputs<-sepsis_inputs_3[,.(input_total=sum(amount,na.rm=T)),by=.(stay_id)]

###
sepsis_inputrate<-sepsis_inputs_3[,.(input_rate=max(rate,na.rm=T)),by=.(stay_id)]
summary(sepsis_inputrate)

#######4.13 uop
colnames(urine_rate)
summary(urine_rate$weight)
sepsis_uorate<-urine_rate%>%
  select(stay_id,charttime,weight,urineoutput_24hr,uo_tm_24hr)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(days=difftime(charttime,timezero,units="hours"))%>%
  mutate(days=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",days))))%>%
  filter(days>=-6)%>%
  filter(days<=24)%>%
  mutate(uo_mlkghr_24hr=urineoutput_24hr/uo_tm_24hr/weight)%>%
  filter(uo_mlkghr_24hr>=0)%>%
  filter(uo_mlkghr_24hr<20)%>%
  as.data.table()

sepsis_uorate<-sepsis_uorate[,.(uorate_max=max(uo_mlkghr_24hr,na.rm=T),uorate_min=min(uo_mlkghr_24hr,na.rm=T),
                                uorate_mean=mean(uo_mlkghr_24hr,na.rm=T),uorate_sd=sd(uo_mlkghr_24hr,na.rm=T)),by=.(stay_id)]
sepsis_uorate<-sepsis_uorate[is.finite(uorate_max),]
sepsis_uorate<-sepsis_uorate%>%
  mutate(uorate_vr=uorate_sd/uorate_mean)

######uop
sepsis_urine<-urine_output%>%
  select(stay_id,charttime,urineoutput)%>%
  right_join(new_sepsis_4[,c(3,20)],by="stay_id")%>%
  mutate(start=difftime(charttime,timezero,units="hours"))%>%
  mutate(start=as.numeric(as.numeric(gsub(pattern = "hours",replacement = "",start))))%>%
  filter(start>=0)%>%
  filter(start<=24)%>%
  as.data.table()

sepsis_urine<-sepsis_urine[,.(urine_total=sum(urineoutput,na.rm=T)),by=.(stay_id)]
sepsis_urine<-sepsis_urine[is.finite(urine_total),]




##############total data
summary(new_sepsis_4$admissiontype)
new_sepsis_4$admissiontype<-factor(new_sepsis_4$admissiontype)
finaldata<-new_sepsis_4%>%
  left_join(first_day_height[,c(2,3)],by="stay_id")%>%
  left_join(first_day_weight[,c(2,3)],by="stay_id")%>%
  mutate(bmi=weight_admit*100*100/height/height)%>%
  left_join(charlson[,c(2,21)],by="hadm_id")%>%
  left_join(apsiii[,c(3,4)],by="stay_id")%>%
  left_join(vent_3[,c(1,3)],by="stay_id")%>%
  left_join(sepsis_rrt_1,by="stay_id")%>%
  left_join(sepsis_hr,by="stay_id")%>%
  left_join(sepsis_isbp,by="stay_id")%>%
  left_join(sepsis_idbp,by="stay_id")%>%
  left_join(sepsis_imbp,by="stay_id")%>%
  left_join(sepsis_nisbp,by="stay_id")%>%
  left_join(sepsis_nidbp,by="stay_id")%>%
  left_join(sepsis_nimbp,by="stay_id")%>%
  left_join(sepsis_rr,by="stay_id")%>%
  left_join(sepsis_temp,by="stay_id")%>%
  left_join(sepsis_spo2,by="stay_id")%>%
  left_join(sepsis_ph,by="stay_id")%>%
  left_join(sepsis_lac,by="stay_id")%>%
  left_join(sepsis_po2,by="stay_id")%>%
  left_join(sepsis_fio2,by="stay_id")%>%
  left_join(sepsis_pco2,by="stay_id")%>%
  left_join(sepsis_pf,by="stay_id")%>%
  left_join(sepsis_baseexcess,by="stay_id")%>%
  left_join(sepsis_rhy_1,by="subject_id")%>%
  left_join(sepsis_alt,by="stay_id")%>%
  left_join(sepsis_alp,by="stay_id")%>%
  left_join(sepsis_ast,by="stay_id")%>%
  left_join(sepsis_bili,by="stay_id")%>%
  left_join(sepsis_ldh,by="stay_id")%>%
  left_join(sepsis_albumin,by="stay_id")%>%
  left_join(sepsis_bicarbonate,by="stay_id")%>%
  left_join(sepsis_bun,by="stay_id")%>%
  left_join(sepsis_calcium,by="stay_id")%>%
  left_join(sepsis_chloride,by="stay_id")%>%
  left_join(sepsis_creatinine,by="stay_id")%>%
  left_join(sepsis_glucose,by="stay_id")%>%
  left_join(sepsis_sodium,by="stay_id")%>%
  left_join(sepsis_potassium,by="stay_id")%>%
  left_join(sepsis_inr,by="stay_id")%>%
  left_join(sepsis_pt,by="stay_id")%>%
  left_join(sepsis_aptt,by="stay_id")%>%
  left_join(sepsis_hematocrit,by="stay_id")%>%
  left_join(sepsis_hemoglobin,by="stay_id")%>%
  left_join(sepsis_platelet,by="stay_id")%>%
  left_join(sepsis_wbc,by="stay_id")%>%
  left_join(sepsis_gcs,by="stay_id")%>%
  left_join(sepsis_dailyinputs,by="stay_id")%>%
  left_join(sepsis_inputrate,by="stay_id")%>%
  left_join(sepsis_uorate,by="stay_id")%>%
  left_join(sepsis_urine,by="stay_id")

finaldata$invasVent[which(is.na(finaldata$invasVent))]<-0 
finaldata$rrt_firstday[which(is.na(finaldata$rrt_firstday))]<-0 
finaldata$heart_rtythm[which(is.na(finaldata$heart_rtythm))]<-0 
colnames(finaldata)
summary(finaldata$ethni)
finaldata$admissiontype<-factor(finaldata$admissiontype)

finaldata<-finaldata%>%
  mutate(admissiontype=ifelse(admissiontype=="emergency","emergency","other"))%>%
  mutate(ethni=ifelse(ethni=="WHITE","white","other"))%>%
  mutate(infectionsite=ifelse(infectionsite==1,"pulmonary","other"))



