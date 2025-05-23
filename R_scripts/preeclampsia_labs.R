
cohort<-read.csv("pregnant_patient_encounters.csv")

library(dplyr) #Table reshaping
library(comorbidity) #Calculating comorbidity indices
library(reticulate) #enables a python session embedded in an R session
library(medExtractR)
library(parallel)
library(doParallel)
#library(FAIRmaterials) #Required for reshaping comorbidity index results
library(lubridate)
library(stringr)

labevents<-read.csv("labevents.csv")

labevents<-labevents[labevents$subject_id %in% cohort$subject_id,] #Around half are missing a hadm_id for encounter so need to be assigned with date ranges

#ADD LOINC CODES FOR EASY RECOVERY OF ITEMS
key<-readRDS("d_labitems_to_loinc.rds")
key<-key %>%
  select("itemid..omop_source_code.","omop_concept_code")
names(key)<-c("itemid","LOINC")
d_labitems<-readRDS("d_labitems.rds")
d_labitems<-left_join(d_labitems,key)
labevents<-left_join(labevents,d_labitems)
labevents$subject_id<-as.factor(labevents$subject_id)
labevents$LOINC<-as.factor(labevents$LOINC)

labevents<-labevents[,c("subject_id","hadm_id","charttime","label","value","valueuom","ref_range_lower","ref_range_upper","fluid","LOINC","flag")]

#Split out admission level labs
#Hgb is an example here

Hgb<-labevents[labevents$itemid %in% c(50811,51222,51640,51645),]
unique(Hgb$valueuom)#May need to convert some values if units are not equal
Hgb$difs<-difftime(Hgb$index_dt,Hgb$charttime,units="days")
Hgb <- Hgb %>%
  arrange(difs) %>%
  group_by(hadm_id) %>%
  slice(1)
Hgb$value<-as.numeric(Hgb$value)
Hgb<-Hgb %>%
  select("hadm_id","value")
names(Hgb)[2]<-"admission_Hgb"

UricAcid<-labevents[labevents$itemid %in% c(30841),]
unique(UricAcid$valueuom)#May need to convert some values if units are not equal
UricAcid$difs<-difftime(UricAcid$index_dt,UricAcid$charttime,units="days")
UricAcid <- UricAcid %>%
  arrange(difs) %>%
  group_by(hadm_id) %>%
  slice(1)
UricAcid$value<-as.numeric(UricAcid$value)
UricAcid<-UricAcid %>%
  select("hadm_id","value")
names(UricAcid)[2]<-"admission_UricAcid"

labevents<-left_join(labevents,Hgb)
labevents<-left_join(labevents,UricAcid)
write.csv(labevents,"labevents_preeclampsia.csv")
