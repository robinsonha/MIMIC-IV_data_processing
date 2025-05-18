cohort<-read.csv("preeclampsia_all.csv")

admissions<-readRDS("admissions.rds")
admissions<-admissions[admissions$subject_id %in% cohort$subject_id,]

deaths<-admissions[!is.na(admissions$deathtime),] %>%
  select("subject_id","hadm_id","dischtime","deathtime","discharge_location","admittime") %>%
  arrange(admittime) %>%
  group_by(subject_id) %>%
  slice(1) %>%
  ungroup()

transfers<-read.csv("transfers.csv")
transfers<-transfers[transfers$hadm_id %in% cohort$hadm_id,]

#where proc is the name of the therapy/procedure
#Simplify transfer classes:
transfers<-transfers %>%
  arrange(intime)
transfers$careunit<-ifelse(transfers$careunit=="" & transfers$eventtype=="discharge","discharge",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Cardiology Surgery"),"Cardiac Surgery",transfers$careunit)
transfers$careunit<-ifelse(endsWith(transfers$careunit,"CU)"),"ICU",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Medicine/Cardiology"),"Cardiology",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Medical/Surgical"),"Med/Surg",transfers$careunit)
transfers$careunit<-ifelse(transfers$careunit=="Surgery/Trauma","Med/Surg/Trauma",transfers$careunit)
transfers$careunit<-ifelse(transfers$careunit=="Discharge Lounge","discharge",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Neuro"),"Neurology",transfers$careunit)
transfers$careunit<-ifelse(endsWith(transfers$careunit,"Observation"),"Observation",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Hematology/Oncology"),"Hematology/Oncology",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Hematology/Oncology"),"Hematology/Oncology",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Surgery"),"Surgery",transfers$careunit)
transfers$careunit<-ifelse(endsWith(transfers$careunit,"Surgery"),"Surgery",transfers$careunit)
transfers$careunit<-ifelse(startsWith(transfers$careunit,"Med/Surg"),"Med/Surg",transfers$careunit)
transfers$careunit<-ifelse(transfers$careunit=="discharge" & transfers$hadm_id %in% deaths$hadm_id,"died",transfers$careunit)

write.csv(transfers,"preeclampsia_transfers.csv")
