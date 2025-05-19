# Load required libraries
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)

pregnant_pats<-readRDS("pregnant_patient_encounters.rds")
patients <- read.csv("patients.csv")
patients<-patients[patients$subject_id %in% pregnant_pats$subject_id,]

# Load necessary tables (adjust filenames if needed)
diagnoses_icd <- read.csv("diagnoses.csv")
diagnoses_icd_detail<-readRDS("d_icd_diagnoses.rds")
diagnoses_icd<-diagnoses_icd[diagnoses_icd$subject_id %in% pregnant_pats$subject_id,]
diagnoses_icd<-dplyr::left_join(diagnoses_icd,diagnoses_icd_detail,by=c("icd_code","icd_version"))

#History of CVD

Hx<-diagnoses_icd[grep(x = diagnoses_icd$icd_code, pattern = "^(I05|I06|I07|I08|I09|I1|I2|Z867|4106|4107|4108|4109|4273|4289|430|431|433|434|4284|4283|4282|4281|4280)"),]
diagnoses_icd$Hx_CVD<-ifelse(diagnoses_icd$subject_id %in% Hx$subject_id,1,0)

admissions <- readRDS("admissions.rds")
admissions<-dplyr::left_join(admissions,patients)

#Use anchor year key from patients to create time dependent variables in admissions
admissions$admittime<-strptime(admissions$admittime, "%Y-%m-%d %H:%M:%S")
admissions$dischtime<-strptime(admissions$dischtime, "%Y-%m-%d %H:%M:%S")
admissions$type<-ifelse(admissions$admission_type %in% c("AMBULATORY OBSERVATION", "DIRECT EMER.", "URGENT", "EW EMER.", "DIRECT OBSERVATION", "EU OBSERVATION", "OBSERVATION ADMIT"),"Emergency","Elective")
admissions$los<-as.numeric(difftime(admissions$dischtime, admissions$admittime, units="days"))
admissions$date_of_death<-as.Date(admissions$dod)
admissions$date_of_discharge<-as.Date(admissions$dischtime)
admissions$age<-as.numeric(format(admissions$admittime,"%Y"))-admissions$anchor_year+admissions$anchor_age

admissions$survival_days<-ifelse(!is.na(admissions$date_of_death),admissions$los,NA)
admissions$mortality1<-ifelse(!is.na(admissions$survival_days) & admissions$survival_days<366,1,0)
admissions$mortality6<-ifelse(!is.na(admissions$survival_days) & admissions$survival_days<183,1,0)
admissions$mortality5<-ifelse(!is.na(admissions$survival_days) & admissions$survival_days<1826,1,0)

#Identify admissions with preeclampsia diagnoses
preeclampsia_dx <- diagnoses_icd %>%
  filter(

    (icd_version == 10 &
    str_starts(icd_code, "O13|O14|O15|O11") ) |
    (icd_version == 9 &
    str_starts(icd_code, "6420|6424|6425|6426|6427|6430|4019") )
     # Broad preeclampsia codes
  ) %>%
  left_join(admissions, by = c("hadm_id","subject_id")) %>%
#  left_join(patients, by = "subject_id") %>%
  mutate(
    # Classify preeclampsia type
    preeclampsia_type = case_when(
      str_starts(icd_code, "O13") ~ "Mild preeclampsia",
      str_starts(icd_code, "O141") ~ "Severe preeclampsia",
      str_starts(icd_code, "O142") ~ "HELLP syndrome",
      str_starts(icd_code, "O15") ~ "Eclampsia",
      str_starts(icd_code, "O11") ~ "Superimposed preeclampsia",
      str_starts(icd_code, "642.4") ~ "Mild preeclampsia",         # ICD-9 equivalent for O13
      str_starts(icd_code, "642.5") ~ "Severe preeclampsia",      # ICD-9 equivalent for O14.1
      str_starts(icd_code, "642.5") ~ "HELLP syndrome",            # Note: ICD-9 doesn't have a specific code for HELLP - often same as severe preeclampsia
      str_starts(icd_code, "642.6") ~ "Eclampsia",                # ICD-9 equivalent for O15
      str_starts(icd_code, "642.7") ~ "Superimposed preeclampsia", # ICD-9 equivalent for O11
      TRUE ~ "Other"
    ),
     # Convert dates for timing analysis
    admittime = ymd_hms(admittime),
    dischtime = ymd_hms(dischtime)
  )
rm(admissions)
rm(diagnoses_icd)

#procedures_icd <- readRDS("procedures_icd.rds")

#Identify delivery procedures (for timing)
#I have tried to implement this but it is not recovering delivery, we may not have capture of obstetrics.
#delivery_procedures <- procedures_icd %>%
#  filter(
 #   str_starts(icd_code, "O6|O7|O8")  #Broad delivery codes
#  ) %>%
 # mutate(procedure_date = ymd_hms(chartdate))  # Assuming chartdate exists

#Determine timing (postpartum?)
#preeclampsia <- preeclampsia_dx %>%
 # left_join(delivery_procedures, by = "hadm_id") %>%
#  mutate(
 #   diagnosis_timing = case_when(
  #    !is.na(procedure_date) & admittime < procedure_date ~ "Antepartum",
   #   !is.na(procedure_date) & admittime >= procedure_date ~ "Postpartum",
    #  TRUE ~ "Unknown"
#    )
#  ) %>%
 # select(
#    subject_id, hadm_id,
 #   icd_code, preeclampsia_type,
  #  admittime, dischtime,
   # diagnosis_timing, gender, anchor_age
  #)
preeclampsia_dx$preeclampsia_structured<-1

write.csv(preeclampsia_dx,"preeclampsia_structured.csv")
