# Load necessary libraries
library(dplyr)
library(tidyr)

pregnant_pats<-readRDS("pregnant_patient_encounters.rds")
patients <- read.csv("patients.csv")
patients<-patients[patients$subject_id %in% pregnant_pats$subject_id,]

# Load required libraries
library(dplyr)
library(readr)
library(lubridate)

# Load necessary tables (adjust filenames if needed)
diagnoses_icd <- read.csv("diagnoses.csv")


# Define preeclampsia ICD-10 codes
#preeclampsia_codes <- c(
 # "O131", "O139",  # Mild preeclampsia (O13.1, O13.9)
  #"O141", "O149",  # Severe preeclampsia (O14.1, O14.9)
#  "O142",          # HELLP syndrome (O14.2)
 # "O150", "O151", "O152", "O159",  # Eclampsia (O15.x)
  #"O111", "O112", "O119"           # Superimposed preeclampsia (O11.x)
#)

admissions <- readRDS("admissions.rds")

#Identify admissions with preeclampsia diagnoses
preeclampsia_dx <- diagnoses_icd %>%
  filter(
    icd_version == 10,               # ICD-10 only
    str_starts(icd_code, "O13|O14|O15|O11")  # Broad preeclampsia codes
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
pregnant_all<-read.csv("preeclampsia_unstructured.csv")
pregnant_all<-left_join(pregnant_all,preeclampsia_dx,by=c("subject_id","hadm_id"))
pregnant_all$preeclampsia<-ifelse(pregnant_all$preeclampsia_structured==1|pregnant_all$has_preeclampsia_symptoms==TRUE,1,0)

write.csv(pregnant_all,"preeclampsia_all.csv")

