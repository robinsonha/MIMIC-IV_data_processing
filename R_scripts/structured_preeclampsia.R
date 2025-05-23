#If using unstructured indications, run that script first
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

#Identify admissions with preeclampsia diagnoses
preeclampsia_dx <- diagnoses_icd %>%
  filter(
    
    (icd_version == 10 &
       str_starts(icd_code, "O13|O14|O15|O11") ) |
      (icd_version == 9 &
         str_starts(icd_code, "6420|6424|6425|6426|6427|6430|4019") )
    # Broad preeclampsia codes
  ) %>%
#  left_join(admissions, by = c("hadm_id","subject_id")) %>%
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
    )
    # Convert dates for timing analysis
#    admittime = ymd_hms(admittime),
#    dischtime = ymd_hms(dischtime)
  )
#rm(admissions)
rm(diagnoses_icd)

preeclampsia_dx$preeclampsia_structured<-1
write.csv(preeclampsia_dx,"preeclampsia_structured.csv")

##########################
pregnant_pats<-readRDS("pregnant_patient_encounters.rds")
patients <- read.csv("patients.csv")
patients<-patients[patients$subject_id %in% pregnant_pats$subject_id,]

pregnant_unstructured<-read.csv("preeclampsia_unstructured.csv")
#preeclampsia_dx<-read.csv("preeclampsia_structured.csv")

pregnant_encounters<-left_join(pregnant_pats,patients)
pregnant_encounters<-pregnant_encounters %>%
  select(-'gender')
pregnant_encounters<-left_join(pregnant_encounters,pregnant_unstructured)
pregnant_encounters<-pregnant_encounters %>%
  select(-'gender')
pregnant_all<-left_join(pregnant_encounters,preeclampsia_dx) 
pregnant_all$preeclampsia<-ifelse(pregnant_all$preeclampsia_structured==1|
                                           pregnant_all$has_preeclampsia_symptoms==TRUE,1,0)

write.csv(pregnant_all,"pregnant_patients.csv")

