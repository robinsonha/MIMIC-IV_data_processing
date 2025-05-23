library(dplyr)
library(stringr)

setwd("D:/MIMIC")
diagnoses_icd<-read.csv("diagnoses.csv")

pregnancy_icd9_codes <- c(paste0("630", 0:9), paste0("631", 0:9), paste0("632", 0:9),
                          paste0("633", 0:9), paste0("634", 0:9), paste0("635", 0:9),
                          paste0("636", 0:9), paste0("637", 0:9), paste0("638", 0:9),
                          paste0("639", 0:9), paste0("V22", 0:9), paste0("V23", 0:9),
                          paste0("V27", 0:9), paste0("V28", 0:9))

pregnancy_icd10_codes <- c(paste0("O", sprintf("%02d", 0:99)), "Z33", "Z34", "Z36",
                           "Z37", "Z39", paste0("Z3A", 0:9))


pregnant_dx <- diagnoses_icd %>%
    filter(
      (icd_version == 9 & str_sub(icd_code, 1, 3) %in% pregnancy_icd9_codes) |
        (icd_version == 10 & (str_sub(icd_code, 1, 1) == "O" |
                                str_sub(icd_code, 1, 3) %in% pregnancy_icd10_codes))
    ) %>%
    distinct(subject_id,hadm_id)
rm(diagnoses_icd)
pregnant_dx$pregnancy_indication<-"diagnosis"

procedures_icd<-readRDS("procedures_icd.rds")

pregnant_proc <- procedures_icd %>%
    filter(
      (icd_version == 9 & str_sub(icd_code, 1, 2) %in% c("72", "73", "74", "75")) |
        (icd_version == 10 & str_sub(icd_code, 1, 3) %in% c("10E", "1"))
    ) %>%
    distinct(subject_id,hadm_id)
rm(procedures_icd)
pregnant_proc$pregnancy_indication<-"procedure"


d_labitems<-readRDS("d_labitems.rds")

  # 3. Lab tests
pregnancy_items <- d_labitems %>%
    filter(str_detect(tolower(label), "pregnan") ) %>%
    pull(itemid)
rm(d_labitems)
pregnancy_items$pregnancy_indication<-"items"


labevents<-read.csv("labevents.csv")

pregnant_labs <- labevents %>%
    filter(itemid %in% pregnancy_items & valuenum == 1) %>%
    distinct(subject_id,hadm_id)
rm(labevents)
pregnancy_labs$pregnancy_indication<-"labs"

pregnancy_keywords <- c(
  "pregnant", "pregnancy", "gestation", "trimester", "gravid",
  "G1P0", "G2P1", "G3", "G4", "primigravida", "multigravida", "obstetric",
  "prenatal", "antenatal", "postnatal", "puerperium", "postpartum",
  "delivery", "birth", "labor", "OB/GYN", "OBGYN", "fetal", "fetus",
  "amniotic", "cesarean", "c-section", "placenta", "uterus",
  "maternal", "NSVD", "EDC", "EDD"
)

#Check if text contains any keywords
contains_keywords <- function(text, keywords) {
  if (is.na(text) || text == "") {
    return(FALSE)
  }
  text_lower <- tolower(text)
  any(sapply(keywords, function(keyword) {
    grepl(tolower(keyword), text_lower, fixed = TRUE)
  }))
}

# extract covariates using regex
extract_covariate <- function(text, pattern, default = NA) {
  if (is.na(text) || text == "") {
    return(default)
  }
  match <- str_match(text, pattern)
  if (is.na(match[1,2])) {
    return(default)
  }
  return(match[1,2])
}

# find which specific keywords are present
find_keywords <- function(text, keywords) {
  if (is.na(text) || text == "") {
    return(character(0))
  }
  text_lower <- tolower(text)
  keywords[sapply(keywords, function(keyword) {
    grepl(tolower(keyword), text_lower, fixed = TRUE)
  })]
}

#notes_data <- read.csv("discharge.csv", stringsAsFactors = FALSE)
#notes_data<- notes_data[notes_data$hadm_id %in% pregnant_pats$hadm_id,]

# Flag patients with pregnancy indicators
#notes_data$is_pregnant <- sapply(notes_data$text, function(text) {
#  contains_keywords(text, pregnancy_keywords)
#})
#pregnancy_notes<-notes_data %>%
 # filter(is_pregnant==TRUE) %>%
  #distinct(subject_id,hadm_id)
#rm(notes_data)

  # Combine all so we have one row per encounter but retain info on the pregnancy indication:
pregnant_pats <- bind_rows(pregnant_dx, pregnant_proc, pregnant_labs) %>%
  group_by(subject_id, hadm_id) %>%
  summarize(
    pregnancy_indication = str_c(unique(pregnancy_indication), collapse = "_"),  # use an underscore separator
    .groups = "drop"  # Avoid grouped output
  ) %>%
  arrange(subject_id, hadm_id)

admissions <- readRDS("admissions.rds")
admissions<-dplyr::left_join(pregnant_pats,admissions)

#Use anchor year key from patients to create time dependent variables in admissions
admissions$admittime<-strptime(admissions$admittime, "%Y-%m-%d %H:%M:%S")
admissions$dischtime<-strptime(admissions$dischtime, "%Y-%m-%d %H:%M:%S")
admissions$type<-ifelse(admissions$admission_type %in% c("AMBULATORY OBSERVATION", "DIRECT EMER.", "URGENT", "EW EMER.", "DIRECT OBSERVATION", "EU OBSERVATION", "OBSERVATION ADMIT"),"Emergency","Elective")
admissions$los<-as.numeric(difftime(admissions$dischtime, admissions$admittime, units="days"))
admissions$date_of_death<-as.Date(admissions$deathtime)
admissions$date_of_discharge<-as.Date(admissions$dischtime)
admissions$subject_id<-as.factor(admissions$subject_id)
pregnant_pats$subject_id<-as.factor(pregnant_pats$subject_id)
pregnant_pats$hadm_id<-as.factor(pregnant_pats$hadm_id)
admissions$hadm_id<-as.factor(admissions$hadm_id)

pregnant_pats<-left_join(pregnant_pats,admissions)
rm(admissions)

patients<-read.csv("patients.csv")
patients$subject_id<-as.factor(patients$subject_id)
pregnant_pats<-left_join(pregnant_pats,patients)
rm(patients)

pregnant_pats$age<-as.numeric(format(pregnant_pats$admittime,"%Y"))-pregnant_pats$anchor_year+pregnant_pats$anchor_age
pregnant_pats$survival_days<-ifelse(!is.na(pregnant_pats$date_of_death),pregnant_pats$los,NA)
pregnant_pats$mortality1<-ifelse(!is.na(pregnant_pats$survival_days) & pregnant_pats$survival_days<366,1,0)
pregnant_pats$mortality6<-ifelse(!is.na(pregnant_pats$survival_days) & pregnant_pats$survival_days<183,1,0)
pregnant_pats$mortality5<-ifelse(!is.na(pregnant_pats$survival_days) & pregnant_pats$survival_days<1826,1,0)

saveRDS(pregnant_pats,"pregnant_patient_encounters.rds")
write.csv(pregnant_pats,"pregnant_patient_encounters.csv")
