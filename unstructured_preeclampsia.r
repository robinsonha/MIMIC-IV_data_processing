
# R code for regex processing clinical notes to identify pregnant preeclampsia patients
library(tidyverse)
library(stringr)
#RUN THIS SCRIPT BEFORE THE STRUCTURED DATA SCRIPT IF ANALYSING BOTH

pregnant_pats<-readRDS("pregnant_patient_encounters.rds")
structured_pats<-read.csv("preeclampsia_structured.csv") # results are formatted differently if joining to structured records
patients <- read.csv("patients.csv")
patients<-patients[patients$subject_id %in% pregnant_pats$subject_id,]
notes_data <- read.csv("discharge.csv", stringsAsFactors = FALSE)
notes_data<- notes_data[notes_data$hadm_id %in% pregnant_pats$hadm_id,]
# Function to read and analyze clinical notes
analyze_clinical_notes <- function(notes_data) {

  # Define keywords for pregnancy
#  pregnancy_keywords <- c(
#    "pregnant", "pregnancy", "gestation", "trimester", "gravid",
 #   "G1P0", "G2P1", "G3", "G4", "primigravida", "multigravida", "obstetric",
  #  "prenatal", "antenatal", "postnatal", "puerperium", "postpartum",
   # "delivery", "birth", "labor", "OB/GYN", "OBGYN", "fetal", "fetus",
#    "amniotic", "cesarean", "c-section", "placenta", "uterus",
 #   "maternal", "NSVD", "EDC", "EDD"
#  )

  # Define keywords for preeclampsia
  preeclampsia_keywords <- c(
    "hypertension", "HTN",
    "seizure", "headache", "visual disturbance", "RUQ pain",
    "right upper quadrant pain", "proteinuria", "protein in urine", "altered mental status",
    "preeclampsia", "pre-eclampsia", "eclampsia", "HELLP", "edema",
    "swelling"
  )

  # Function to check if text contains any keywords
  contains_keywords <- function(text, keywords) {
    if (is.na(text) || text == "") {
      return(FALSE)
    }
    text_lower <- tolower(text)
    any(sapply(keywords, function(keyword) {
      grepl(tolower(keyword), text_lower, fixed = TRUE)
    }))
  }

  # Function to extract covariates using regex
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

  # Function to find which specific keywords are present
  find_keywords <- function(text, keywords) {
    if (is.na(text) || text == "") {
      return(character(0))
    }
    text_lower <- tolower(text)
    keywords[sapply(keywords, function(keyword) {
      grepl(tolower(keyword), text_lower, fixed = TRUE)
    })]
  }

  # Extract chief complaint
  notes_data$chief_complaint <- sapply(notes_data$text, function(text) {
    cc_match <- str_match(text, "Chief Complaint:\\s*([^\\r\\n]+)")
    if (is.na(cc_match[1,2])) {
      return(NA)
    }
    return(trimws(cc_match[1,2]))
  })

  # Flag patients with preeclampsia symptoms
  notes_data$has_preeclampsia_symptoms <- sapply(notes_data$text, function(text) {
    contains_keywords(text, preeclampsia_keywords)
  })

  # Find specific preeclampsia keywords
  notes_data$preeclampsia_keywords_found <- sapply(notes_data$text, function(text) {
    paste(find_keywords(text, preeclampsia_keywords), collapse = ", ")
  })

  # Extract additional covariates
  notes_data$us_age <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "(?:Age|DOB):\\s*(\\d+)", NA)
  })

  notes_data$us_bmi <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "BMI:\\s*(\\d+\\.?\\d*)", NA)
  })

  notes_data$gestational_age <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "(?:gestational age|GA):\\s*(\\d+)\\s*(?:weeks|wks)", NA)
  })

  notes_data$us_systolic_bp <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "(?:BP|blood pressure).*?(\\d{2,3})\\/\\d{2,3}", NA)
  })

  notes_data$us_diastolic_bp <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "(?:BP|blood pressure).*?\\d{2,3}\\/(\\d{2,3})", NA)
  })

  notes_data$gravidity <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "G(\\d+)P\\d+", NA)
  })

  notes_data$parity <- sapply(notes_data$text, function(text) {
    extract_covariate(text, "G\\d+P(\\d+)", NA)
  })

  # Check for history of hypertension
  notes_data$us_hypertension_history <- sapply(notes_data$text, function(text) {
    med_history_match <- str_match(text, "Past Medical History:[^\\r\\n]*(\\r\\n[^\\r\\n]+)*")
    if (is.na(med_history_match[1,1])) {
      return(FALSE)
    }
    med_history <- med_history_match[1,1]
    grepl("hypertension", tolower(med_history), fixed = TRUE)
  })

  # Function to extract medication information
  extract_medications <- function(text, med_list) {
    if (is.na(text) || text == "") {
      return(character(0))
    }
    text_lower <- tolower(text)
    meds_found <- med_list[sapply(med_list, function(med) {
      grepl(tolower(med), text_lower, fixed = TRUE)
    })]
    return(paste(meds_found, collapse = ", "))
  }

  # Common medications for hypertension/preeclampsia
  hypertension_meds <- c(
    "labetalol", "methyldopa", "nifedipine", "hydralazine",
    "amlodipine", "enalapril", "lisinopril", "magnesium sulfate"
  )
  preeclampsia_meds <- c(
    "probenecid", "pegloticase", "allopurinol", "febucostat"
  )
  # Extract medication information
  notes_data$us_hypertension_meds <- sapply(notes_data$text, function(text) {
    extract_medications(text, hypertension_meds)
  })
  notes_data$us_preeclampsia_meds <- sapply(notes_data$text, function(text) {
    extract_medications(text, preeclampsia_meds)
  })

  # Filter for target population: female patients with both pregnancy indicators and preeclampsia symptoms
  return(
   notes_data
  )
}

 results <- analyze_clinical_notes(notes_data)
#
# # View target patients (pregnant patients with preeclampsia symptoms)
nrow(results)
head(results)
pregnant_all<-merge(patients,results,by=c("subject_id"))
write.csv(pregnant_all, "pregnant_unstructured.csv", row.names = FALSE)

