predata<-read.csv("pregnant_patient_encounters.csv")

#In addition to looking for the structured medications, the unstructured_preeclampsia script will also search for structured medication indications
#Current selection: probenecid, pegloticase, allopurinol, febuxostat.

#using the prescriptions tables (Meds prescribed during ED visits only),
#and the discharge summaries
#Note that the prescription and pharmacy fields for active/terminated prescriptions
#are overwritten as a prescription ends, so these should be ignored.

#This code has an embedded python chunk that uses the transformers module
#Before using you will need to install the transformers and torch modules in your python environment
#see https://packaging.python.org/en/latest/tutorials/installing-packages/ for instructions on how to do this
#If you are not a python user it may be easier to do this in the IDLE browser which is the python equivalent of R studio.

library(lubridate)
library(dplyr)
library(reticulate)#Allows python calls within R
library(medExtractR)#Use the medExtractR wrapper
library(doParallel)
#reticulate::install_python(version = '3.9:latest') #Install python if absent
#py_install("transformers") #Enables the python chunk
#py_install("torch") #Enables the python chunk

cohort<-pregnant_pats

########################################################################################################

meds_list<-c("probenecid", "pegloticase", "allopurinol", "febuxostat","oxytocin", "magnesium sulfate", "misoprostol", "dexamethasone",
             "penicillin", "terbutaline", "betamethasone", "bupivacaine",
             "ropivacaine", "fentanyl", "indomethacin", "rho(d) immune globulin",
             "rho immune globulin", "labetalol", "nifedipine", "hydralazine",
             "tranexamic acid", "carboprost", "dinoprostone")

prescriptions<-read.csv("prescriptions.csv")
prescriptions<-prescriptions[prescriptions$subject_id %in% cohort$subject_id,]
prescriptions$drug<-tolower(prescriptions$drug)

for(i in 1:length(meds_list)){
  eval(parse(text=paste0("ac<-prescriptions[grepl(\"",meds_list[i],"\",prescriptions$drug),]")))
  if(exists("ad")){
    print("yes")
    if(exists("ac")){
      print(names(ac))
      ad<-rbind(ad,ac)
    }
  } else {
    print("no")
    if(exists("ac")){
      ad<-ac
    }
  }
}
rm(prescriptions)
presc_meds<-ad[!is.na(ad$drug),]
#presc_meds<-presc_meds %>%
 # select("subject_id","hadm_id","starttime","drug")
########
# 2. Get medications from EMAR table (new addition)
emar <- read.csv("emar.csv")
emar <- emar[emar$subject_id %in% cohort$subject_id,]
emar$medication <- tolower(emar$medication)

# Initialize dataframe
emar_meds <- NULL
for(i in 1:length(meds_list)){
  eval(parse(text=paste0("ac <- emar[grepl(\"", meds_list[i], "\", emar$medication),]")))
  if(!is.null(emar_meds)){
    if(!is.null(ac) && nrow(ac) > 0){
      emar_meds <- rbind(emar_meds, ac)
    }
  } else {
    if(!is.null(ac) && nrow(ac) > 0){
      emar_meds <- ac
    }
  }
}
rm(emar)

# Standardize EMAR columns to match prescriptions format
if(!is.null(emar_meds) && nrow(emar_meds) > 0){
  emar_meds <- emar_meds %>%
    rename(drug = medication,
           starttime = charttime) %>%
    select(subject_id, hadm_id, starttime, drug)
  emar_meds$prod_strength <- NA
  emar_meds$dose_unit_rx <- NA
  emar_meds$doses_per_24_hrs <- NA
  emar_meds$route <- NA
}
saveRDS(emar_meds,"emar_meds_preeclampsia.rds")

saveRDS(presc_meds,"presc_meds_preeclampsia.rds")

#USE MEDEXTRACTR TO PULL PRESCRIPTIONS FROM DISCHARGE SUMMARIES

ncores<-5
if( .Platform$OS.type == "windows" ){
  cl <- makeCluster(as.numeric(ncores))  #specify how many cores to use
} else { # We are using the fork cluster type on linux because its faster but this isn't available for windows
  cl <- makeCluster(as.numeric(ncores),type="FORK")  #specify how many cores to use
}  #specify how many cores to use
registerDoParallel(cl)

#discharges<-discharges[1:10,]
discharges<-read.csv("discharge.csv")
discharges<-discharges[discharges$subject_id %in% cohort$subject_id,]

caps2<-foreach(a = 1:nrow(discharges), .packages = c("dplyr", "medExtractR"), .export = c("meds_list"),.combine=rbind) %dopar% { # For every
  # sink(file="log progress.txt",append=FALSE)
  note<-discharges$text[a]
  caps<-medExtractR(note=note, drug_names=meds_list,
                    unit="mg",
                    window_length=30, max_dist=1)
  #
  if(!is.logical(caps)){
    caps$hadm_id<-discharges$hadm_id[a]
    caps$charttime<-discharges$charttime[a]
    caps<-caps[!is.na(caps$entity),]
    return(caps)
  }
}

drugs_only<-caps2[caps2$entity=="DrugName",]
drugs_only$expr<-tolower(drugs_only$expr)
table(drugs_only$expr)

drugs_only<-drugs_only %>%
  select("hadm_id","charttime","expr") %>%
  left_join(pregnant_pats)

drugs_only$drug<-drugs_only$expr
drugs_only$starttime<-drugs_only$charttime
drugs_only<-drugs_only[drugs_only$hadm_id %in% pregnant_pats$hadm_id ,]

drugs_only<-drugs_only %>%
  select("subject_id","hadm_id","starttime","drug")
drugs_only$prod_strength<-NA
drugs_only$dose_unit_rx<-NA
drugs_only$doses_per_24_hrs<-NA
drugs_only$route<-NA

presc_meds<-presc_meds %>%
  select("subject_id","hadm_id","starttime","drug","prod_strength","dose_unit_rx","doses_per_24_hrs", "route")
# Add EMAR medications if they exist
if(!is.null(emar_meds) && nrow(emar_meds) > 0){
  presc_meds <- rbind(presc_meds, emar_meds)
}
presc_meds<-rbind(presc_meds,drugs_only)
#presc_meds$starttime<-as.POSIXct(presc_meds$starttime)


#There are lots of synonyms for the same medication in the drugs field so combine the similar ones:
groups <- list()
i <- 1
similarGroups <- function(x, thresh = 0.8){
  grp <- integer(length(x))
  name <- x
  for(i in seq_along(name)){
    if(!is.na(name[i])){
      sim <- agrepl(x[i], x, ignore.case = TRUE, max.distance = 1 - thresh)
      k <- which(sim & !is.na(name))
      grp[k] <- i
      is.na(name) <- k
    }
  }
  grp
}
presc_meds$drug<-gsub("desensitization","",presc_meds$drug)
presc_meds$drug<-gsub("*","",presc_meds$drug)
presc_meds$drug<-str_replace(presc_meds$drug, " \\s*\\([^\\)]+\\)", "")
presc_meds$drug <- stringr::str_replace(presc_meds$drug, '\\*', '') #This is used twice as some have 2 lead asterisks
presc_meds$drug <- stringr::str_replace(presc_meds$drug, '\\*', '')
presc_meds$drug<-gsub("nf ","",presc_meds$drug)
presc_meds$drug <-gsub(".*:","",presc_meds$drug)

write.csv(presc_meds,"preeclampsia_medications.csv")





## List of drugs to create binary columns for
drugs_to_flag <- c("probenecid", "pegloticase", "allopurinol", "febuxostat","oxytocin", "magnesium sulfate", "misoprostol", "dexamethasone",
                   "penicillin", "terbutaline", "betamethasone", "bupivacaine",
                   "ropivacaine", "fentanyl", "indomethacin", "rho(d) immune globulin",
                   "rho immune globulin", "labetalol", "nifedipine", "hydralazine",
                   "tranexamic acid", "carboprost", "dinoprostone")

# Convert drug names to lowercase for case-insensitive matching
presc_meds$drug_lower <- tolower(presc_meds$drug)

# Create binary columns for each drug
for (drug in drugs_to_flag) {
  # Clean drug name for column name
  col_name <- gsub("[^a-zA-Z0-9]", "_", tolower(drug))
  col_name <- gsub("_+", "_", col_name)  # Replace multiple underscores with one
  col_name <- gsub("_$|^_", "", col_name)  # Remove leading/trailing underscores

  # Create binary column (1 if drug is found, 0 otherwise)
  presc_meds[[col_name]] <- as.integer(grepl(drug, presc_meds$drug_lower, ignore.case = TRUE))
}

# Remove temporary lowercase column
presc_meds$drug_lower <- NULL

# Aggregate by subject_id to get patient-level medication flags
patient_meds <- presc_meds %>%
  group_by(subject_id,hadm_id) %>%
  summarise(
    across(ends_with("_immune_globulin"), ~ as.integer(any(. == 1)), .names = "{.col}"),
    across(c(oxytocin:tranexamic_acid, carboprost:dinoprostone), ~ as.integer(any(. == 1)), .names = "{.col}")
  ) %>%
  distinct()

# For rho immune globulin, combine both spellings
if ("rho_d_immune_globulin" %in% names(patient_meds) && "rho_immune_globulin" %in% names(patient_meds)) {
  patient_meds$rho_immune_globulin <- as.integer(patient_meds$rho_d_immune_globulin | patient_meds$rho_immune_globulin)
  patient_meds$rho_d_immune_globulin <- NULL
}

# Merge 
cohort <- left_join(presc_meds, patient_meds)

# Save the updated cohort
write.csv(cohort, "preeclampsia_medications.csv", row.names = FALSE)
saveRDS(cohort, "preeclampsia_medications.rds")
