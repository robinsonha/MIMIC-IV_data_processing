predata<-read.csv("preeclampsia_all.csv")

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

cohort<-read.csv("preeclampsia_all.csv")

########################################################################################################

meds_list<-c("probenecid", "pegloticase", "allopurinol", "febuxostat")

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
  select("hadm_id","charttime","expr")

admissions_s<-admissions[,c("subject_id","hadm_id")]
drugs_only<-left_join(drugs_only,admissions_s)
drugs_only$drug<-drugs_only$expr
drugs_only$starttime<-drugs_only$charttime
drugs_only<-drugs_only[drugs_only$subject_id %in% cohort$subject_id ,]

drugs_only<-drugs_only %>%
  select("subject_id","hadm_id","starttime","drug")
drugs_only$prod_strength<-NA
drugs_only$dose_unit_rx<-NA
drugs_only$doses_per_24_hrs<-NA
drugs_only$route<-NA

presc_meds<-presc_meds %>%
  select("subject_id","hadm_id","starttime","drug","prod_strength","dose_unit_rx","doses_per_24_hrs", "route")

presc_meds<-rbind(presc_meds,drugs_only)
presc_meds$starttime<-as.POSIXct(presc_meds$starttime)


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
