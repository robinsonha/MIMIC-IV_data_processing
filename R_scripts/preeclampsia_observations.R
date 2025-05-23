cohort<-read.csv("pregnant_patient_encounters.csv")

#omr table is the Online Medical Record 
#blood pressure, height, weight, body mass index, and Estimated Glomerular Filtration Rate (eGFR). 
#These values are available from both inpatient and outpatient visits

omr<-read.csv("omr.csv")
omr<-omr[omr$subject_id %in% cohort$subject_id,]

write.csv(omr,"preeclampsia_observations.csv")
