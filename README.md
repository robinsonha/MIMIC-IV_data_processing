# MIMIC-IV_data_processing
MIMIC IV data processing to extract key variables and assemble cohorts

pregnant_patients.csv is the primary patient table, containing all pregnant patients identified in pregnant_patient_encounters, linked to preeclampsia indications

preeclampsia_prescriptions contains prescriptions of interest for all of the patients in pregnant_patients.csv

preeclampsia_observations contains all observations for all of the patients in pregnant_patients.csv

preeclampsia_transfers follows the patient journey between wards for all of the patients in pregnant_patients.csv

preeclampsia_labs contains all lab values for all of the patients in pregnant_patients.csv


Guide to raw data:

diagnoses_icd contains all diagnoses of patients during their admission

emar records medicine given, populated by bedside nursing staff (NOT billing staff). (emar time stamp should be from barcode scanning at the time of administratio0.

procedures_icd lists all procedures a patient was billed for during their hospital stay.

admissions contain unique hospital visits of a patient.

patient contains demographic data

omr is the online medical record and contains BP, height, weight, eGFR


