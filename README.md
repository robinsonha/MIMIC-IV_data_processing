# MIMIC-IV_data_processing
MIMIC IV data processing to extract key variables and assemble cohorts

diagnoses_icd contains all diagnoses of patients during their admission

emar records medicine given, populated by bedside nursing staff (NOT billing staff). (emar time stamp should be from barcode scanning at the time of administratio0.

procedures_icd lists all procedures a patient was billed for during their hospital stay.

admissions contain unique hospital visits of a patient.

patient contains demographic data

omr is the online medical record and contains BP, height, weight, eGFR
