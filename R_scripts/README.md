The following pipeline will assemble the derived MIMIC-IV tables for the pregnant patient cohort:

pregnancy_diagnoses.R isolates all probable pregnant patients, based on coded indications, pregnancy related item orders, pregnancy related medications and regex matching in the discharge notes, FROM THE SAME ENCOUNTER. Sourcing from diagnoses_icd, d_items, lab_items, procedures_icd. Admissions is then joined onto this table- this table has multiple rows for each encounter potentially, relating to different indications of pregnancy. That can later be collapsed to one row per encounter if required. Admissions brings in key dates amd the death indicator and allows calculation of age.

In unstructured_preeclampsia.R, free text discharge notes for the pregnancy cohort are searched for regex matches to key medications and mentions of symptoms. Where these notes are available, they are left joined to the primary table.

In structured_preeclampsia.R, coded fields for the pregnancy cohort are searched for indications of key medications and coded diagnosis or symptoms. Where these fields are available, they are left joined to the primary table.
