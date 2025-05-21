The following pipeline will assemble the derived MIMIC-IV tables for the pregnant patient cohort:

pregnancy_diagnoses.R isolates all probable pregnant patients, based on coded indications, pregnancy related item orders, pregnancy related medications and regex matching in the discharge notes, FROM THE SAME ENCOUNTER.

In unstructured_preeclampsia.R, free text discharge notes for the pregnancy cohort are searched for regex matches to key medications and mentions of symptoms. Where these notes are available, they are left joined to the primary table.

In structured_preeclampsia.R, coded fields for the pregnancy cohort are searched for indications of key medications and coded diagnosis or symptoms. Where these fields are available, they are left joined to the primary table.
