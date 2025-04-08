
# Script for processing raw CIPHA DynAirX data into workable format.

# Loading raw data

Raw_Extr_df <- read.csv(here("data", "raw_extracts", "Raw_Patient_Extract.csv"), header = TRUE)

# Manually formatting new columns, including manually splitting appropriate prescription values and units and ensuring consistent test, diagnoses and prescription terms

Raw_Extr_df_Filter <-  Raw_Extr_df %>%
  filter(term %in% c("Lying diastolic blood pressure (observable entity)",
                     "Lying systolic blood pressure (observable entity)",
                     "Renal profile (observable entity)",
                     "Serum low density lipoprotein cholesterol level (observable entity)",
                     "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)",
                     "Essential hypertension (disorder)",
                     "Constipation (disorder)",
                     "Diabetes mellitus type 2 (disorder)",
                     "Chronic obstructive lung disease (disorder)",
                     "Mixed anxiety and depressive disorder (disorder)",
                     "Depressive disorder NEC (disorder)",
                     "Depressive disorder (disorder)",
                     "Atorvastatin 10mg tablets",
                     "Atorvastatin 20mg tablets",
                     "Atorvastatin 40mg tablets",
                     "Atorvastatin 60mg tablets",
                     "Bisacodyl 5mg gastro-resistant tablets",
                     "Bisoprolol 1.25mg tablets",
                     "Bisoprolol 10mg tablets",
                     "Bisoprolol 2.5mg tablets",
                     "Bisoprolol 5mg tablets",
                     "Candesartan 4mg tablets",
                     "Candesartan 8mg tablets",
                     "Citalopram 10mg tablets",                     
                     "Citalopram 20mg tablets",
                     "Escitalopram 10mg tablets",
                     "Flucloxacillin 500mg capsules",
                     "Fluconazole 150mg capsules")) %>%
  mutate(Contact_Record_Category = case_when(
    term %in% c("Lying diastolic blood pressure (observable entity)",
                "Lying systolic blood pressure (observable entity)",
                "Renal profile (observable entity)",
                "Serum low density lipoprotein cholesterol level (observable entity)",
                "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)") ~ "Biomedical Assay",
    term %in% c("Essential hypertension (disorder)",
                "Constipation (disorder)",
                "Diabetes mellitus type 2 (disorder)",
                "Chronic obstructive lung disease (disorder)",
                "Mixed anxiety and depressive disorder (disorder)",
                "Depressive disorder NEC (disorder)",
                "Depressive disorder (disorder)") ~ "Diagnosis/Review",
    term %in% c("Atorvastatin 10mg tablets",
                "Atorvastatin 20mg tablets",
                "Atorvastatin 40mg tablets",
                "Atorvastatin 60mg tablets",
                "Bisacodyl 5mg gastro-resistant tablets",
                "Bisoprolol 1.25mg tablets",
                "Bisoprolol 10mg tablets",
                "Bisoprolol 2.5mg tablets",
                "Bisoprolol 5mg tablets",
                "Candesartan 4mg tablets",
                "Candesartan 8mg tablets",
                "Citalopram 10mg tablets",                     
                "Citalopram 20mg tablets",
                "Escitalopram 10mg tablets",
                "Flucloxacillin 500mg capsules",
                "Fluconazole 150mg capsules") ~ "Medication",
    TRUE ~ "NotKnown")) %>%
  mutate(units = case_when(
    term %in% c("Atorvastatin 10mg tablets",
                "Atorvastatin 20mg tablets",
                "Atorvastatin 40mg tablets",
                "Atorvastatin 60mg tablets",
                "Bisacodyl 5mg gastro-resistant tablets",
                "Bisoprolol 1.25mg tablets",
                "Bisoprolol 10mg tablets",
                "Bisoprolol 2.5mg tablets",
                "Bisoprolol 5mg tablets",
                "Candesartan 4mg tablets",
                "Candesartan 8mg tablets",
                "Citalopram 10mg tablets",                     
                "Citalopram 20mg tablets",
                "Escitalopram 10mg tablets",
                "Flucloxacillin 500mg capsules",
                "Fluconazole 150mg capsules") ~ "mg",
    term == "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)" ~ "mmol/mol",
    TRUE ~ units)) %>%
  mutate(value = case_when(
    term == "Atorvastatin 10mg tablets" ~ "10",
    term == "Atorvastatin 20mg tablets" ~ "20",
    term == "Atorvastatin 40mg tablets" ~ "40",
    term == "Atorvastatin 60mg tablets" ~ "60",
    term == "Bisacodyl 5mg gastro-resistant tablets" ~ "5",
    term == "Bisoprolol 1.25mg tablets" ~ "1.25",
    term == "Bisoprolol 10mg tablets" ~ "10",
    term == "Bisoprolol 2.5mg tablets" ~ "2.5",
    term == "Bisoprolol 5mg tablets" ~ "5",
    term == "Candesartan 4mg tablets" ~ "4",
    term == "Candesartan 8mg tablets" ~ "8",
    term == "Citalopram 10mg tablets" ~ "10",                     
    term == "Citalopram 20mg tablets" ~ "20",
    term == "Escitalopram 10mg tablets" ~ "10",
    term == "Flucloxacillin 500mg capsules" ~ "500",
    term == "Fluconazole 150mg capsules"  ~ "150",
    TRUE ~ value)) %>%
  mutate(Contact_Event_Term = case_when(
    term == "Lying diastolic blood pressure (observable entity)" ~ "Lying diastolic BP",
    term == "Lying systolic blood pressure (observable entity)" ~ "Lying systolic BP",
    term == "Renal profile (observable entity)" ~ "Renal profile",
    term == "Serum low density lipoprotein cholesterol level (observable entity)" ~ "Serum LDL chol. level",
    term == "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)" ~ "HbA1c level",
    term == "Essential hypertension (disorder)" ~ "Essential hypertension",
    term == "Constipation (disorder)" ~ "Constipation",
    term == "Diabetes mellitus type 2 (disorder)" ~ "Diabetes type 2",
    term == "Chronic obstructive lung disease (disorder)" ~ "COLD",
    term == "Mixed anxiety and depressive disorder (disorder)" ~ "Depressive disorders",
    term == "Depressive disorder NEC (disorder)" ~ "Depressive disorders",
    term == "Depressive disorder (disorder)" ~ "Depressive disorders",
    term == "Atorvastatin 10mg tablets" ~ "Atorvastatin",
    term == "Atorvastatin 20mg tablets" ~ "Atorvastatin",
    term == "Atorvastatin 40mg tablets" ~ "Atorvastatin",
    term == "Atorvastatin 60mg tablets" ~ "Atorvastatin",
    term == "Bisacodyl 5mg gastro-resistant tablets" ~ "Bisacodyl",
    term == "Bisoprolol 1.25mg tablets" ~ "Bisoprolol",
    term == "Bisoprolol 10mg tablets" ~ "Bisoprolol",
    term == "Bisoprolol 2.5mg tablets" ~ "Bisoprolol",
    term == "Bisoprolol 5mg tablets" ~ "Bisoprolol",
    term == "Candesartan 4mg tablets" ~ "Candesartan",
    term == "Candesartan 8mg tablets" ~ "Candesartan",
    term == "Citalopram 10mg tablets" ~ "Citalopram",                     
    term == "Citalopram 20mg tablets" ~ "Citalopram",
    term == "Escitalopram 10mg tablets" ~ "Escitalopram",
    term == "Flucloxacillin 500mg capsules" ~ "Flucloxacillin",
    term == "Fluconazole 150mg capsules"  ~ "Fluconazole",
    TRUE ~ term)) %>%
  mutate(Patient_Name = case_when(
    PK_Patient_ID == "610673" ~ "Olivia Davies",
    PK_Patient_ID == "1684031" ~ "Thomas Williams",
    PK_Patient_ID == "714765" ~ "Leah O'Sullivan",
    PK_Patient_ID == "5329931" ~ "Emily Johnson",
    PK_Patient_ID == "2337077" ~ "Debelah Oluwaseyi",
    PK_Patient_ID == "2813256" ~ "Maya Ahmed",
    PK_Patient_ID == "5162922" ~ "Jack Murphy",
    PK_Patient_ID == "3055848" ~ "Zara Singh",
    PK_Patient_ID == "5182896" ~ "Georgina Taylor",
    PK_Patient_ID == "2839091" ~ "Leila Hassan",
    PK_Patient_ID == "2701908" ~ "William Edwards",
    PK_Patient_ID == "2752409" ~ "Danielle Roberts",
    PK_Patient_ID == "4822061" ~ "Rhea Desai",
    PK_Patient_ID == "5330850" ~ "Harriet Evans",
    PK_Patient_ID == "3359779" ~ "Amara Nwachukwu",
    PK_Patient_ID == "1920086" ~ "Isla Campbell",
    PK_Patient_ID == "3293167" ~ "Saesha Sharma",
    TRUE ~ "NotKnown")) %>%
  filter(Patient_Name %in% c("Olivia Davies",
                             "Thomas Williams",
                             "Emily Johnson",
                             "Debelah Oluwaseyi",
                             "Jack Murphy",
                             "Zara Singh",
                             "Georgina Taylor",
                             "Rhea Desai",
                             "Harriet Evans",
                             "Amara Nwachukwu",
                             "Isla Campbell",
                             "Saesha Sharma")) %>%
  mutate(Patient_Gender = case_when(
    Sex == "M" ~ "Male",
    Sex == "F" ~ "Female",
    TRUE ~ "NotKnown")) %>%
  rename(Patient_DOB = Dob,
         Contact_Event_Date = date,
         Prescription_Instruction = Episodicity,
         Value = value,
         Units = units,
         Contact_Event_Category = eventtype) %>%
  select(13, 14, 3, 5, 11, 12, 9, 10, 8)

# Splitting each individual patient's data into isolated data frames to inspect and manually filter out extraneous terms

Pt_Proc_OD <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Olivia Davies") %>%
  filter(Contact_Event_Term %in% c("Depressive disorders",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Bisacodyl",
                                   "Escitalopram"))

Pt_Proc_TW <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Thomas Williams") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Essential hypertension",
                                   "Atorvastatin"))

Pt_Proc_EJ <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Emily Johnson") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Atorvastatin",
                                   "Citalopram"))

Pt_Proc_DO <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Debelah Oluwaseyi") %>%
  filter(Contact_Event_Term %in% c("Depressive disorders",
                                   "HbA1c level",
                                   "Serum LDL chol. level"))

Pt_Proc_JM <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Jack Murphy") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "HbA1c level",
                                   "Atorvastatin",
                                   "Citalopram"))

Pt_Proc_ZS <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Zara Singh") %>%
  filter(Contact_Event_Term %in% c("Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Atorvastatin",
                                   "Citalopram"))

Pt_Proc_GT <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Georgina Taylor") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level"))

Pt_Proc_RD <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Rhea Desai") %>%
  filter(Contact_Event_Term %in% c("COLD",
                                   "Constipation",
                                   "HbA1c level"))

Pt_Proc_HE <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Harriet Evans") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Atorvastatin",
                                   "Candesartan"))

Pt_Proc_AN <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Amara Nwachukwu") %>%
  filter(Contact_Event_Term %in% c("Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level"))

Pt_Proc_IC <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Isla Campbell") %>%
  filter(Contact_Event_Term %in% c("Essential hypertension",
                                   "Depressive disorders",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Atorvastatin",
                                   "Bisoprolol"))

Pt_Proc_SS <- Raw_Extr_df_Filter %>%
  filter(Patient_Name == "Saesha Sharma") %>%
  filter(Contact_Event_Term %in% c("Diabetes type 2",
                                   "Essential hypertension",
                                   "HbA1c level",
                                   "Serum LDL chol. level",
                                   "Atorvastatin"))

# Binding all individual patient data frames together

Pt_Proc_Long <- rbind(Pt_Proc_OD,
                      Pt_Proc_TW,
                      Pt_Proc_EJ,
                      Pt_Proc_DO,
                      Pt_Proc_JM,
                      Pt_Proc_ZS,
                      Pt_Proc_GT,
                      Pt_Proc_RD,
                      Pt_Proc_HE,
                      Pt_Proc_AN,
                      Pt_Proc_IC,
                      Pt_Proc_SS)

# Transforming combined long format data frame into wider format appropriate for the shiny visualisation tools

initial_events <- Pt_Proc_Long %>%
  group_by(Patient_Name) %>%
  summarise(
    First_Diagnosis = unique(Contact_Event_Term[Contact_Record_Category == "Diagnosis/Review"])[1],
    Second_Diagnosis = unique(Contact_Event_Term[Contact_Record_Category == "Diagnosis/Review"])[2],
    
    First_Biomedical_Test = unique(Contact_Event_Term[Contact_Record_Category == "Biomedical Assay"])[1],
    Second_Biomedical_Test = unique(Contact_Event_Term[Contact_Record_Category == "Biomedical Assay"])[2],
    
    First_Drug_Name = unique(Contact_Event_Term[Contact_Record_Category == "Medication"])[1],
    Second_Drug_Name = unique(Contact_Event_Term[Contact_Record_Category == "Medication"])[2]
  )

Pt_Proc_Wide <- Pt_Proc_Long %>%
  left_join(initial_events, by = "Patient_Name") %>%
  group_by(Patient_Name, Contact_Event_Date) %>%
  mutate(

    Diagnosis_1 = if_else(Contact_Event_Term == First_Diagnosis, First_Diagnosis, NA_character_),
    Diagnosis_2 = if_else(Contact_Event_Term == Second_Diagnosis, Second_Diagnosis, NA_character_),
    
    Biomedical_Test_1 = if_else(Contact_Event_Term == First_Biomedical_Test, First_Biomedical_Test, NA_character_),
    Biomedical_Value_1 = if_else(Contact_Event_Term == First_Biomedical_Test, as.numeric(Value), NA_real_),
    Biomedical_1_Units = if_else(Contact_Event_Term == First_Biomedical_Test, Units, NA_character_),
    
    Biomedical_Test_2 = if_else(Contact_Event_Term == Second_Biomedical_Test, Second_Biomedical_Test, NA_character_),
    Biomedical_Value_2 = if_else(Contact_Event_Term == Second_Biomedical_Test, as.numeric(Value), NA_real_),
    Biomedical_2_Units = if_else(Contact_Event_Term == Second_Biomedical_Test, Units, NA_character_),
    
    Drug_Name_1 = if_else(Contact_Event_Term == First_Drug_Name, First_Drug_Name, NA_character_),
    Drug_1_Value = if_else(Contact_Event_Term == First_Drug_Name, as.numeric(Value), NA_real_),
    Drug_1_Units = if_else(Contact_Event_Term == First_Drug_Name, Units, NA_character_),
    
    Drug_Name_2 = if_else(Contact_Event_Term == Second_Drug_Name, Second_Drug_Name, NA_character_),
    Drug_2_Value = if_else(Contact_Event_Term == Second_Drug_Name, as.numeric(Value), NA_real_),
    Drug_2_Units = if_else(Contact_Event_Term == Second_Drug_Name, Units, NA_character_)
  ) %>%
  ungroup() %>%
  distinct() %>%
  select(-c(5:9))

Pt_Proc_Wide$Contact_Event_Date <- as.Date(Pt_Proc_Wide$Contact_Event_Date, format = "%d/%m/%Y")
Pt_Proc_Wide$Patient_DOB <- as.Date(Pt_Proc_Wide$Patient_DOB, format = "%d/%m/%Y")

# Now, reformat it explicitly to the required format as a character string
Pt_Proc_Wide$Contact_Event_Date <- format(Pt_Proc_Wide$Contact_Event_Date, "%Y-%m-%d")
Pt_Proc_Wide$Patient_DOB <- format(Pt_Proc_Wide$Patient_DOB, "%Y-%m-%d")

# Check if the transformation was successful
str(Pt_Proc_Wide)
head(Pt_Proc_Wide$Contact_Event_Date)

# Saving the newly processed data frame for loading into the RMD Shiny App

write.xlsx(Pt_Proc_Wide, 
           here("data", "processed_extracts", "Proc_Patient_Extract.xlsx"), 
           sheetName = "Processed Data", 
           colNames = TRUE, 
           append = FALSE)
