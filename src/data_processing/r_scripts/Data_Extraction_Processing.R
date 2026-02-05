# script for processing raw CIPHA DynAirX data into workable format

library(dplyr)
library(stringr)
library(here)
library(readr)
library(readxl)
library(openxlsx)

# helper functions

extract_strength_mg <- function(x) {
  out <- str_match(x, "([0-9]+(\\.[0-9]+)?)\\s*mg")[, 2]
  suppressWarnings(as.numeric(out))
}

is_bp_term <- function(x) {
  x_low <- str_to_lower(x)
  str_detect(x_low, "blood pressure") & str_detect(x_low, "systolic|diastolic")
}

is_metformin_term <- function(x) {
  x_chr <- as.character(x)
  x_low <- str_to_lower(x_chr)
  ok <- !is.na(x_chr) & x_chr != ""
  ok & (str_detect(x_low, "\\bmetformin\\b") | str_detect(x_low, "\\bglucophage\\b"))
}

random_metformin_strength_mg <- function(n = 1L) {

  sample(c(500, 850, 1000), size = n, replace = TRUE, prob = c(0.70, 0.15, 0.15))
}

random_sertraline_strength_mg <- function(n = 1L) {

  sample(c(50, 100), size = n, replace = TRUE, prob = c(0.75, 0.25))
}

# creating synthetic event rows matching the processed schema

make_synth_rows <- function(patient_name,
                            patient_dob,
                            sex,
                            patient_gender,
                            dates,
                            record_category,
                            event_term,
                            value,
                            units,
                            event_category = "SYNTHETIC",
                            prescription_instruction = NA_character_) {
  tibble(
    Patient_Name = rep(patient_name, length(dates)),
    Patient_DOB = rep(patient_dob, length(dates)),
    Sex = rep(sex, length(dates)),
    Patient_Gender = rep(patient_gender, length(dates)),
    Contact_Event_Date = dates,
    Prescription_Instruction = rep(prescription_instruction, length(dates)),
    Value = value,
    Units = units,
    Contact_Event_Category = rep(event_category, length(dates)),
    Contact_Record_Category = rep(record_category, length(dates)),
    Contact_Event_Term = rep(event_term, length(dates))
  )
}

# ----------------------------
# 1) Load raw data
# ----------------------------
Raw_Extr_df <- read.csv(here("data", "raw_extracts", "Raw_Patient_Extract.csv"), header = TRUE)

-
# definiging terms to include


# assays

assay_terms <- c(
  "Lying diastolic blood pressure (observable entity)",
  "Lying systolic blood pressure (observable entity)",
  "Serum low density lipoprotein cholesterol level (observable entity)",
  "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)"
)

# diagnoses

diagnosis_terms <- c(
  "Essential hypertension (disorder)",
  "Diabetes mellitus type 2 (disorder)",
  "Mixed anxiety and depressive disorder (disorder)",
  "Depressive disorder NEC (disorder)",
  "Depressive disorder (disorder)",
  "Hypercholesterolaemia (disorder)",
  "Hyperlipidaemia (disorder)",
  "Disorder of lipid metabolism (disorder)",
  "Lipid metabolism disorder (disorder)"
)

# prescriptions

lipid_drug_terms <- c(
  "Atorvastatin 10mg tablets",
  "Atorvastatin 20mg tablets",
  "Atorvastatin 40mg tablets",
  "Atorvastatin 60mg tablets",
  "Atorvastatin 80mg tablets"
)

htn_drug_terms <- c(
  "Amlodipine 2.5mg tablets",
  "Amlodipine 5mg tablets",
  "Amlodipine 10mg tablets"
)

dm_drug_terms <- c(
  "Metformin 500mg tablets",
  "Metformin 850mg tablets",
  "Metformin 1g tablets",
  "Metformin 500mg modified-release tablets",
  "Metformin 750mg modified-release tablets",
  "Metformin 1g modified-release tablets"
)

mdd_drug_terms <- c(
  "Sertraline 50mg tablets",
  "Sertraline 100mg tablets",
  "Sertraline 50mg capsules",
  "Sertraline 100mg capsules"
)

all_allowed_terms <- c(assay_terms, diagnosis_terms, lipid_drug_terms, htn_drug_terms, dm_drug_terms, mdd_drug_terms)

# filtering and standardisation


Raw_Extr_df_Filter <- Raw_Extr_df %>%
  mutate(
    is_metformin = is_metformin_term(term),
    is_bp = is_bp_term(term)
  ) %>%
  filter(term %in% all_allowed_terms | is_bp | is_metformin) %>%
  mutate(
    Contact_Record_Category = case_when(
      term %in% assay_terms ~ "Biomedical Assay",
      is_bp ~ "Biomedical Assay",
      term %in% diagnosis_terms ~ "Diagnosis/Review",
      term %in% c(lipid_drug_terms, htn_drug_terms, dm_drug_terms, mdd_drug_terms) | is_metformin ~ "Medication",
      TRUE ~ "NotKnown"
    ),
    
    # units
    
    units = case_when(
      term %in% c(lipid_drug_terms, htn_drug_terms, dm_drug_terms, mdd_drug_terms) | is_metformin ~ "mg",
      term == "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)" ~ "mmol/mol",
      is_bp ~ if_else(is.na(units) | units == "", "mmHg", units),
      TRUE ~ units
    ),
    
    # values
    
    value = case_when(
      term %in% c(lipid_drug_terms, htn_drug_terms, dm_drug_terms, mdd_drug_terms) | is_metformin ~ extract_strength_mg(term),
      TRUE ~ suppressWarnings(as.numeric(value))
    ),
    
    value = if_else(is_metformin & is.na(value), random_metformin_strength_mg(dplyr::n()), value),
    
    # harmonising labels
    
    Contact_Event_Term = case_when(
      term == "Lying diastolic blood pressure (observable entity)" ~ "Diastolic BP",
      term == "Lying systolic blood pressure (observable entity)" ~ "Systolic BP",
      term == "Serum low density lipoprotein cholesterol level (observable entity)" ~ "Serum LDL (non-HDL)",
      term == "Haemoglobin A1c level - International Federation of Clinical Chemistry and Laboratory Medicine standardised (observable entity)" ~ "HbA1c",
      
      term == "Essential hypertension (disorder)" ~ "Hypertension",
      term == "Diabetes mellitus type 2 (disorder)" ~ "Type 2 diabetes",
      term %in% c("Mixed anxiety and depressive disorder (disorder)",
                  "Depressive disorder NEC (disorder)",
                  "Depressive disorder (disorder)") ~ "Depression",
      term %in% c("Hypercholesterolaemia (disorder)",
                  "Hyperlipidaemia (disorder)",
                  "Disorder of lipid metabolism (disorder)",
                  "Lipid metabolism disorder (disorder)") ~ "Lipid disorder",
      
      term %in% lipid_drug_terms ~ "Atorvastatin",
      term %in% htn_drug_terms ~ "Amlodipine",
      term %in% dm_drug_terms | is_metformin ~ "Metformin",
      term %in% mdd_drug_terms ~ "Sertraline",
      
      is_bp & str_detect(str_to_lower(term), "systolic") ~ "Systolic BP",
      is_bp & str_detect(str_to_lower(term), "diastolic") ~ "Diastolic BP",
      
      TRUE ~ term
    )
  ) %>%
  
  # same patient mapping
  
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
    TRUE ~ "NotKnown"
  )) %>%
  filter(Patient_Name %in% c(
    "Olivia Davies",
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
    "Saesha Sharma"
  )) %>%
  mutate(Patient_Gender = case_when(
    Sex == "M" ~ "Male",
    Sex == "F" ~ "Female",
    TRUE ~ "NotKnown"
  )) %>%
  rename(
    Patient_DOB = Dob,
    Contact_Event_Date = date,
    Prescription_Instruction = Episodicity,
    Value = value,
    Units = units,
    Contact_Event_Category = eventtype
  ) %>%
  select(
    Patient_Name,
    Patient_DOB,
    Sex,
    Patient_Gender,
    Contact_Event_Date,
    Contact_Record_Category,
    Contact_Event_Term,
    Value,
    Units,
    Prescription_Instruction,
    Contact_Event_Category
  )

set.seed(42)

get_patient_demographics <- function(df, patient_name) {
  d <- df %>% filter(Patient_Name == patient_name) %>% slice(1)
  if (nrow(d) == 0) {
    return(list(Patient_DOB = "1985-01-01", Sex = "F", Patient_Gender = "Female"))
  }
  list(
    Patient_DOB = as.character(d$Patient_DOB[1]),
    Sex = as.character(d$Sex[1]),
    Patient_Gender = as.character(d$Patient_Gender[1])
  )
}

choose_dates_from_patient <- function(df, patient_name, n = 6L) {
  dts <- df %>% filter(Patient_Name == patient_name) %>% pull(Contact_Event_Date) %>% unique()
  dts <- dts[!is.na(dts)]
  if (length(dts) == 0) {

    return(format(seq.Date(as.Date("2024-01-01"), by = "month", length.out = n), "%d/%m/%Y"))
  }
  
  dts <- sort(dts)
  if (length(dts) >= n) return(sample(dts, n, replace = FALSE))
  rep(dts, length.out = n)
}

inject_med_if_missing <- function(df, patient_name, required_diag, med_term, dose_sampler, units = "mg") {
  has_diag <- df %>% filter(Patient_Name == patient_name, Contact_Record_Category == "Diagnosis/Review", Contact_Event_Term == required_diag) %>% nrow() > 0
  has_med <- df %>% filter(Patient_Name == patient_name, Contact_Record_Category == "Medication", Contact_Event_Term == med_term) %>% nrow() > 0
  
  if (has_diag && !has_med) {
    demo <- get_patient_demographics(df, patient_name)
    dates <- choose_dates_from_patient(df, patient_name, n = 6L)
    synth <- make_synth_rows(
      patient_name = patient_name,
      patient_dob = demo$Patient_DOB,
      sex = demo$Sex,
      patient_gender = demo$Patient_Gender,
      dates = dates,
      record_category = "Medication",
      event_term = med_term,
      value = dose_sampler(length(dates)),
      units = rep(units, length(dates)),
      event_category = "SYNTHETIC"
    )
    return(bind_rows(df, synth))
  }
  df
}

Raw_Extr_df_Filter <- Raw_Extr_df_Filter %>%
  inject_med_if_missing(
    patient_name = "Thomas Williams",
    required_diag = "Type 2 diabetes",
    med_term = "Metformin",
    dose_sampler = random_metformin_strength_mg,
    units = "mg"
  ) %>%
  inject_med_if_missing(
    patient_name = "Saesha Sharma",
    required_diag = "Type 2 diabetes",
    med_term = "Metformin",
    dose_sampler = random_metformin_strength_mg,
    units = "mg"
  )

Raw_Extr_df_Filter <- Raw_Extr_df_Filter %>%
  inject_med_if_missing(
    patient_name = "Olivia Davies",
    required_diag = "Depression",
    med_term = "Sertraline",
    dose_sampler = random_sertraline_strength_mg,
    units = "mg"
  )

rhea_has_any_diag <- Raw_Extr_df_Filter %>% filter(Patient_Name == "Rhea Desai", Contact_Record_Category == "Diagnosis/Review") %>% nrow() > 0
rhea_has_any_med  <- Raw_Extr_df_Filter %>% filter(Patient_Name == "Rhea Desai", Contact_Record_Category == "Medication") %>% nrow() > 0

if (!rhea_has_any_diag && !rhea_has_any_med) {

  rhea_demo <- get_patient_demographics(Raw_Extr_df_Filter, "Rhea Desai")
  dates <- choose_dates_from_patient(Raw_Extr_df_Filter, "Rhea Desai", n = 6L)
  
  domain <- sample(c("t2dm", "depr", "lipids", "htn"), size = 1, prob = c(0.35, 0.30, 0.20, 0.15))
  
  synth_rows <- list()
  
  if (domain == "t2dm") {
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates[1], record_category = "Diagnosis/Review", event_term = "Type 2 diabetes",
                                                            value = NA_real_, units = NA_character_)
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Medication", event_term = "Metformin",
                                                            value = random_metformin_strength_mg(length(dates)), units = rep("mg", length(dates)))

    hba1c_vals <- round(runif(length(dates), min = 45, max = 95))
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Biomedical Assay", event_term = "HbA1c",
                                                            value = hba1c_vals, units = rep("mmol/mol", length(dates)))
  }
  
  if (domain == "depr") {
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates[1], record_category = "Diagnosis/Review", event_term = "Depression",
                                                            value = NA_real_, units = NA_character_)
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Medication", event_term = "Sertraline",
                                                            value = random_sertraline_strength_mg(length(dates)), units = rep("mg", length(dates)))
  }
  
  if (domain == "lipids") {
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates[1], record_category = "Diagnosis/Review", event_term = "Lipid disorder",
                                                            value = NA_real_, units = NA_character_)
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Medication", event_term = "Atorvastatin",
                                                            value = sample(c(10, 20, 40, 80), size = length(dates), replace = TRUE, prob = c(0.25, 0.35, 0.30, 0.10)),
                                                            units = rep("mg", length(dates)))
    ldl_vals <- round(runif(length(dates), min = 2.0, max = 4.8), 1)
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Biomedical Assay", event_term = "Serum LDL (non-HDL)",
                                                            value = ldl_vals, units = rep("mmol/L", length(dates)))
  }
  
  if (domain == "htn") {
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates[1], record_category = "Diagnosis/Review", event_term = "Hypertension",
                                                            value = NA_real_, units = NA_character_)
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Medication", event_term = "Amlodipine",
                                                            value = sample(c(2.5, 5, 10), size = length(dates), replace = TRUE, prob = c(0.20, 0.60, 0.20)),
                                                            units = rep("mg", length(dates)))
    sys <- round(runif(length(dates), min = 130, max = 170))
    dia <- round(runif(length(dates), min = 80, max = 105))
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Biomedical Assay", event_term = "Systolic BP",
                                                            value = sys, units = rep("mmHg", length(dates)))
    synth_rows[[length(synth_rows) + 1]] <- make_synth_rows("Rhea Desai", rhea_demo$Patient_DOB, rhea_demo$Sex, rhea_demo$Patient_Gender,
                                                            dates = dates, record_category = "Biomedical Assay", event_term = "Diastolic BP",
                                                            value = dia, units = rep("mmHg", length(dates)))
  }
  
  Raw_Extr_df_Filter <- bind_rows(Raw_Extr_df_Filter, bind_rows(synth_rows))
}

# Final clean-up after injection

Raw_Extr_df_Filter <- Raw_Extr_df_Filter %>%
  mutate(
    Value = suppressWarnings(as.numeric(Value)),
    Value = if_else(Contact_Record_Category == "Medication" & is.na(Value), 1, Value),
    Units = if_else(Contact_Record_Category == "Medication" & (is.na(Units) | Units == ""), "mg", Units)
  )


# converting to wife format

Pt_Proc_Long <- Raw_Extr_df_Filter

# enforcing clinical coherence between diagnoses and prescriptions

diag_domains <- Pt_Proc_Long %>%
  filter(Contact_Record_Category == "Diagnosis/Review") %>%
  mutate(domain = case_when(
    Contact_Event_Term == "Lipid disorder"   ~ "lipids",
    Contact_Event_Term == "Hypertension"    ~ "htn",
    Contact_Event_Term == "Type 2 diabetes" ~ "t2dm",
    Contact_Event_Term == "Depression"      ~ "depr",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(domain)) %>%
  distinct(Patient_Name, domain)

med_rows <- Pt_Proc_Long %>%
  filter(Contact_Record_Category == "Medication") %>%
  mutate(domain = case_when(
    Contact_Event_Term == "Atorvastatin" ~ "lipids",
    Contact_Event_Term == "Amlodipine"   ~ "htn",
    Contact_Event_Term == "Metformin"    ~ "t2dm",
    Contact_Event_Term == "Sertraline"   ~ "depr",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(domain)) %>%
  semi_join(diag_domains, by = c("Patient_Name", "domain")) %>%
  select(-domain)

non_med_rows <- Pt_Proc_Long %>%
  filter(Contact_Record_Category != "Medication")

Pt_Proc_Long <- bind_rows(non_med_rows, med_rows)

bp_complete_patients <- Pt_Proc_Long %>%
  filter(Contact_Record_Category == "Biomedical Assay",
         Contact_Event_Term %in% c("Systolic BP", "Diastolic BP")) %>%
  group_by(Patient_Name) %>%
  summarise(
    has_systolic = any(Contact_Event_Term == "Systolic BP"),
    has_diastolic = any(Contact_Event_Term == "Diastolic BP"),
    .groups = "drop"
  ) %>%
  filter(has_systolic & has_diastolic) %>%
  pull(Patient_Name)

Pt_Proc_Long <- Pt_Proc_Long %>%
  filter(!(Contact_Record_Category == "Diagnosis/Review" &
             Contact_Event_Term == "Hypertension" &
             !(Patient_Name %in% bp_complete_patients))) %>%
  filter(!(Contact_Record_Category == "Biomedical Assay" &
             Contact_Event_Term %in% c("Systolic BP", "Diastolic BP") &
             !(Patient_Name %in% bp_complete_patients)))

initial_events <- Pt_Proc_Long %>%
  group_by(Patient_Name) %>%
  summarise(
    First_Diagnosis  = unique(Contact_Event_Term[Contact_Record_Category == "Diagnosis/Review"])[1],
    Second_Diagnosis = unique(Contact_Event_Term[Contact_Record_Category == "Diagnosis/Review"])[2],
    
    First_Biomedical_Test  = unique(Contact_Event_Term[Contact_Record_Category == "Biomedical Assay"])[1],
    Second_Biomedical_Test = unique(Contact_Event_Term[Contact_Record_Category == "Biomedical Assay"])[2],
    
    First_Drug_Name  = unique(Contact_Event_Term[Contact_Record_Category == "Medication"])[1],
    Second_Drug_Name = unique(Contact_Event_Term[Contact_Record_Category == "Medication"])[2]
  )

initial_events <- initial_events %>%
  mutate(
    First_Biomedical_Test = if_else(Patient_Name %in% bp_complete_patients, "Systolic BP", First_Biomedical_Test),
    Second_Biomedical_Test = if_else(Patient_Name %in% bp_complete_patients, "Diastolic BP", Second_Biomedical_Test)
  )

Pt_Proc_Wide <- Pt_Proc_Long %>%
  left_join(initial_events, by = "Patient_Name") %>%
  group_by(Patient_Name, Contact_Event_Date) %>%
  mutate(
    Diagnosis_1 = if_else(Contact_Event_Term == First_Diagnosis, First_Diagnosis, NA_character_),
    Diagnosis_2 = if_else(Contact_Event_Term == Second_Diagnosis, Second_Diagnosis, NA_character_),
    
    Biomedical_Test_1  = if_else(Contact_Event_Term == First_Biomedical_Test, First_Biomedical_Test, NA_character_),
    Biomedical_Value_1 = if_else(Contact_Event_Term == First_Biomedical_Test, as.numeric(Value), NA_real_),
    Biomedical_1_Units = if_else(Contact_Event_Term == First_Biomedical_Test, Units, NA_character_),
    
    Biomedical_Test_2  = if_else(Contact_Event_Term == Second_Biomedical_Test, Second_Biomedical_Test, NA_character_),
    Biomedical_Value_2 = if_else(Contact_Event_Term == Second_Biomedical_Test, as.numeric(Value), NA_real_),
    Biomedical_2_Units = if_else(Contact_Event_Term == Second_Biomedical_Test, Units, NA_character_),
    
    Drug_Name_1  = if_else(Contact_Event_Term == First_Drug_Name, First_Drug_Name, NA_character_),
    Drug_1_Value = if_else(Contact_Event_Term == First_Drug_Name, as.numeric(Value), NA_real_),
    Drug_1_Units = if_else(Contact_Event_Term == First_Drug_Name, Units, NA_character_),
    
    Drug_Name_2  = if_else(Contact_Event_Term == Second_Drug_Name, Second_Drug_Name, NA_character_),
    Drug_2_Value = if_else(Contact_Event_Term == Second_Drug_Name, as.numeric(Value), NA_real_),
    Drug_2_Units = if_else(Contact_Event_Term == Second_Drug_Name, Units, NA_character_)
  ) %>%
  ungroup() %>%
  distinct() %>%
  select(-any_of(c("is_metformin", "is_bp")))

Pt_Proc_Wide$Contact_Event_Date <- as.Date(Pt_Proc_Wide$Contact_Event_Date, format = "%d/%m/%Y")
Pt_Proc_Wide$Patient_DOB <- as.Date(Pt_Proc_Wide$Patient_DOB, format = "%d/%m/%Y")

# format explicitly as character strings for downstream consistency

Pt_Proc_Wide$Contact_Event_Date <- format(Pt_Proc_Wide$Contact_Event_Date, "%Y-%m-%d")
Pt_Proc_Wide$Patient_DOB <- format(Pt_Proc_Wide$Patient_DOB, "%Y-%m-%d")

# saving processed data set

write.xlsx(
  Pt_Proc_Wide,
  here("data", "processed_extracts", "Proc_Patient_Extract.xlsx"),
  sheetName = "Processed Data",
  colNames = TRUE,
  append = FALSE
)
