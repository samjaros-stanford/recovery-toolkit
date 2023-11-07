library(readxl)
library(tidyverse)

raw_data = read_xlsx("data/RecoveryToolkitClean_DATA__POD1_FU MEDD.xlsx")
# Issue with import expecting numeric but got a date

clean_data = raw_data %>%
  filter(age>=18) %>%
  # Prepare survey_type columns for combination
  mutate(record_id = record_id,
         mrn = single_mrn,
         n_surg_types = rowSums(across(starts_with("survey_type__")), na.rm=T),
         surgery_type_fusion            = if_else(survey_type___1==1,  "Fusion", ""),
         surgery_type_laminectomy       = if_else(survey_type___2==1,  "Laminectomy", ""),
         surgery_type_disectomy         = if_else(survey_type___3==1,  "Discectomy", ""),
         surgery_type_fractureReduction = if_else(survey_type___4==1,  "Fracture reduction", ""),
         surgery_type_foraminotomy      = if_else(survey_type___5==1,  "Foraminotomy", ""),
         surgery_type_decompression     = if_else(survey_type___6==1,  "Decompression", ""),
         surgery_type_laminotomy        = if_else(survey_type___7==1,  "Laminotomy", ""),
         surgery_type_osteotomy         = if_else(survey_type___8==1,  "Osteotomy", ""),
         surgery_type_biopsy            = if_else(survey_type___9==1,  "Biopsy", ""),
         surgery_type_kyphoplasty       = if_else(survey_type___10==1, "Kyphoplasty", ""),
         surgery_type_facetectomy       = if_else(survey_type___11==1, "Facetectomy", ""),
         surgery_type_other             = if_else(survey_type___99==1, "Other", ""),
  # Renaming/factoring
  # DEMOGS
         age = age,
         age_cat = factor(case_when(
           age<40 ~ "<40",
           age<60 ~ "41-60",
           age<70 ~ "61-70",
           age<80 ~ "71-80",
           age>80 ~ ">80",
           T      ~ NA_character_
         ), levels=c("<40", "41-60", "61-70", "71-80", ">80")),
         gender = factor(kit_gender_v2, levels=c(1,0), labels=c("Female", "Male")),
         raceeth = factor(
           case_when(
             kit_ethnicity_v2==2                       ~ "White",
             kit_ethnicity_v2==6                       ~ "Hispanic/Latino",
             kit_ethnicity_v2%in%c(3,5)                ~ "AAPI",
             kit_ethnicity_v2==4                       ~ "African American",
             kit_ethnicity_other_v2=="Patient Refused" ~ NA_character_,
             kit_ethnicity_v2%in%c(1,99)               ~ "Other",
             T                                         ~ NA_character_),
           levels=c("White","Hispanic/Latino","AAPI","African American","Other")),
  # TREATMENT
         tx_group = factor(toolkit_yes, levels=c(2,1), labels=c("Control", "RTK")),
         tx_received = factor(kit_receive, levels=c(0,1), labels=c("Not received", "Received")),
         tx_used = factor(kit_use, levels=c(0,1), labels=c("Not used", "Used")),
         tx_info_satisfaction = factor(kit_info_satisfaction),
         tx_recommend = factor(kit_recommend, levels=1:5, labels=c("Definitely continue", "Probably continue", "Neutral", "Probably do not continue", "Definitely do not continue")),
         admission_date = kit_admission_date_v2,
         surgery_date = kit_surgery_date_v2,
         surgical_unit = factor(kit_surgical_unit, levels=c(1), labels=c("Spine")),
         length_of_stay = kit_los_v2,
         pod1_med = as.numeric(`POD1 MEDD`),
         pod1_taking_opioids = factor(pod1_med>0, levels=c(F, T), labels=c("No Opioids", "Opioids")),
  # PAIN
         pain_yesno = factor(kit_pain_v2, levels=c(0,1), labels=c("No pain", "Pain")),
         pain_care_satisfaction = factor(kit_satisfaction_v2, levels=1:6, labels=6:1),
  # READMISSION
         readmission = factor(readmission, levels=c(0,1), labels=c("None", "Readmit")),
         readmission_date = kit_admission_date_v3,
  # FOLLOWUP
         fu_complete = !is.na(fu_date),
         fu_date = fu_date,
         fu_pain_yesno = factor(pain_yes_no, levels=0:1, labels=c("No pain", "Pain")),
         fu_pain_level = pain_level,
         fu_taking_opioids = factor(opioid_yes_no, levels=0:1, labels=c("No opioids", "Opioids")),
         fu_n_opioids = num_opioids,
         fu_med = as.numeric(`FU MEDD 2`),
         fu_tx_used = kit_used_yesno==1,
         fu_tx_use_relaxApp     = if_else(is.na(kit_used_yesno), NA, kit_info_used___1==1),
         fu_tx_use_videos       = if_else(is.na(kit_used_yesno), NA, kit_info_used___2==1),
         fu_tx_use_book         = if_else(is.na(kit_used_yesno), NA, kit_info_used___3==1),
         fu_tx_use_personalPlan = if_else(is.na(kit_used_yesno), NA, kit_info_used___4==1),
         fu_tx_use_other        = if_else(is.na(kit_used_yesno), NA, kit_info_used___5==1),
         fu_tx_useText_other = kit_other,
         fu_tx_notUse_text = kit_not_used_why,
         fu_tx_location_hospital = if_else(is.na(kit_used_yesno), NA, kit_engage_location___1==1),
         fu_tx_location_home     = if_else(is.na(kit_used_yesno), NA, kit_engage_location___2==1),
         fu_tx_location_both     = if_else(is.na(kit_used_yesno), NA, kit_engage_location___3==1),
         fu_tx_location_other    = if_else(is.na(kit_used_yesno), NA, kit_engage_location___4==1),
         fu_tx_location = case_when(
           fu_tx_location_both     ~ "Both",
           fu_tx_location_home     ~ "Home",
           fu_tx_location_hospital ~ "Hospital",
           fu_tx_location_other    ~ "Other",
           T                       ~ NA_character_
         ),
         fu_tx_locationText_other = kit_engage_other,
  .keep="none")

# Write csv, turning lists into strings
write.csv(clean_data, file="data/primary_data.csv", row.names=F)
saveRDS(clean_data, file="data/primary_data.rds")
