library(readxl)
library(tidyverse)

#############
# MAIN DATA #
#############
raw_rtkClean = read_excel("data/RecoveryToolkitClean_DATA__POD1_FU MEDD.xlsx")
primary_data = read_csv("data/primary_data.csv")

# See mismatched admission/surgery dates
raw_rtkClean %>%
  filter(kit_admission_date_v2!=kit_surgery_date_v2) %>%
  select(record_id, kit_admission_date_v2, kit_admission_date_v3, kit_surgery_date_v2) %>%
  mutate(admit_surgery_difference = kit_surgery_date_v2 - kit_admission_date_v2) %>%
  View()

# Get mean satisfaction by whether they reported using/receiving the toolkit
primary_data %>%
  group_by(tx_received, tx_used) %>%
  summarize(mean_info_satisf = mean(tx_info_satisfaction, na.rm=T),
            nmissing_info_satisf = sum(is.na(tx_info_satisfaction)))

# Receive & use comments
unique(raw_rtkClean$kit_receive_comment)
unique(raw_rtkClean$kit_use_comment)

# See admission/surgery/follow-up dates
raw_rtkClean%>%
  select(record_id, kit_admission_date_v2, kit_surgery_date_v2, fu_date) %>%
  mutate(fu_time = fu_date - kit_surgery_date_v2) %>%
  View()

############
# MED Data #
############
summary(raw_rtkClean$`POD1 MEDD`)
summary(raw_rtkClean$`FU MEDD 2`)


####################
# READMISSION DATA #
####################
raw_cleanReadmit = read_excel("data/RecoveryToolkitClean-Readmissions_DATA_2022-02-25_1452.xlsx")

readmit_id_compare = clean_data %>%
  filter(readmission=="Readmit") %>%
  select(record_id) %>%
  mutate(inPrimary=T) %>%
  full_join(raw_cleanReadmit %>%
              select(record_id) %>%
              mutate(inReadmit=T),
            by="record_id")
