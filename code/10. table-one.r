# Summary tables
# Probs one for patient journey related/demographic
# Another with a deep dive into dental data

library(tidyverse)
library(gtsummary)
library(lubridate)
library(readxl)

event_df <- readRDS('data/event_log_df.rds')
dental_df <- readRDS('data/all_iuc_dental_with_LSOA_df.rds')

dental_loc_df <- read_xlsx('data/dental_full_nhs_address_cleaned_DM.xlsx') %>%
  filter(Type %in% c("private", "ortho", "secondary", "urgent", "prmiary") | grepl("primary", Type)) %>%
  mutate(
    Type = if_else(Type == "prmiary", "primary", Type)
  ) %>% select(Postcode, Type)

dental_df1 <- dental_df %>%
  left_join(dental_loc_df, by=c("TRT_LOCATION_POSTCODE"="Postcode"))


# Age bins: 0-10, 11-20, 20-60, > 60 
# Follow all cases (index call to 24 hours and 7 days) 

table1_df <- dental_df1 %>%
  mutate(
    age = as.integer(index_age),
    age_bin = case_when(
      between(age, 0, 10) ~ "0-10",
      between(age, 11, 20) ~ "11-20",
      between(age, 21, 60) ~ "21-60",
      age > 60 ~ ">60",
      .default = NA_character_
    ),
    age_bin = factor(age_bin, levels = c("0-10", "11-20", "21-60", ">60")),
    sex = index_sex,
    IUC_service_referral = index_referral_group,
    ethnicity = fct_relevel(str_extract(index_ethnicity, "[^:]+"), 'White'),
    IMD_decile = factor(index_imd_decile, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")),
    dental_date = tbl_Dental_Data_NHBSA_start_date,
    #across(ends_with('_TEETH'), as.integer),
    EXEMPTION_REMISSION_FLAG = str_replace(EXEMPTION_REMISSION_FLAG, " -.*", "")
  ) %>%
  select(-age, -starts_with('index_'), -starts_with('tbl_'), -dental, index_call_date, index_call_ref) %>%
  group_by(index_call_ref) %>%
  mutate(
    time_to_dental = as.numeric(min(difftime(dental_date, index_call_date, units = 'days'), na.rm = T)),
  ) %>% ungroup() %>%
  mutate(
    time_to_dental = if_else(is.infinite(time_to_dental), NA_real_, time_to_dental),
    dental_24 = if_else(time_to_dental < 1, 'yes', 'no'),
    dental_7 = if_else(time_to_dental < 7, 'yes', 'no')
  ) %>% select(
    -index_call_date, 
    -index_call_ref, 
    -dental_date, 
    -person_id,
    -TRT_LOCATION_POSTCODE,
    -TREATMENT_COMPLETED,
    -LSOA
  )


table1_df %>% 
  select(age_bin, sex, ethnicity, IMD_decile, IUC_service_referral, time_to_dental, dental_7, dental_24, `Dental practice type`, everything()) %>%
  tbl_summary(
    by = 'dental_contact',
    sort = list(
      all_categorical() ~ "frequency",
      c("age_bin", "IMD_decile") ~ "alphanumeric"
    )
  )










