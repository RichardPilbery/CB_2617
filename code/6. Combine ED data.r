# Combine ED data

library(tidyverse)

airedale_df <- readRDS('data/airedale_df1.rds')
calderdale_df <- readRDS('data/calderdale_df1.rds')
bri_df <- readRDS('data/bri_df1.rds')

airedale_df1 <- airedale_df %>%
  rename(
    start_date = tbl_SC_Airdale_ECDS_start_date,
    end_date = tbl_SC_Airdale_ECDS_end_date,
    follow_up = ec_discharge_follow_up,
    discharge = ec_discharge_destination,
    diagnosis = diagnosiscode,
    investigation = ec_procedure
  ) %>%
  select(
    person_id,
    starts_with('index_'),
    start_date,
    end_date,
    follow_up,
    discharge,
    diagnosis,
    investigation,
    hosp,
    site
  ) %>%
  mutate (
    treatment = NA_character_,
    follow_up = as.character(follow_up),
    discharge = as.character(discharge)
  )


calderdale_df1 <- calderdale_df %>%
  select(-ed_id, -hrg_code) %>%
  mutate(
    follow_up = NA_character_,
    discharge = NA_character_
  )


bri_df1 <- bri_df %>%
  rename(
    discharge = attendance_disposal_description
  ) %>%
  select(-hrg_code, -ed_id, -source_of_referral_description) %>%
  mutate(
    start_date = lubridate::ymd_hms(start_date),
    end_date = lubridate::ymd_hms(end_date),
    follow_up = NA_character_
  )


all_ed_df <- bind_rows(airedale_df1, calderdale_df1, bri_df1)  

all_ed_df %>% saveRDS('data/all_ed_df.rds')
