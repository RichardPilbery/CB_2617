# Combine all datasets into a single dataframe to convert to event log

library(tidyverse)


# IUC data ----------------

iuc_df <- readRDS('data/iuc_df.rds')

iuc_df1 <- iuc_df %>%
  rename(
    start_date = tbl_YAS_IUC_start_date,
    end_date = tbl_YAS_IUC_end_date,
    diagnosis = SymptomGroup,
    discharge = final_dx_description,
    treatment = Symdisc,
    investigation = first_pathway_selected,
    follow_up = index_referral_service
  ) %>%
  mutate(
    site = if_else(is_index_call == "1", 'indexIUC', 'IUC')
  ) %>%
  select(
    starts_with('index_'),
    person_id,
    start_date,
    end_date,
    diagnosis,
    discharge,
    treatment,
    investigation,
    follow_up,
    site
  )

iuc_df1 %>% count(site)
# site         n
# <chr>    <int>
# 1 IUC       2237
# 2 indexIUC  7050


# GP data ----------------

gp_df <- readRDS('data/gp.rds') %>%
  rename(
    start_date = tbl_SRCode_start_date,
    end_date = tbl_SRCode_end_date,
    discharge = CTV3Text_joined,
  ) %>%
  mutate(
    diagnosis = NA_character_,
    treatment = NA_character_,
    investigation = NA_character_,
    follow_up = NA_character_,
    site = 'GP'
  ) %>% select(-index_referral_service)


# Dental data -----------

dental_df <- readRDS('data/all_iuc_dental_df.rds') %>%
  filter(dental_contact == 'yes')

dental_df1 <- dental_df %>%
  rename(
    treatment = TREATMENT_CHARGE_BAND,
    follow_up = NICE_RECALL_GROUP,
    start_date = tbl_Dental_Data_NHBSA_start_date,
    end_date = tbl_Dental_Data_NHBSA_end_date
  ) %>% 
  mutate(
    diagnosis = as.character(RADIOGRAPH_FLAG),
    investigation = NA_character_,
    site = 'Dentist'
  ) %>%
  select(
    starts_with('index_'),
    -index_referral_service,
    start_date,
    end_date,
    person_id,
    site,
    diagnosis,
    treatment,
    investigation,
    follow_up
  )

# ED data ---------------


ed_df <- readRDS('data/all_ed_df.rds') %>%
  select(-hosp, -index_referral_service)


# Final dataset --------------

all_df <- bind_rows(iuc_df1, ed_df, gp_df, dental_df1)

all_df %>% count(site)
# site         n
# <chr>    <int>
#   1 Dentist   2385
# 2 ED        2808
# 3 GP       13428
# 4 IUC       2237
# 5 indexIUC  7050


all_df %>% saveRDS('data/combined_data_for_event_log_df.rds')
