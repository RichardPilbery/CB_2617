# This script will identify index IUC calls for patients with an entry in the 
# primary care table and with a dental dispostion between 1st October 2022 and
# 31st October 2023

options(repr.plot.width = 14, repr.plot.height = 8) 
options(tibble.width = Inf, width = 300)

# Load libraries -----------

library(DBI)
library(dbplyr)
library(bigrquery)
suppressPackageStartupMessages(library(tidyverse))
library(lubridate)
library(readxl)

# Connect to GCP
con <- DBI::dbConnect(
  bigquery(), 
  project = "yhcr-prd-bradfor-bia-core", 
  bigint = "character", 
  page_size = 5000,
  dataset = 'CB_2617'
)


# Prep IUC query ------------------


iuc_sql <- glue::glue("
WITH base_data AS (
  SELECT a.*
  FROM tbl_YAS_IUC a
  INNER JOIN (SELECT DISTINCT person_id FROM tbl_PrimaryCare_SRCode_all) b
    ON a.person_id = b.person_id
  WHERE a.person_id IS NOT NULL
),

ordered_calls AS (
  SELECT
    *,
    LAG(tbl_YAS_IUC_start_date) OVER (
      PARTITION BY person_id
      ORDER BY tbl_YAS_IUC_start_date
    ) AS previous_call
  FROM base_data
),

session_marks AS (
  SELECT
    *,
    CASE
      WHEN previous_call IS NULL THEN 1
      WHEN DATE_DIFF(CAST(tbl_YAS_IUC_start_date AS DATE), CAST(previous_call AS DATE), MONTH) > 6 THEN 1
      ELSE 0
    END AS new_session
  FROM ordered_calls
),

sessions AS (
  SELECT
    *,
    SUM(new_session) OVER (
      PARTITION BY person_id
      ORDER BY tbl_YAS_IUC_start_date
    ) AS session_id
  FROM session_marks
),

-- Identify the index caseref (first call per session)
index_calls AS (
  SELECT
    person_id,
    session_id,
    tbl_YAS_IUC_start_date AS index_call_date,
    caseref AS index_call_ref,
    age AS index_age,
    sex AS index_sex,
    final_dx_description AS index_disposition,
    referral_service AS index_referral_service,
    patient_home_postcode AS index_home_postcode,
    patient_current_postcode AS index_current_postcode
  FROM (
    SELECT
      person_id,
      session_id,
      age,
      sex,
      patient_home_postcode,
      patient_current_postcode,
      final_dx_description,
      referral_service,
      tbl_YAS_IUC_start_date,
      caseref,
      ROW_NUMBER() OVER (
        PARTITION BY person_id, session_id
        ORDER BY tbl_YAS_IUC_start_date
      ) AS rn
    FROM sessions
  )
  WHERE rn = 1
)

-- Final output
SELECT
  s.person_id,
  s.tbl_YAS_IUC_start_date,
  s.tbl_YAS_IUC_end_date,
  s.caseref,
  s.gp_surgery_id,
  s.patient_home_postcode,
  s.first_pathway_selected,
  s.final_dx_description,
  s.final_symptom_group,
  s.final_symptom_discriminator,
  s.referral_service,
  s.referral_service_type,
  s.direct_booking_service,
  s.rejected_service,
  s.rejected_service_reason,
  ic.index_call_date,
  ic.index_call_ref,
  ic.index_age,
  ic.index_sex,
  ic.index_home_postcode,
  ic.index_current_postcode,
  ic.index_referral_service,
  CASE WHEN s.caseref = ic.index_call_ref THEN 1 ELSE 0 END AS is_index_call
FROM sessions s
JOIN index_calls ic
  ON s.person_id = ic.person_id
  AND s.session_id = ic.session_id
ORDER BY s.person_id, s.tbl_YAS_IUC_start_date;
")

# Fetch IUC data ------------------

#iuc_df <- dbGetQuery(con, iuc_sql)

iuc_df %>% count() # 9287


# Fetch ethnicity and location info ---------

person_df <- tbl(con, "person") %>% collect()
person_LSOA_df <- tbl(con, "personLSOA") %>% collect()
lsoa_csv_df <- tbl(con, "lsoa_csv_tbl") %>% collect()

person_df1 <- person_df %>%
  select(person_id, ethnicity_source_value, birth_datetime) %>%
  left_join(person_LSOA_df) %>%
  left_join(lsoa_csv_df) %>%
  distinct()

person_df1 %>% count(n_distinct(person_id))

# A tibble: 1 × 2
# `n_distinct(person_id)`     n
# <int> <int>
#   1                   49267 49267

iuc_df1 <- iuc_df %>%
  mutate(
    index_referral_group = case_when(
      grepl('Dental CAS', index_referral_service) ~ 'Dental CAS',
      grepl('ED:', index_referral_service) ~ 'ED',
      grepl('GP OOH', index_referral_service) ~ 'GP OOH',
      grepl('GP', index_referral_service) ~ 'GP in-hours',
      grepl('Dental Emergency', index_referral_service) ~ 'Dental Emergency',
      grepl('Dentist|Dental', index_referral_service) ~ 'Dentist',
      grepl('YAS NHS111', index_referral_service) ~ '111 ED verification',
      grepl('UTC|Urgent Care', index_referral_service) ~ 'UTC',
      .default = 'Other'
    )
  ) %>%
  left_join(person_df1)

iuc_df1 %>% count() # 9287


# Lookup for SD and SG ---------

sd_df <- read_csv('data/111-SD.csv')
sg_df <- read_csv('data/111-SG.csv') %>% distinct()

iuc_df2 <- iuc_df1 %>%
  mutate(
    final_symptom_group = paste0('SG', final_symptom_group),
    final_symptom_discriminator = paste0('SD', final_symptom_discriminator)
  ) %>%
  left_join(sd_df, by=c("final_symptom_discriminator"="SD_ID")) %>%
  left_join(sg_df, by=c("final_symptom_group"="SG_ID")) %>%
  mutate(
    index_imd_rank = imd_rank,
    index_imd_decile = imd_decile,
    index_ethnicity = ethnicity_source_value
  )

iuc_df2 %>% count(is.na(LSOA))
# `is.na(LSOA)`     n
# <lgl>         <int>
#   1 FALSE          9028
# 2 TRUE            259

#iuc_df2 %>% saveRDS('data/iuc_df.rds')
iuc_df2 <- readRDS('data/iuc_df.rds')


index_iuc <- iuc_df2 %>% 
  filter(is_index_call == "1") %>%
  select(
    person_id,
    starts_with('index_')#, LSOA
  ) %>% distinct()

index_iuc %>% count() # 7050

#index_iuc %>% saveRDS('data/index_iuc_df.rds')
#DBI::dbWriteTable(con, "index_iuc", index_iuc, overwrite = TRUE)


# Dental data -------------------

dental_tbl <- tbl(con, "tbl_Dental_Data_NHBSA")

dental_tbl %>% summarise(
  min_date = min(tbl_Dental_Data_NHBSA_start_date),
  max_date = max(tbl_Dental_Data_NHBSA_start_date)
)
# Source:   SQL [?? x 2]
# Database: BigQueryConnection
# min_date            max_date           
# <dttm>              <dttm>             
#   1 2006-09-20 00:00:00 2023-11-22 00:00:00

dental_df <- dental_tbl %>%
  filter(
    between(tbl_Dental_Data_NHBSA_start_date, '2022-10-01', '2023-10-31')
  ) %>% collect()

#dental_df %>% saveRDS('data/dental_df.rds')

dental_df %>% summarise(
  min_date = min(tbl_Dental_Data_NHBSA_start_date),
  max_date = max(tbl_Dental_Data_NHBSA_start_date)
)
# A tibble: 1 × 2
# min_date            max_date           
# <dttm>              <dttm>             
#   1 2022-10-01 00:00:00 2023-10-31 00:00:00


dental_df1 <- dental_df %>%
  inner_join(index_iuc, by="person_id", relationship = "many-to-many") %>%
  filter(
    tbl_Dental_Data_NHBSA_start_date >= index_call_date,
    tbl_Dental_Data_NHBSA_start_date <= index_call_date + months(6)
  )

dental_df1 %>% count() # 3409


# Summary of dental data --------------

all_iuc_dental_df <- index_iuc %>%
  left_join(dental_df1 %>% 
              mutate(dental = 1) %>% 
              select(dental, 
                     index_call_ref, 
                     tbl_Dental_Data_NHBSA_start_date,
                     tbl_Dental_Data_NHBSA_end_date,
                     EXEMPTION_REMISSION_FLAG, 
                     TREATMENT_CHARGE_BAND, 
                     NICE_RECALL_GROUP,
                     UNTREAT_DECAY,
                     TRT_LOCATION_POSTCODE,
                     ANTIBIOTICS,
                     TREATMENT_COMPLETED,
                     FILLED_PERMANENT_TEETH,
                     FILLED_DECIDUOUS_TEETH,
                     DECAYED_PERMANENT_TEETH,
                     DECAYED_DECIDUOUS_TEETH,
                     MISSING_PERMANENT_TEETH,
                     MISSING_DECIDUOUS_TEETH,
                     FLEXIBLE_COMMISSIONING_FLAG,
                     SEDATION_FLAG,
                     AEROSOL_GENERATING_PROCEDURES,
                     ENDODONTIC_TEETH,
                     RADIOGRAPH_FLAG,
                     HIGH_BPE_SCORE_IND), by="index_call_ref") %>%
  arrange(index_call_ref, index_call_date) %>%
  group_by(index_call_ref, index_call_date) %>%
  slice(1) %>% # Only return first treatment for this
  ungroup() %>%
  mutate(
    dental_contact = as.factor(if_else(is.na(dental), 'no', 'yes')),
  )


#all_iuc_dental_df %>% saveRDS('data/all_iuc_dental_df.rds')

#all_iuc_dental_df %>% saveRDS('data/all_iuc_dental_with_LSOA_df.rds')


all_iuc_dental_df %>% mutate(
  month = floor_date(index_call_date, unit = 'months')
) %>% count(dental_contact, month) %>%
  ggplot(aes(x = month, y = n, fill = dental_contact)) +
  geom_col()








































