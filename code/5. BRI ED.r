# BRI ED

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

bri_tbl <- tbl(con, 'tbl_ae')


bri_sql <- "
  SELECT
    iuc.*,
    b.*
  FROM
    index_iuc AS iuc
  JOIN tbl_ae AS b
      ON iuc.person_id = b.person_id
WHERE DATE(PARSE_TIMESTAMP('%F %H:%M:%S', b.tbl_ae_start_date))
    BETWEEN CAST(iuc.index_call_date AS DATE)
    AND DATE_ADD(CAST(iuc.index_call_date AS DATE), INTERVAL 6 MONTH)
"

bri_df <- dbGetQuery(con, bri_sql)
bri_df %>% glimpse()
bri_df %>% count() # 2243


bri_df1 <- bri_df %>%
  mutate(
    diagnosis = pmap_chr(
      across(starts_with("diagnosis_")),
      ~ {
        vals <- c(...)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
      }
    ),
    treatment = pmap_chr(
      across(starts_with("treatment_")),
      ~ {
        vals <- c(...)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
      }
    ),
    investigation = pmap_chr(
      across(starts_with("investigation_")),
      ~ {
        vals <- c(...)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
      }
    ) 
  ) %>%
  select(
    person_id,
    starts_with('index_'),
    investigation,
    diagnosis,
    treatment,
    start_date = tbl_ae_start_date,
    end_date = tbl_ae_end_date,
    hrg_code,
    attendance_disposal_description,
    source_of_referral_description
  ) %>% distinct() %>%
  mutate(
    ed_id = row_number(),
    site = "ED",
    hosp = 'BRI'
  )


bri_df1 %>% count() # 2243
bri_df1 %>% glimpse()


bri_df1 %>% saveRDS('data/bri_df1.rds')







