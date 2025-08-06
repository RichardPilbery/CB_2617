# Calderdale ED

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

calderdale_tbl <- tbl(con, 'tbl_SUS_Calderdale_EC_Backward_Compatible_20220601_to_20230930')

calderdale_sql <- "
  SELECT
    iuc.*,
    c.*
  FROM
    index_iuc AS iuc
  JOIN tbl_SUS_Calderdale_EC_Backward_Compatible_20220601_to_20230930 AS c
      ON iuc.person_id = c.person_id
WHERE CAST(c.tbl_SUS_Calderdale_EC_Backward_Compatible_20220601_to_20230930_start_date AS DATE)
    BETWEEN CAST(iuc.index_call_date AS DATE)
    AND DATE_ADD(CAST(iuc.index_call_date AS DATE), INTERVAL 6 MONTH)
"

calderdale_df <- dbGetQuery(con, calderdale_sql)
calderdale_df %>% glimpse()
calderdale_df %>% count() # 131


calderdale_df1 <- calderdale_df %>%
  mutate(
    diagnosis = pmap_chr(
      across(starts_with("accident_and_emergency_diagnosis_")),
      ~ {
        vals <- c(...)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
      }
    ),
    treatment = pmap_chr(
      across(starts_with("accident_and_emergency_treatment_")),
      ~ {
        vals <- c(...)
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
      }
    ),
    investigation = pmap_chr(
      across(starts_with("accident_and_emergency_investigation_")),
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
    start_date = tbl_SUS_Calderdale_EC_Backward_Compatible_20220601_to_20230930_start_date ,
    end_date = tbl_SUS_Calderdale_EC_Backward_Compatible_20220601_to_20230930_start_date ,
    hrg_code
  ) %>% distinct() %>%
  mutate(
    ed_id = row_number(),
    site = "ED",
    hosp = 'Calderdale'
  )


calderdale_df1 %>% count() # 131
calderdale_df1 %>% glimpse()


calderdale_df1 %>% saveRDS('data/calderdale_df1.rds')







