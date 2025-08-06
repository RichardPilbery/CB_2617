# Airedale ED

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

airedale_tbl <- tbl(con, 'tbl_SC_Airdale_ECDS')

airedale_sql <- "
  SELECT
    iuc.*,
    air.*
  FROM
    index_iuc AS iuc
  JOIN tbl_SC_Airdale_ECDS AS air
      ON iuc.person_id = air.person_id
WHERE CAST(air.tbl_SC_Airdale_ECDS_start_date AS DATE)
    BETWEEN CAST(iuc.index_call_date AS DATE)
    AND DATE_ADD(CAST(iuc.index_call_date AS DATE), INTERVAL 6 MONTH)
"

airedale_df <- dbGetQuery(con, airedale_sql)
airedale_df %>% glimpse()
airedale_df %>% count() # 434


airedale_df1 <- airedale_df %>%
  select(
    person_id,
    starts_with('index_'),
    ec_discharge_follow_up,
    ec_diagnosis,
    ec_discharge_destination,
    ec_procedure,
    care_prof,
    tbl_SC_Airdale_ECDS_start_date,
    tbl_SC_Airdale_ECDS_end_date
  ) %>% distinct() %>%
  mutate(
    ed_id = row_number(),
    site = "ED",
    hosp = 'Airedale'
  ) %>%
  separate(
    ec_diagnosis, 
    into = c("diagnosiscode", "diagnosissequence", "relatedcode"),
    sep = "\\^",
    fill = "right"
  )

airedale_df1 %>% count() # 434

airedale_df1 %>% saveRDS('data/airedale_df1.rds')








