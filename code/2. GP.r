# Primary care data wrangling

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

# Fetch codes associated with face-to-face or telephone consultations --------

#gp_codes_df <- read_csv('../data/richard_pilbery-primary-care-clinician-patient-interaction-72708505.csv')
#DBI::dbWriteTable(con, "gp_codes", gp_codes_df, overwrite = TRUE)

# TODO: Some of these codes might need removing e.g. Patient contact administration


gp_sql <- "
  SELECT
    iuc.*,
    pc.*
  FROM
    index_iuc AS iuc
  JOIN tbl_PrimaryCare_SRCode_all AS pc
      ON iuc.person_id = pc.person_id
  INNER JOIN gp_codes GPC
      ON LOWER(pc.CTV3Code) = LOWER(GPC.code)
WHERE CAST(pc.tbl_SRCode_start_date AS DATE)
    BETWEEN CAST(iuc.index_call_date AS DATE)
    AND DATE_ADD(CAST(iuc.index_call_date AS DATE), INTERVAL 6 MONTH)
"
gp_df <- dbGetQuery(con, gp_sql)

gp_df %>% count() # 14954

gp_df %>% count(CTV3Text, sort = T) %>% view()


# Fetch all rows relating to a consultation -----------

#DBI::dbWriteTable(con, "gp_event_id", gp_df %>% distinct(IDEvent) %>% mutate(IDEvent = as.character(IDEvent)), overwrite = T)

gp_sql2 <- "
  SELECT GP.person_id, GP.IDEvent, CTV3Text from tbl_PrimaryCare_SRCode_all AS GP
    INNER JOIN gp_event_id EVENT
    ON GP.IDEvent = EVENT.IDEVENT
"
gp_ctv_df <- dbGetQuery(con, gp_sql2)

gp_ctv_df %>% count() # 54035


gp_df_nested <- gp_df %>%
  nest_join(gp_ctv_df, by = c("person_id", "IDEvent"), name = "gp_rows") %>%
  mutate(
    CTV3Text_joined = purrr::map_chr(
      gp_rows,
      ~ if (nrow(.x) > 0 && "CTV3Text" %in% names(.x)) {
        paste(unique(na.omit(.x$CTV3Text)), collapse = "; ")
      } else {
        NA_character_
      }
    )
  )

gp_df_nested %>% glimpse()

final_df <- gp_df_nested %>%
  mutate(
    tbl_SRCode_end_date = tbl_SRCode_start_date + minutes(10)
  ) %>%
  select(person_id, tbl_SRCode_start_date, tbl_SRCode_end_date, CTV3Text_joined, starts_with('index_')) %>%
  distinct()

final_df %>% count() # 13,428

#final_df %>% saveRDS('data/gp.rds')












