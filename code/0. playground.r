# playground

library(DBI)
library(dbplyr)
library(bigrquery)
suppressPackageStartupMessages(library(tidyverse))
library(lubridate)
library(readxl)
library(knitr)
library(IRdisplay)
library(htmltools)

con <- DBI::dbConnect(
  bigquery(), 
  project = "yhcr-prd-bradfor-bia-core", 
  bigint = "character", 
  page_size = 5000,
  dataset = 'CB_2617'
)


iuc_tbl <- tbl(con, "tbl_YAS_IUC")



