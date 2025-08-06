# Event log

library(tidyverse)
library(lubridate)
library(bupaverse)

# Prep data -----------------

df <- readRDS('data/combined_data_for_event_log_df.rds') # 27908

df1 <- df %>%
  arrange(index_call_ref, start_date) %>%
  mutate(
    activity_id = paste(site, row_number(), sep="-")
  ) %>%
  filter(index_call_date <= start_date)

df1 %>% count() # 27692

activity_log_df <- df1 %>%
  rename(
    start = start_date,
    complete = end_date
  ) %>%
  arrange(index_call_ref, start) %>%
  activitylog(
    case_id = "index_call_ref",
    activity_id = "site",
    timestamps = c("start", "complete"),
    resource_id = "activity_id"
  )

event_log_df <- activity_log_df %>%
  to_eventlog()

#event_log_df %>% saveRDS('data/event_log_df.rds')
event_log_df <- readRDS('data/event_log_df.rds')

# Process map -------------

pm <- event_log_df %>% process_map()
pm


event_log_df %>%
  process_map(performance(FUN=median, "days"))


event_log_df %>%
  process_map(type = frequency("relative_case"),
              sec = frequency("absolute"))

event_log_df %>%
  process_matrix(frequency("absolute")) %>%
  plot()

