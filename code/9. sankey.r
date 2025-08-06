# Sankey

library(tidyverse)
library(networkD3)
library(htmlwidgets)

event_log_df <- readRDS('data/event_log_df.rds')

sankey_df <- event_log_df %>%
  filter(lifecycle_id == "complete") %>%
  arrange(index_call_ref, .order) %>%
  group_by(index_call_ref) %>%
  mutate(
    source = site,
    target = lead(site),
    source_num = case_when(
      row_number() == 1 ~ 1,
      .default = as.numeric(row_number()) - 1
    ),
    target_num = case_when(
      row_number() == 1 ~ 1,
      .default = as.numeric(row_number())
    )
  ) %>% ungroup() %>%
  transmute(
    source = paste(source, source_num, sep = "_"),
    target = if_else(is.na(target), "End", target),
    target = paste(target, target_num, sep = "_")
  )

sankey_df1 <- sankey_df %>%
  group_by(source, target) %>%
  summarise(
    counts2 = n()
  ) %>% ungroup() %>%
  rename(
    value = counts2
  ) %>%
  distinct() %>%
  separate(source, c('rawsource', 'iteration'), sep="_", remove = F) %>%
  filter(as.numeric(iteration) < 5) %>%
  select(-rawsource, -iteration)

a <- sankey_df1 %>% distinct(source)
b <- sankey_df1 %>% distinct(target) %>% rename(source = target)
c <- distinct(bind_rows(a, b)) %>% mutate(id = row_number()-1)


sankey_df2 <- sankey_df1 %>%
  left_join(c, by=c("source"="source")) %>%
  select(-source) %>%
  rename(
    source = id
  ) %>%
  left_join(c, by=c("target" = "source")) %>%
  select(-target) %>%
  rename(
    target = id
  ) %>% as.data.frame()

nodes <- c %>%
  rename(
    name = source
  ) %>% as.data.frame() %>%
  mutate(
    label = paste
  )



p <- sankeyNetwork(
  Links = sankey_df2,
  Nodes = nodes,
  Source = 'source',
  Target = 'target',
  Value = 'value',
  NodeID = 'name', 
  fontSize = 12, 
  nodeWidth = 30, 
  units = '', 
  sinksRight = F
)

p

p %>% saveRDS('data/sankey.rds')

saveWidget(p, file = "data/sankey.html", selfcontained = TRUE)



  