library(here)
library(tidyverse)
library(glue)

raw <- read_csv(here("data", "awards and grants.csv"))
raw %>% 
  mutate(date = mdy(date)) %>% 
  arrange(date) %>% 
  filter(date > ymd("2014-1-1"),
         !is.na(amount)) %>% 
  mutate(whatfor = case_when(tag == "presentation" ~ "superior presentation",
                             tag == "grant" ~ "research and travel costs",
                             tag == "travel" ~ "travel costs"),
         sentence = glue(
           "{date}. {award}. Awarded to Eric Scott. Total award amount: {amount} for {whatfor}"
         )) %>% 
  select(sentence) %>% 
  write.table(file = here("awards text.txt"), col.names = FALSE, row.names = FALSE, quote = FALSE)

