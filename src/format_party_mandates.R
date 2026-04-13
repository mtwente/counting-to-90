library(readxl)
library(here)

mandates <- read_xlsx(here("data", "raw", "party_mandates.xlsx"))

# calculate time span end dates
mandates <- mandates %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(
    start = date,
    end = lead(date) - 1
  ) %>%
  mutate(
    end = if_else(is.na(end), Sys.Date(), end)
  ) %>%
  select(-date) %>%
  relocate(start) %>%
  relocate(end, .after = start)

saveRDS(mandates, here("data", "processed", "party_mandates.rds"))