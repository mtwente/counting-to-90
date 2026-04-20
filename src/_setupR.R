# Setup -----
## Packages -----

library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(knitr)
library(forcats)
library(lubridate)
library(scales)
library(showtext)
library(xfun)
library(patchwork)
suppressMessages(library(stargazer))
library(kableExtra)
library(irr)
library(modelsummary)

## Read Data -----

### List of MPs -----
mp_names <- readRDS(here("data", "processed", "MP_names.rds"))

### List of political parties -----
political_parties <- readRDS(here("data", "processed", "political_parties.rds"))

### Voting Records Database -----
northatlantic_ft <- readRDS(here("data", "processed", "northatlantic_ft.rds"))

### Roll Call Database -----
roll_call_results_ft <- readRDS(here("data", "processed", "roll_call_results_ft.rds"))

### Timeline of Mandates -----
MP_dates <- readRDS(here("data", "processed", "MP_dates.rds"))
elections <- readRDS(here("data", "processed", "election_dates.rds"))

### Folketing seats per party per date
party_mandates <- readRDS(here("data", "processed", "party_mandates.rds"))

#### Convert dates
MP_dates <- MP_dates %>%
  mutate(
    start = ymd(start),
    end   = ymd(end),
    # Replace NA in "end" with today's date
    #end   = if_else(is.na(end), Sys.Date(), end)
    # use date of 2026 election announcement instead (nordatlantisk-ft v0.2.0 release)
    end    = if_else(is.na(end), as.Date("2026-02-26"), end)
  )

## Manipulate Data -----

### Avoid confusing MPs with the same last name
MP_dates <- MP_dates %>%
  mutate(
    plotting_name = case_when(
      MP_id == "12283" ~ "A. V. Johannesen",
      MP_id == "6687"  ~ "K. L. Johannesen",
      MP_id == "18688" ~ "A.-M. Høegh-Dam",
      MP_id == "21388" ~ "Q. Høegh-Dam",
      TRUE             ~ surname
    )
  )

northatlantic_ft <- northatlantic_ft %>%
  mutate(
    plotting_name = case_when(
      MP_id == "12283" ~ "A. V. Johannesen",
      MP_id == "6687"  ~ "K. L. Johannesen",
      MP_id == "18688" ~ "A.-M. Høegh-Dam",
      MP_id == "21388" ~ "Q. Høegh-Dam",
      TRUE             ~ surname
    )
  )

### Add number of cabinet seats on date of observation
northatlantic_ft <- northatlantic_ft %>%
  rowwise() %>%
  mutate(
    cabinet_parties_seats = party_mandates$gvt_seats[
      which(date >= party_mandates$start & date <= party_mandates$end)[1]
    ]
  ) %>%
  ungroup() %>%
  relocate(cabinet_parties_seats, .after = gvt_bloc_dk)

### Order origin factor levels for facet order
northatlantic_ft <- northatlantic_ft %>%
  mutate(origin = forcats::fct_rev(origin))

### Order dataset by MP surnames for plotting
northatlantic_ft_ordered <- northatlantic_ft %>%
  mutate(surname = fct_reorder(surname, vote_type_id, .fun = length)) %>%
  mutate(plotting_name = fct_reorder(plotting_name, vote_type_id, .fun = length)) %>%
  mutate(vote_type_id = fct_relevel(vote_type_id, "1", "2", "4", "3"))

### Subset only with votes that were actually cast
northatlantic_ft_without_absences <- subset(northatlantic_ft_ordered, vote_type_id != 3) %>%
  mutate(surname = fct_reorder(surname, vote_type_id, .fun = length)) %>%
  mutate(plotting_name = fct_reorder(plotting_name, vote_type_id, .fun = length))

### Subset Election Dates for Folketinget only

elections_ft <- elections %>%
  filter(election_type == "Folketing Election")

# Define Party Groups

left_parties <- c("IA", "SIU", "E", "C")
center_parties <- c("N")
right_parties <- c("B", "A")

pro_independence_parties <- c("IA", "SIU", "E", "A", "N", "NQ")
against_independence_parties <- c("B", "C")

# Add Derived Variables

northatlantic_ft$participation <- northatlantic_ft$vote_type_id %in% c("1", "2", "4")

northatlantic_ft$close_vote <- abs(northatlantic_ft$ft_for - northatlantic_ft$ft_against) <= 4

northatlantic_ft <- northatlantic_ft %>%
  relocate(participation, .after = vote_type_id) %>%
  relocate(close_vote, .after = ft_absent)
