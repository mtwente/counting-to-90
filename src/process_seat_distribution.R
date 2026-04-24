library(dplyr)
library(tidyr)
library(readr)
library(here)

# Read data
election <- read_delim(here("data", "raw", "election_results.csv"), delim = ";")
changes  <- read_delim(here("data", "raw", "mandate_changes.csv"), delim = ";")
# source for mandate_changes.csv: https://folkevalgte.dk/partiskift/
cabinets <- readRDS(here("data", "processed", "cabinets_dk.rds"))

# Reformat input data
election$date <- as.Date(election$date)
changes$date  <- as.Date(changes$date)
cabinets$start <- as.Date(cabinets$start)
cabinets$end <- as.Date(cabinets$end)

changes[is.na(changes)] <- 0

changes[,-1] <- lapply(changes[,-1], as.numeric)

all_dates <- sort(unique(c(election$date, changes$date)))

# Create and prepare result data frame
result <- data.frame(date = all_dates)
parties <- names(election)[-1]

# Empty matrix to store results
res_mat <- matrix(NA, nrow = length(all_dates), ncol = length(parties))
colnames(res_mat) <- parties

current <- NULL

for (i in seq_along(all_dates)) {
  d <- all_dates[i]
  
  # Reset baseline after election dates
  if (d %in% election$date) {
    current <- as.numeric(election[election$date == d, -1])
  }
  
  # apply changes if there are any that day
  if (d %in% changes$date) {
    delta <- colSums(changes[changes$date == d, -1, drop = FALSE])
    current <- current + delta
  }
  
  res_mat[i, ] <- current
}

result <- cbind(result, res_mat)

# specify elections via boolean
result <- result %>%
  mutate(election_date = date %in% election$date) %>%
  relocate(election_date, .after = date)

# add control column (has to be 179)
result <- result %>%
  mutate(total = rowSums(across(c(-date, -election_date)), na.rm = TRUE)) %>%
  relocate(total, .before = socialdemokratiet)

# add cabinet ID per observation
cabinets <- cabinets %>%
  mutate(
    start = as.Date(start),
    end = as.Date(end),
    end = if_else(is.na(end), Sys.Date(), end)
  )

result <- result %>%
  rowwise() %>%
  mutate(
    cabinet = cabinets$cabinet[
      which(date >= cabinets$start & date <= cabinets$end)[1]
    ]
  ) %>%
  ungroup()

# add dates of cabinet changes as observations
new_dates <- as.Date(setdiff(cabinets$start, result$date))

## Create empty rows with same structure
new_rows <- result[rep(1, length(new_dates)), ]  # duplicate first row
new_rows[,] <- NA                                # set everything to NA
new_rows$date <- new_dates                       # fill dates

result <- rbind(result, new_rows)
result <- result[order(result$date), ]

## fill missing column values for dates with new cabinets
### add missing Nyrup Rasmussen cabinet manually
result <- result %>%
  mutate(cabinet = ifelse(date == as.Date("2001-11-20"), "rasmussen-p-n_4", cabinet))

### fill all other cabinets upwarads
result <- result %>%
  fill(last_col(), .direction = "up")

### set election to FALSE for dates of new cabinets
result$election_date[is.na(result$election_date)] <- FALSE

### Fill down all missing seat columns (carry last observation forward)
result <- result %>%
  fill(everything(), .direction = "down")

# calculate time span end dates
result <- result %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(
    start = date,
    end = lead(date) - 1
  ) %>%
  #mutate(
  #  end = if_else(is.na(end), Sys.Date(), end)
  #) %>%
  select(-date) %>%
  relocate(start) %>%
  relocate(end, .after = election_date) %>%
  
  # sort columns
  relocate(cabinet, .after = end)

# add cabinet party seat number
party_map <- c(
  DK_A = "socialdemokratiet",
  DK_B = "radikale_venstre",
  DK_C = "det_konservative_folkeparti",
  DK_F = "socialistisk_folkeparti",
  DK_I = "liberal_alliance",
  DK_M = "moderaterne",
  DK_V = "venstre"
)

dk_cols <- names(party_map)

# join cabinet info into result df
result <- result %>%
  left_join(
    cabinets %>%
      select(cabinet, DK_A, DK_B, DK_C, DK_F, DK_I, DK_M, DK_V),
    by = "cabinet"
  )

# compute government seats
result$gvt_seats <- apply(result, 1, function(row) {
  
  dk_cols <- names(party_map)
  
  # which government parties are active
  active_parties <- dk_cols[row[dk_cols] == "TRUE"]
  
  # map to result column names
  party_cols <- party_map[active_parties]
  
  # sum seats (handle missing / NA safely)
  sum(as.numeric(row[party_cols]), na.rm = TRUE)
})

# clean data frame
result <- result %>%
  select(!starts_with("DK_")) %>%
  relocate(gvt_seats, .after = cabinet) %>%
  # manually remove earliest gvt_seats (out of bounds)
  mutate(gvt_seats = ifelse(start == as.Date("2001-11-20"), NA, gvt_seats))

# export
write_rds(result, here("data", "processed", "party_mandates.rds"))