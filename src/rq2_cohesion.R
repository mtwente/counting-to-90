# load data and setup using _setupR.R
library(dplyr)
library(tidyr)
library(ggplot2)
library(irr)

# RQ2: Cohesion

northatlantic_ft_without_absences <- northatlantic_ft %>%
  filter(participation == TRUE)

northatlantic_ft_participation_wide <- northatlantic_ft_without_absences %>%
  select(roll_call_id, party, vote_type_id) %>%
  distinct() %>%
  pivot_wider(names_from = party, values_from = vote_type_id)

## pairwise agreement

pairwise_agreement <- function(votes) {
  votes <- na.omit(votes)
  if (length(votes) < 2) return(NA)
  
  pairs <- combn(votes, 2)
  mean(pairs[1, ] == pairs[2, ])
}

cohesion_df <- northatlantic_ft_participation_wide %>%
  rowwise() %>%
  mutate(cohesion = pairwise_agreement(c_across(-roll_call_id))) %>%
  ungroup()

# find out number of complete cases (= one or more MPs vote)
## it's 64 out of 727, aka 8.8%
sum(complete.cases(cohesion_df$cohesion))

# report mean
mean(cohesion_df$cohesion, na.rm = TRUE)

# plot histogram
ggplot(cohesion_df, aes(x = cohesion)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()

# Compare Within-Bloc vs. Origin Agreement

pairs_df <- northatlantic_ft_without_absences %>%
  select(roll_call_id, party, origin, vote_type_id) %>%
  mutate(party = as.character(party)) %>%
  inner_join(., ., by = "roll_call_id", suffix = c("_1", "_2"), relationship = "many-to-many") %>%
  filter(party_1 < party_2) %>%  # avoid duplicates and self-pairs
  mutate(
    agree = vote_type_id_1 == vote_type_id_2,
    same_origin = origin_1 == origin_2
  )

pairs_df %>%
  group_by(same_origin) %>%
  summarise(
    agreement_rate = mean(agree, na.rm = TRUE),
    n = n()
  )
## same_origin = TRUE => cohesion within Greenland / within Faroe Islands
## same_origin = FALSE => cross-origin agreement

# Compare left/right bloc

## define bloc affiliation
left_parties <- c("IA", "SIU", "E", "C")
center_parties <- c("N")
right_parties <- c("B", "A")

pairs_df <- pairs_df %>%
  mutate(
    same_bloc = case_when(
      party_1 %in% left_parties & party_2 %in% left_parties ~ TRUE,
      party_1 %in% right_parties & party_2 %in% right_parties ~ TRUE,
      party_1 %in% center_parties & party_2 %in% center_parties ~ TRUE,
      TRUE ~ FALSE
    )
  )

pairs_df %>%
  group_by(same_bloc) %>%
  summarise(
    agreement_rate = mean(agree, na.rm = TRUE),
    n = n()
  )
## same_bloc = TRUE => cohesion within parties from the same bloc
## same_bloc = FALSE => cohesion within parties across different blocs# Krippendorff's alpha

vote_matrix <- northatlantic_ft_participation_wide %>%
  select(-roll_call_id) %>%
  as.matrix()

# transpose: parties = rows, roll calls = columns
vote_matrix_t <- t(vote_matrix)

kripp.alpha(vote_matrix_t, method = "nominal")