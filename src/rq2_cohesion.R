# load data and setup using _setupR.R
library(dplyr)
library(tidyr)
library(ggplot2)
library(irr)

# RQ2: Cohesion

## define bloc affiliations
### defined in _setupR.R include

## filter/subset original dataset
northatlantic_ft_present_only <- northatlantic_ft %>%
  filter(participation == TRUE)

## add GL/FO party blocs
northatlantic_ft_present_only <- northatlantic_ft_present_only %>%
  mutate(
    party_bloc = case_when(
      party %in% left_parties   ~ "left",
      party %in% center_parties ~ "center",
      party %in% right_parties  ~ "right",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(party_bloc, .after = party)

## pivot wider
northatlantic_ft_participation_wide <- northatlantic_ft_present_only %>%
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

# find out number of complete cases (= two or more MPs vote on the same proposal)
## it's 64 out of 727, aka 8.8%
sum(complete.cases(cohesion_df$cohesion))

# report mean
mean(cohesion_df$cohesion, na.rm = TRUE)

# plot histogram
ggplot(cohesion_df, aes(x = cohesion)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()

# Compare Within-Bloc vs. Origin Agreement

pairs_df <- northatlantic_ft_present_only %>%
  select(roll_call_id, party, origin, vote_type_id, party_bloc, gvt_bloc_dk) %>%
  mutate(party = as.character(party)) %>%
  inner_join(., ., by = "roll_call_id", suffix = c("_1", "_2"), relationship = "many-to-many") %>%
  filter(party_1 < party_2) %>%  # avoid duplicates and self-pairs
  mutate(
    agree = vote_type_id_1 == vote_type_id_2,
    same_origin = origin_1 == origin_2
  )

 ### reformat gvt_bloc_dk as control variable
pairs_df <- pairs_df %>%
  mutate(party_bloc_dk_gvt = gvt_bloc_dk_1) %>%
  relocate(party_bloc_dk_gvt, .before = agree) %>%
  select(-gvt_bloc_dk_1, -gvt_bloc_dk_2)

origin_agreement_rate <- pairs_df %>%
  group_by(same_origin) %>%
  summarise(
    agreement_rate = round(mean(agree, na.rm = TRUE), digits = 3),
    n = n()
  )
## same_origin = TRUE => cohesion within Greenland / within Faroe Islands
## same_origin = FALSE => cross-origin agreement

# Compare left/right bloc agreement
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
    agreement_rate = round(mean(agree, na.rm = TRUE), digits = 3),
    n = n()
  )
## same_bloc = TRUE => cohesion within parties from the same bloc
## same_bloc = FALSE => cohesion within parties across different blocs

# Compare independence/non-independence blocs
pairs_df <- pairs_df %>%
  mutate(
    same_independence_stance = case_when(
      party_1 %in% pro_independence_parties & party_2 %in% pro_independence_parties ~ TRUE,
      party_1 %in% against_independence_parties & party_2 %in% against_independence_parties ~ TRUE,
      TRUE ~ FALSE
    )
  )

pairs_df %>%
  group_by(same_independence_stance) %>%
  summarise(
    agreement_rate = mean(agree, na.rm = TRUE),
    n = n()
  )
## same_independence_stance = TRUE => cohesion across parties with the same stance on independence
## same_independence_stance = FALSE => cohesion across parties with different stances on independence

# Krippendorff's alpha

vote_matrix <- northatlantic_ft_participation_wide %>%
  select(-roll_call_id) %>%
  as.matrix()

# transpose: parties = rows, roll calls = columns
vote_matrix_t <- t(vote_matrix)

krippendorf_result <- as.data.frame(t(unlist(kripp.alpha(vote_matrix_t, method = "nominal"))))

alpha_table <- data.frame(
  Alpha = round(as.numeric(krippendorf_result$value), 3),
  Subjects = krippendorf_result$subjects,
  Raters = krippendorf_result$raters
)