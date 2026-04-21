# RQ3: Closeness/Pivotality

# calculate margin of votes
northatlantic_ft$margin <- abs(northatlantic_ft$ft_for - northatlantic_ft$ft_against)

# blokpolitik as variable
northatlantic_ft <- northatlantic_ft %>%
  mutate(
    cross_bloc_gvt = ifelse(gvt_bloc_dk == "across", 1, 0)
  )

# Model 1a: Closeness/Participation
rq3_closevote <- glm(
  participation ~ close_vote,
  data = northatlantic_ft,
  family = binomial
)

# Model 1b: Closeness/Participation plus Cabinet Type
rq3_closevote_gvt_dk <- glm(
  participation ~ close_vote + cross_bloc_gvt,
  data = northatlantic_ft,
  family = binomial
)

# Model 2a: Cabinet Party Seats/Participation

rq3_cabinet_seats <- glm(
  participation ~ cabinet_parties_seats,
  data = northatlantic_ft,
  family = binomial
)

# Model 2b: Cabinet Party Seats/Participation plus Cabinet Type
rq3_cabinet_seats_with_bloc <- glm(
  participation ~ cabinet_parties_seats + cross_bloc_gvt,
  data = northatlantic_ft,
  family = binomial
)

all_models_rq3 <- list("Close Vote 1" = rq3_closevote,
                       "Close Vote 2" = rq3_closevote_gvt_dk,
                       "Cabinet Seats" = rq3_cabinet_seats,
                       "Cabinet Seats 2" = rq3_cabinet_seats_with_bloc)

modelsummary(
  all_models_rq3,
  coef_map = c(
    "close_voteTRUE" = "Close Voting Result<br />(Margin ≤4)",
    "cross_bloc_gvt" = "Single-Bloc Coalition",
    "cabinet_parties_seats" = "N° of Cabinet Parties'<br /> Seats in Parliament"
  ),
  statistic = "({std.error})",
  stars = TRUE,
  output = "markdown")