# RQ4: Cross-Parliamentary Control

northatlantic_ft <- northatlantic_ft %>%
  mutate(
    opposition_home = ifelse(gvt_party_at_home == TRUE, 0, 1)
  )

# descriptive comparison
rq4_table <- northatlantic_ft %>%
  group_by(opposition_home) %>%
  summarise(
    participation_rate = mean(participation),
    n = n()
  )

# chi^2-test
prop.test(
  table(northatlantic_ft$participation, northatlantic_ft$opposition_home)
)

chisq <- chisq.test(
  table(northatlantic_ft$participation, northatlantic_ft$opposition_home)
)

# logistic regression
rq4_model <- glm(
  participation ~ opposition_home,
  data = northatlantic_ft,
  family = binomial
)

rq4_model_control <- glm(
  participation ~ opposition_home + cabinet_parties_seats,
  data = northatlantic_ft,
  family = binomial
)

#rq4_interaction <- glm(
#  participation ~ opposition_home * origin,
#  data = northatlantic_ft,
#  family = binomial
#)

# result tables

all_models_rq4 <- list("Baseline" = rq4_model,
                       #"Origin-Based" = rq4_interaction,
                       "Cabinet Seats" = rq4_model_control)

modelsummary(
  all_models_rq4,
  coef_map = c(
    "opposition_home" = "Opposition Party in GL/FO",
    "cabinet_parties_seats" = "N° of Cabinet Parties'<br /> Seats in Parliament"
    #"originFO" = "originFO",
    #"opposition_home:originFO" = "opposition_home:originFO"
  ),
  statistic = "({std.error})",
  ci_method = "wald",
  stars = TRUE,
  output = "markdown")