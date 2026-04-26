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

rq4_model_control_with_topic <- glm(
  participation ~ opposition_home + cabinet_parties_seats + nord_topic,
  data = northatlantic_ft,
  family = binomial
)

rq4_interaction <- glm(
  participation ~ opposition_home * origin,
  data = northatlantic_ft,
  family = binomial
)

rq4_topic <- glm(
  participation ~ opposition_home + opposition_home:origin + nord_topic,
  data = northatlantic_ft,
  family = binomial
)

# result tables
all_models_rq4 <- list("Model 3a" = rq4_model,
                       "Model 3b" = rq4_model_control,
                       "Model 3c" = rq4_model_control_with_topic,
                       "Model 3d" = rq4_interaction,
                       "Model 3e" = rq4_topic)

modelsummary(
  all_models_rq4,
  coef_map = c(
    "(Intercept)" = "(Intercept)", 
    "opposition_home" = "Opposition Party in GL/FO",
    "cabinet_parties_seats" = "N° of Cabinet Parties'<br /> Seats in Parliament",
    "nord_topicTRUE" = "Relevance",
    "originFO" = "Faroese MPs",
    "opposition_home:originFO" = "Faroese MPs × Home Opposition"
    ),
  statistic = "({std.error})",
  ci_method = "wald",
  #output = "markdown",
  stars = TRUE)