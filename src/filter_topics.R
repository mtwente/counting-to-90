# filter for 'northatlantic' topics

# as control variable
northatlantic_ft <- northatlantic_ft %>%
  mutate(
    opposition_home = ifelse(gvt_party_at_home == TRUE, 0, 1)
  )

# descriptive results
prop.test(
  table(northatlantic_ft$participation, northatlantic_ft$nord_topic)
)

nord_topic_table <- northatlantic_ft %>%
  group_by(nord_topic) %>%
  summarise(participation_rate = scales::percent(mean(participation, na.rm = TRUE), accuracy = 0.1)) %>%
  filter(!row_number() %in% c(3))

# try out a glm
nord_topics_model <- glm(participation ~ nord_topic,
                         family = binomial,
                         data = northatlantic_ft)

nord_topics_model_control <- glm(participation ~ nord_topic + cabinet_parties_seats + opposition_home,
    family = binomial,
    data = northatlantic_ft)

modelsummary(
  list(nord_topics_model, nord_topics_model_control),
  statistic = "({std.error})",
  ci_method = "wald",
  stars = TRUE,
  output = "markdown")