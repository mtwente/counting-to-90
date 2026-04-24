# filter for 'northatlantic' topics

northatlantic_ft <- northatlantic_ft %>%
  mutate(nord_topic = str_detect(ft_topic, regex("grønl.*|færø.*|arktis.*", ignore_case = TRUE)))

northatlantic_ft_without_absences <- northatlantic_ft_without_absences %>%
  mutate(nord_topic = str_detect(ft_topic, regex("grønl.*|færø.*|arktis.*", ignore_case = TRUE)))