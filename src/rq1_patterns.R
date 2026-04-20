library(dplyr)
library(lubridate)
# https://www.princeton.edu/~otorres/DID101R.pdf

# RQ1: Participation Before/After 2009

northatlantic_ft <- northatlantic_ft %>%
  mutate(
    postgroup2009 = ifelse(date >= as.Date("2009-08-14"), 1, 0),
    postgroup2011 = ifelse(date >= as.Date("2011-09-15"), 1, 0)
  )

northatlantic_ft <- northatlantic_ft %>%
  mutate(
    treated = ifelse(origin == "GL", 1, 0)
  )

# interaction variable between postgroup20* and treatment
northatlantic_ft <- northatlantic_ft %>%
  mutate(
    did2009 = postgroup2009 * treated,
    did2011 = postgroup2011 * treated
  )

# estimate DID models
didreg2009 = lm(participation ~ treated + postgroup2009 + did2009, data = northatlantic_ft)
didreg2011 = lm(participation ~ treated + postgroup2011 + did2011, data = northatlantic_ft)
summary(didreg2009)
summary(didreg2011)

modelsummary(
  list(
    "2009 Cutoff" = didreg2009,
    "2011 Cutoff" = didreg2011
  ),
  coef_map = c(
    "treated" = "Greenland (treated)",
    "postgroup2009" = "Post 2009",
    "postgroup2011" = "Post 2011",
    "did2009" = "GL × Post 2009",
    "did2011" = "GL × Post 2011",
    "treated:postgroup2009" = "GL × Post 2009",
    "treated:postgroup2011" = "GL × Post 2011"
  ),
  statistic = "({std.error})",
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "latex"
)