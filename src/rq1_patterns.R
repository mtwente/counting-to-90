# load data and setup using _setupR.R
library(dplyr)
library(tidyr)
library(ggplot2)
library(irr)

# RQ1: Participation Before/After 2009

## define end date of Parliamentary Group (start was before dataset inception)
northatlantic_ft <- northatlantic_ft %>%
  mutate(
    post_2009 = date >= as.Date("2009-08-14")
    ## try FT election 2011 instead
    #post_2011 = date >= as.Date("2011-09-15")
  )

## define Greenlandic MPs as treated group (only 1 FO MP was member anyway)
#northatlantic_ft <- northatlantic_ft %>%
#  mutate(
#    treated = origin == "GL"
#  )

## descriptive table
did_df <- northatlantic_ft %>%
  group_by(origin, post_2009) %>%
  summarise(
    participation_rate = mean(participation),
    .groups = "drop"
  )

## reshape
did_wide <- did_df %>%
  tidyr::pivot_wider(
    names_from = c(origin, post_2009),
    values_from = participation_rate
  )

## calculate differences
did_wide <- did_wide %>%
  mutate(
    change_GL = GL_TRUE - GL_FALSE,
    change_FO = FO_TRUE - FO_FALSE,
    diff_in_diff = change_GL - change_FO
  )

did_wide$diff_in_diff