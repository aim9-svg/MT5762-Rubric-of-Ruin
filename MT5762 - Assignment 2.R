# MT5762 Assignment 2 - Rubric of Ruin

# Install/library required packages
install.packages("tidyverse")
library(tidyverse)

# Import data 
babies <- readxl::read_xlsx("Original Data.xlsx")

# Clean data
clean_babies <- babies %>%
  select(-id, -pluralty, -outcome, -date, -sex) %>%
  rename(baby_wt = wt..7, previous_preg = parity, m_race = race, m_age = age,
         m_edu = ed, m_ht = ht, m_wt = wt..13, d_race = drace, d_age = dage, d_edu = ded,
         d_ht = dht, d_wt = dwt, income = inc, smoke_time = time, cig_number = number)

# Filter function to remove unknown data (doesn't work - ignore)
# remove_unknown <- function (var, data = clean_babies) {
#   test <- data %>% 
#     filter(var != 9 | var != 98 | var != 99 | var != 999)
#   return(test)
# }

# Explore data (simple version)
# Relationship: baby weight and smoke
explore_babies <- clean_babies %>%
  filter(smoke != 9) %>%
  group_by(smoke) %>%
  summarise(bbwt_mean = mean(baby_wt, na.rm = TRUE))

# Relationship: baby weight and mother weight
m_wt_filter <- filter(clean_babies, (m_wt != 999))
bb_m_weight <- lm(baby_wt ~ m_wt, data = bb_m_filter)
bb_m_weight_summary <- summary(bb_m_weight)

bb_m_wt_plot <- ggplot(bb_m_filter, aes(m_wt, baby_wt)) + geom_point() + geom_smooth(method = lm)