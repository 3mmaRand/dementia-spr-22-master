# packages
library(tidyverse)
library(janitor)



# import
patients <- read_csv("data-raw/oasis_longitudinal.csv") %>%
  clean_names()

patients$cdr <- factor(patients$cdr)

# explore
patients %>% ggplot(aes(x = group, y = age)) +
  geom_boxplot()
patients %>% ggplot(aes(x = group, y = age)) +
  geom_violin()
patients %>% ggplot(aes(x = group, y = age)) +
  geom_jitter()



patients %>% ggplot(aes(x = age, y = n_wbv, color = group)) +
  geom_point() +
  geom_smooth(method = "lm")

# normalised brain volumn is related to dementia status.
# demented have lower nbv


patients %>% ggplot(aes(x = e_tiv, y = n_wbv, color = group)) +
  geom_point()

summary(patients$cdr)
table(patients$cdr)

patients %>% ggplot(aes(x = cdr, y = age)) +
  geom_boxplot()
