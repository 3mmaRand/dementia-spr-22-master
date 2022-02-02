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

patients %>% filter(is.na(mmse))
patients %>% filter(subject_id == "OAS2_0181")


patients %>% filter(visit == 1) %>%
  ggplot(aes(x = ses, y = educ)) +
  geom_point(alpha = 0.1)


# remove
#    ses because it has 19 missing values
#    the two rows with missing mmse values (one patient)
#    remove uninformative values
patients_noses <- patients %>%
  select(-ses, -hand, -asf) %>%
  filter(!is.na(mmse))

summary(patients_noses)

library(GGally)
patients_noses %>%
  select(-subject_id, -mri_id) %>%
  ggpairs()

patients_noses %>% group_by(group, m_f) %>% count()
