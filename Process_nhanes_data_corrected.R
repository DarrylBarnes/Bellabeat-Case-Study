nhanes_combined <- demo %>%
  inner_join(sleep_nhanes, by = "SEQN") %>%
  inner_join(paq, by = "SEQN")
View(nhanes_combined)

nhanes_female <- nhanes_combined %>%
  filter(RIAGENDR == 2)  #2 Female

## clean column names in nhanes_female dataset

nhanes_female_c <- nhanes_female %>%
  rename(
    id = SEQN,
    age = RIDAGEYR,
    sleep_hours = SLD012,
  ) %>%
  select(id, age, sleep_hours, RIAGENDR, PAD615, PAD630)

## checking for nulls

colSums(is.na(nhanes_female_c))

##My observation is that these null values exist bause these particular women did not report moderate to vigours physical activtty. It makes sense to remove them from the data set and focus on users who are already active. Interestingly, a large portion of women in NHANES data reported no weekly moderate activity. While our current product fits active women best, this reveals a potential growth segment: women who want to become more active but lack structure or tools. Bellabeat could position its app as a gentle introduction to wellness—an entry point for the ‘aspiring active woman.


nhanes_female_active <- nhanes_female_c %>%
  drop_na(PAD615, PAD630)

nhanes_female_active %>%
  filter(RIAGENDR != 2) %>%

nhanes_female_active_clean <- nhanes_female_active %>%
  drop_na(sleep_hours)

nhanes_female_active_clean <- nhanes_female_active %>%
  mutate(age_group = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 25 ~ "18-25",
    age > 25 & age <= 35 ~ "26-35",
    age > 35 & age <= 45 ~ "36-45",
    age > 45 & age <= 55 ~ "46-55",
    age > 55 & age <= 65 ~ "56-65",
    age > 65 ~ "66+"
  ))

nhanes_female_active_clean <- nhanes_female_active_clean[-c(7, 265),]

## make a value for id instead of a SEQN number

participant_temp <- nhanes_female_active %>%
  distinct(id) %>%
  mutate(participant = row_number())

nhanes_female_active_clean <- nhanes_female_active_clean %>%
  left_join(participant_temp, by = "id") %>%
  select(participant, everything(), -id)

nhanes_sleep_pa_data_clean <- nhanes_female_active_clean