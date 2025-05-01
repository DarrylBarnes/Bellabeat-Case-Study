## Process Sleep date from FitBit

sleep_data_2 <- sleep_data %>%
  select(id, value, log_id) %>%
  group_by(id, log_id)

## creating sleep score

sleep_data_3 <- sleep_data_2 %>%
  group_by(id, log_id) %>%
  summarise(
    total_minutes = n(),
    asleep_minutes = sum(value==1),
    restless_minutes = sum(value==2),
    sleep_score = asleep_minutes / total_minutes,
    .groups = "drop")

## renaming columns and transforming coluimns (log_id, id)

sleep_data_4 <- sleep_data_3 %>%
  group_by(id) %>%
  mutate(
    sleep_session = row_number()
  ) %>%
  rename(participant = id) %>% 
  select(-log_id)

## created unique value for distinct pariticipants

participant_temp <- sleep_data_3 %>% 
  distinct(id) %>% 
  mutate(participant = row_number())

## Joining data

sleep_data_5 <- sleep_data_3 %>% 
  left_join(participant_temp, by = "id") %>%
  group_by(participant) %>%
  mutate(sleep_session = row_number()) %>% 
  select(participant, sleep_session, everything(), -id, -log_id)
  
avg_sleep_data_fb <- sleep_data_5 %>%
  group_by(participant) %>%
  summarize(avg_sleep_score = mean(sleep_score, na.rm = TRUE))

## Aggregate data/collect averages per participant

sleep_data_5 <- sleep_data_5 %>%
  select(participant, sleep_session, total_minutes, asleep_minutes, restless_minutes, sleep_score)

FB_sleep_data_clean <- sleep_data_5 %>%
  group_by(participant) %>%
  summarise(
    avg_asleep_hours = round(mean(asleep_minutes, na.rm = TRUE)) / 60,
    avg_restless_minutes = round(mean(restless_minutes, na.rm = TRUE)),
    avg_total_sleep_hours = round(mean(total_minutes, na.rm = TRUE)) / 60,
    avg_sleep_score = mean(sleep_score, na.rm = TRUE)
  )

## Rounding to whole numbers

FB_sleep_data_clean <- FB_sleep_data_clean %>% 
  mutate(
    avg_asleep_hours = round(avg_asleep_hours, 1),
    avg_total_sleep_hours = round(avg_total_sleep_hours, 1),
    avg_sleep_score = round(avg_sleep_score, 2)
  )
