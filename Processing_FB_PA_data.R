## Process the FB data for physical activity - daily activity

participant_temp_fb <- daily_activity %>%
  distinct(id) %>%
  mutate(participant = row_number())

daily_activity_2 <- daily_activity %>%
  left_join(participant_temp_fb, by = "id") %>%
  select(participant, everything(), -id)

daily_activity_3 <- daily_activity_2 %>%
  select(-total_steps, -total_distance, -tracker_distance, -logged_activities_distance, -very_active_distance, -moderately_active_distance, -light_active_distance, -sedentary_active_distance)


daily_activity_4 <- daily_activity_3 %>%
  group_by(participant) %>%
  summarise(
    avg_vigorous_activity_min = round(mean(very_active_minutes, na.rm = TRUE)),
    avg_moderate_activity_min = round(mean(fairly_active_minutes, na.rm = TRUE)),
    avg_light_activity_min = round(mean(lightly_active_minutes, na.rm = TRUE)),
    avg_sedentary_activity_min = round(mean(sedentary_minutes, na.rm = TRUE)),
    avg_calories_burned = round(mean(calories, na.rm = TRUE))
  )

fb_pa_data_clean <- daily_activity_4