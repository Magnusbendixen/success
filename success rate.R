# Add Success Rate
nflpbp <- nflpbp %>%
  mutate(
    success_rate = ifelse(down == 1 & yards_gained >= ydstogo*0.4 | down == 2 & yards_gained >= ydstogo*0.6 | down == 3 & yards_gained >= ydstogo | down == 4 & yards_gained >= ydstogo, 1, 0))

# Success Rate Run
SR_run <- nflpbp %>%
  filter(play_type != "NA", play_type == "run", ydstogo != "0", ydstogo <= 10, down != "4") %>%
  group_by(play_type, down, ydstogo) %>%
  summarise(rush_attempts = sum(rush_attempt == 1, na.rm = TRUE), success_rate = mean(success_rate, na.rm = TRUE))

# Success Rate Pass
SR_pass <- nflpbp %>%
  filter(play_type != "NA", play_type == "pass", ydstogo != "0", ydstogo <= 10, score_differential <= 10) %>%
  group_by(play_type, down, ydstogo) %>%
  summarise(pass_attempts = sum(pass_attempt == 1, na.rm = TRUE), success_rate = mean(success_rate, na.rm = TRUE))

# Bind data
SR_rates <- rbind(SR_run, SR_pass)

# Create chart
ggplot(SR_rates, aes(x=ydstogo, group = play_type)) +
  geom_smooth(aes(y = success_rate, colour = play_type), se = FALSE) +
  scale_x_reverse() +
  facet_wrap(~down) +
  labs(title = "Success Rate by Yards To Go, by down (2009-2018)",
       x = "Yards To Go",
       y = "Success Rate",
       caption = "Source: nflscrapR")