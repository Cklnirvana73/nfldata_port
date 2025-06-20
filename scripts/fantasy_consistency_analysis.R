library(nflreadr)
library(dplyr)
library(purrr)
library(nflfastR)
library(glue)
library(tidyr)
library(ggplot2)
library(plotly)

# Load data from 2014 to 2024
years <- 2015:2024
weekly_fp_all <- purrr::map_df(years, calc_fp_weekly)

consistency_df <- weekly_fp_all %>%
  group_by(player_id, player_name) %>%
  summarise(
    games = n(),
    avg_fp = mean(fantasy_points, na.rm = TRUE),
    sd_fp = sd(fantasy_points, na.rm = TRUE),
    cv_fp = sd_fp / avg_fp,
    boom_weeks = sum(fantasy_points >= 20),
    bust_weeks = sum(fantasy_points < 5),
    .groups = "drop"
  ) %>%
  arrange(cv_fp)


# add player = "C.Lamb" then replace his name in the rest of the code with variable player

# Prepare the data
plot_data <- weekly_fp_all %>%
  filter(player_name == "C.Lamb") %>%
  group_by(player_id) %>%
  arrange(game_id) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

# Make the base ggplot
plot2 <- ggplot(plot_data, aes(x = game_number, y = fantasy_points)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
  labs(
    title = "CeeDee Lamb Fantasy Points by Game Played",
    x = "Game Number",
    y = "Fantasy Points"
  ) +
  theme_minimal()

# Add tooltip interactivity with game_id in hover
ggplotly(plot2, tooltip = c("x", "y", "text")) %>%
  style(text = paste("Game ID:", plot_data$game_id), traces = 1)
