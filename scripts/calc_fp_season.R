library(dplyr)
library(nflfastR)
library(glue)
library(tidyr)
library(nflreadr)


#' Calculate fantasy football points from nflfastR play-by-play data
#'
#' @param season NFL season (e.g., 2023)
#' @param use_tiered_ppr Use tiered PPR scoring (default = TRUE)
#' @param te_premium Add 0.5 points per TE reception (default = TRUE)
#' @param rush_att_bonus Points per rushing attempt (default = 0.25)
#' @param pass_yd, pass_td, pass_int, pick6_penalty Standard passing scoring
#' @param rush_yd, rush_td Rushing scoring
#' @param rec_yd, rec_td, ppr Standard reception scoring (not used if tiered = TRUE)
#' @param fumbles Points lost per fumble (default = -2)
#'
#' @return Dataframe of player fantasy totals
calc_fp_season <- function(season,
                           use_tiered_ppr = TRUE,
                           te_premium = TRUE,
                           rush_att_bonus = 0.25,
                           pass_yd = 0.04,
                           pass_td = 6,
                           pass_int = -2,
                           pick6_penalty = -4,
                           rush_yd = 0.1,
                           rush_td = 6,
                           rec_yd = 0.1,
                           rec_td = 6,
                           ppr = 1,
                           fumbles = -2) {
  
  message(glue("Loading play-by-play data for {season}..."))
  pbp <- nflfastR::load_pbp(season)
  
  # Load roster to get player position (if needed for TE premium)
  roster <- nflreadr::load_rosters(season) %>%
    select(gsis_id, position)
  
  # Filter relevant plays
  pbp_filtered <- pbp %>%
    filter(!is.na(play_type), play_type %in% c("pass", "run"))
  
  # QB Stats
  qb_stats <- pbp_filtered %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(player_id = passer_player_id, player_name = passer_player_name) %>%
    summarise(
      pass_yards = sum(passing_yards, na.rm = TRUE),
      pass_tds = sum(pass_touchdown, na.rm = TRUE),
      pass_ints = sum(interception, na.rm = TRUE),
      pick6 = sum(interception & touchdown == 1, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & passer_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Rushing Stats
  rush_stats <- pbp_filtered %>%
    filter(!is.na(rusher_player_id)) %>%
    group_by(player_id = rusher_player_id, player_name = rusher_player_name) %>%
    summarise(
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds = sum(rush_touchdown, na.rm = TRUE),
      rush_attempts = sum(rush_attempt, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & rusher_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Receiving Stats with Tiered PPR
  rec_stats <- pbp_filtered %>%
    filter(!is.na(receiver_player_id)) %>%
    mutate(
      tiered_ppr_pts = case_when(
        receiving_yards >= 5  & receiving_yards <= 9  ~ 0.5,
        receiving_yards >= 10 & receiving_yards <= 19 ~ 1.0,
        receiving_yards >= 20 & receiving_yards <= 29 ~ 1.5,
        receiving_yards >= 30 & receiving_yards <= 39 ~ 2.0,
        receiving_yards >= 40                         ~ 2.0,
        TRUE ~ 0
      )
    ) %>%
    group_by(player_id = receiver_player_id, player_name = receiver_player_name) %>%
    summarise(
      rec_yards = sum(receiving_yards, na.rm = TRUE),
      rec_tds = sum(touchdown == 1 & pass_touchdown == 1, na.rm = TRUE),
      receptions = sum(complete_pass, na.rm = TRUE),
      tiered_pts = sum(tiered_ppr_pts, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & receiver_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge all and compute fantasy points
  fantasy_df <- full_join(qb_stats, rush_stats, by = c("player_id", "player_name")) %>%
    full_join(rec_stats, by = c("player_id", "player_name")) %>%
    replace_na(list(
      pass_yards = 0, pass_tds = 0, pass_ints = 0, pick6 = 0,
      rush_yards = 0, rush_tds = 0, rush_attempts = 0,
      rec_yards = 0, rec_tds = 0, receptions = 0, tiered_pts = 0,
      fumbles_lost = 0
    )) %>%
    left_join(roster, by = c("player_id" = "gsis_id")) %>%
    mutate(
      tiered_ppr_points = ifelse(use_tiered_ppr, tiered_pts, 0),
      regular_ppr_points = ifelse(!use_tiered_ppr, receptions * ppr, 0),
      te_bonus = ifelse(te_premium & position == "TE", 0.5 * receptions, 0),
      fantasy_points =
        (pass_yards * pass_yd) + (pass_tds * pass_td) + (pass_ints * pass_int) + (pick6 * pick6_penalty) +
        (rush_yards * rush_yd) + (rush_tds * rush_td) + (rush_attempts * rush_att_bonus) +
        (rec_yards * rec_yd) + (rec_tds * rec_td) +
        tiered_ppr_points + regular_ppr_points + te_bonus +
        (fumbles_lost * fumbles)
    ) %>%
    arrange(desc(fantasy_points))
  
  return(fantasy_df)
}


calc_fp_per_game <- function(season,
                             use_tiered_ppr = TRUE,
                             te_premium = TRUE,
                             rush_att_bonus = 0.25,
                             pass_yd = 0.04,
                             pass_td = 6,
                             pass_int = -2,
                             pick6_penalty = -4,
                             rush_yd = 0.1,
                             rush_td = 6,
                             rec_yd = 0.1,
                             rec_td = 6,
                             ppr = 1,
                             fumbles = -2) {
  
  message(glue("Loading play-by-play data for {season}..."))
  pbp <- nflfastR::load_pbp(season)
  
  roster <- nflreadr::load_rosters(season) %>%
    select(gsis_id, position)
  
  pbp_filtered <- pbp %>%
    filter(!is.na(play_type), play_type %in% c("pass", "run"))
  
  # Calculate number of games played for each player (any involvement)
  games_pass <- pbp_filtered %>%
    filter(!is.na(passer_player_id)) %>%
    distinct(player_id = passer_player_id, game_id)
  
  games_rush <- pbp_filtered %>%
    filter(!is.na(rusher_player_id)) %>%
    distinct(player_id = rusher_player_id, game_id)
  
  games_rec <- pbp_filtered %>%
    filter(!is.na(receiver_player_id)) %>%
    distinct(player_id = receiver_player_id, game_id)
  
  games_played <- bind_rows(games_pass, games_rush, games_rec) %>%
    distinct(player_id, game_id) %>%
    group_by(player_id) %>%
    summarise(games_played = n(), .groups = "drop")
  
  # QB Stats
  qb_stats <- pbp_filtered %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(player_id = passer_player_id, player_name = passer_player_name) %>%
    summarise(
      pass_yards = sum(passing_yards, na.rm = TRUE),
      pass_tds = sum(pass_touchdown, na.rm = TRUE),
      pass_ints = sum(interception, na.rm = TRUE),
      pick6 = sum(interception & touchdown == 1, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & passer_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Rushing Stats
  rush_stats <- pbp_filtered %>%
    filter(!is.na(rusher_player_id)) %>%
    group_by(player_id = rusher_player_id, player_name = rusher_player_name) %>%
    summarise(
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds = sum(rush_touchdown, na.rm = TRUE),
      rush_attempts = sum(rush_attempt, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & rusher_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Receiving Stats with Tiered PPR
  rec_stats <- pbp_filtered %>%
    filter(!is.na(receiver_player_id)) %>%
    mutate(
      tiered_ppr_pts = case_when(
        receiving_yards >= 5  & receiving_yards <= 9  ~ 0.5,
        receiving_yards >= 10 & receiving_yards <= 19 ~ 1.0,
        receiving_yards >= 20 & receiving_yards <= 29 ~ 1.5,
        receiving_yards >= 30 & receiving_yards <= 39 ~ 2.0,
        receiving_yards >= 40                         ~ 2.0,
        TRUE ~ 0
      )
    ) %>%
    group_by(player_id = receiver_player_id, player_name = receiver_player_name) %>%
    summarise(
      rec_yards = sum(receiving_yards, na.rm = TRUE),
      rec_tds = sum(touchdown == 1 & pass_touchdown == 1, na.rm = TRUE),
      receptions = sum(complete_pass, na.rm = TRUE),
      tiered_pts = sum(tiered_ppr_pts, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & receiver_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine stats
  fantasy_df <- full_join(qb_stats, rush_stats, by = c("player_id", "player_name")) %>%
    full_join(rec_stats, by = c("player_id", "player_name")) %>%
    replace_na(list(
      pass_yards = 0, pass_tds = 0, pass_ints = 0, pick6 = 0,
      rush_yards = 0, rush_tds = 0, rush_attempts = 0,
      rec_yards = 0, rec_tds = 0, receptions = 0, tiered_pts = 0,
      fumbles_lost = 0
    )) %>%
    left_join(roster, by = c("player_id" = "gsis_id")) %>%
    left_join(games_played, by = "player_id") %>%
    mutate(
      games_played = ifelse(is.na(games_played), 0, games_played),
      tiered_ppr_points = ifelse(use_tiered_ppr, tiered_pts, 0),
      regular_ppr_points = ifelse(!use_tiered_ppr, receptions * ppr, 0),
      te_bonus = ifelse(te_premium & position == "TE", 0.5 * receptions, 0),
      fantasy_points =
        (pass_yards * pass_yd) + (pass_tds * pass_td) + (pass_ints * pass_int) + (pick6 * pick6_penalty) +
        (rush_yards * rush_yd) + (rush_tds * rush_td) + (rush_attempts * rush_att_bonus) +
        (rec_yards * rec_yd) + (rec_tds * rec_td) +
        tiered_ppr_points + regular_ppr_points + te_bonus +
        (fumbles_lost * fumbles),
      fantasy_points_per_game = ifelse(games_played > 0, fantasy_points / games_played, NA_real_)
    ) %>%
    arrange(desc(fantasy_points_per_game))
  
  return(fantasy_df)
}


calc_fp_weekly <- function(season,
                           use_tiered_ppr = TRUE,
                           te_premium = TRUE,
                           rush_att_bonus = 0.25,
                           pass_yd = 0.04,
                           pass_td = 6,
                           pass_int = -2,
                           pick6_penalty = -4,
                           rush_yd = 0.1,
                           rush_td = 6,
                           rec_yd = 0.1,
                           rec_td = 6,
                           ppr = 1,
                           fumbles = -2) {
  
  message(paste0("Loading play-by-play data for ", season, "..."))
  pbp <- nflfastR::load_pbp(season)
  
  roster <- nflreadr::load_rosters(season) %>%
    select(gsis_id, position)
  
  pbp_filtered <- pbp %>%
    filter(!is.na(play_type), play_type %in% c("pass", "run"))
  
  # Weekly fantasy for passers
  qb_weekly <- pbp_filtered %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(game_id, player_id = passer_player_id, player_name = passer_player_name) %>%
    summarise(
      pass_yards = sum(passing_yards, na.rm = TRUE),
      pass_tds = sum(pass_touchdown, na.rm = TRUE),
      pass_ints = sum(interception, na.rm = TRUE),
      pick6 = sum(interception & touchdown == 1, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & passer_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Weekly fantasy for rushers
  rush_weekly <- pbp_filtered %>%
    filter(!is.na(rusher_player_id)) %>%
    group_by(game_id, player_id = rusher_player_id, player_name = rusher_player_name) %>%
    summarise(
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds = sum(rush_touchdown, na.rm = TRUE),
      rush_attempts = sum(rush_attempt, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & rusher_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Weekly fantasy for receivers
  rec_weekly <- pbp_filtered %>%
    filter(!is.na(receiver_player_id)) %>%
    mutate(
      tiered_ppr_pts = case_when(
        receiving_yards >= 5  & receiving_yards <= 9  ~ 0.5,
        receiving_yards >= 10 & receiving_yards <= 19 ~ 1.0,
        receiving_yards >= 20 & receiving_yards <= 29 ~ 1.5,
        receiving_yards >= 30 & receiving_yards <= 39 ~ 2.0,
        receiving_yards >= 40                         ~ 2.0,
        TRUE ~ 0
      )
    ) %>%
    group_by(game_id, player_id = receiver_player_id, player_name = receiver_player_name) %>%
    summarise(
      rec_yards = sum(receiving_yards, na.rm = TRUE),
      rec_tds = sum(touchdown == 1 & pass_touchdown == 1, na.rm = TRUE),
      receptions = sum(complete_pass, na.rm = TRUE),
      tiered_pts = sum(tiered_ppr_pts, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & receiver_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine all weekly stats
  fantasy_weekly <- full_join(qb_weekly, rush_weekly, by = c("game_id", "player_id", "player_name")) %>%
    full_join(rec_weekly, by = c("game_id", "player_id", "player_name")) %>%
    replace_na(list(
      pass_yards = 0, pass_tds = 0, pass_ints = 0, pick6 = 0,
      rush_yards = 0, rush_tds = 0, rush_attempts = 0,
      rec_yards = 0, rec_tds = 0, receptions = 0, tiered_pts = 0,
      fumbles_lost = 0
    )) %>%
    left_join(roster, by = c("player_id" = "gsis_id")) %>%
    mutate(
      tiered_ppr_points = ifelse(use_tiered_ppr, tiered_pts, 0),
      regular_ppr_points = ifelse(!use_tiered_ppr, receptions * ppr, 0),
      te_bonus = ifelse(te_premium & position == "TE", 0.5 * receptions, 0),
      fantasy_points =
        (pass_yards * pass_yd) + (pass_tds * pass_td) + (pass_ints * pass_int) + (pick6 * pick6_penalty) +
        (rush_yards * rush_yd) + (rush_tds * rush_td) + (rush_attempts * rush_att_bonus) +
        (rec_yards * rec_yd) + (rec_tds * rec_td) +
        tiered_ppr_points + regular_ppr_points + te_bonus +
        (fumbles_lost * fumbles)
    ) %>%
    arrange(player_id, game_id)
  
  return(fantasy_weekly)
}




pw_2022 <- calc_fp_weekly(2022)
pg_2022 <- calc_fp_per_game(2022)
py_2022 <- calc_fp_season(2022)
