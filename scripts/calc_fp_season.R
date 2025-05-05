library(dplyr)
library(nflfastR)
library(glue)
library(tidyr)

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

calc_fantasy_fastR <- function(season,
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
  
  # Filter relevant plays
  pbp_filtered <- pbp %>%
    filter(!is.na(play_type), play_type %in% c("pass", "run"))
  
  # QB Stats
  qb_stats <- pbp_filtered %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(player_id = passer_player_id, player_name = passer_player_name, position = passer_player_position) %>%
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
    group_by(player_id = rusher_player_id, player_name = rusher_player_name, position = rusher_player_position) %>%
    summarise(
      rush_yards = sum(rushing_yards, na.rm = TRUE),
      rush_tds = sum(rush_touchdown, na.rm = TRUE),
      rush_attempts = n(),
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
    group_by(player_id = receiver_player_id, player_name = receiver_player_name, position = receiver_player_position) %>%
    summarise(
      rec_yards = sum(receiving_yards, na.rm = TRUE),
      rec_tds = sum(receive_touchdown, na.rm = TRUE),
      receptions = sum(complete_pass, na.rm = TRUE),
      tiered_pts = sum(tiered_ppr_pts, na.rm = TRUE),
      fumbles_lost = sum(fumble_lost & receiver_player_id == fumbled_1_player_id, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge all and compute fantasy points
  fantasy_df <- full_join(qb_stats, rush_stats, by = c("player_id", "player_name", "position")) %>%
    full_join(rec_stats, by = c("player_id", "player_name", "position")) %>%
    replace_na(list(
      pass_yards = 0, pass_tds = 0, pass_ints = 0, pick6 = 0,
      rush_yards = 0, rush_tds = 0, rush_attempts = 0,
      rec_yards = 0, rec_tds = 0, receptions = 0, tiered_pts = 0,
      fumbles_lost = 0
    )) %>%
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
