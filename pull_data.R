library(tidyverse)
library(gt)
library(bslib)
library(shiny)
library(bsicons)
library(ggimage)
library(rsvg)
library(here)

invisible(
  lapply(
    list.files(
      "./R",
      full.names = TRUE
    ),
    source
  )
)

current_date <- today(
  tzone = "EST"
)

# current_date <- as.Date("2026-04-25")
# current_date <- as.Date("2025-11-15")
# current_date <- as.Date("2026-05-20")

season_schedules_by_id <- get_season_schedules_by_id()

season_id <- get_season_id_of_current_date(
  current_date,
  season_schedules_by_id
)

current_schedule <- season_schedules_by_id[[
  season_id
]]$schedule

saveRDS(
  current_schedule,
  file = "current_schedule.rds"
)

current_date <- current_schedule |>
  select(
    game_date
  ) |>
  last() |>
  pull()

saveRDS(
  current_date,
  file = "current_date.rds"
)

next_game_day <- current_schedule |>
  filter(
    current_date <= game_date
  ) |>
  first() |>
  select(
    game_date
  ) |>
  pull()

saveRDS(
  next_game_day,
  file = "next_game_day.rds"
)

team_info <- get_team_info(
  season_id
)

saveRDS(
  team_info,
  file = "team_info.rds"
)

team_stats <- get_team_stats(
  season_id,
  team_info
)

saveRDS(
  team_stats,
  file = "team_stats.rds"
)

player_boxes_per_game <- get_player_boxes_per_game(
  current_schedule
)

saveRDS(
  player_boxes_per_game,
  file = "player_boxes_per_game.rds"
)

fantasy_teams <- get_fantasy_teams(
  current_schedule,
  current_date,
  team_stats,
  player_boxes_per_game
)

saveRDS(
  fantasy_teams,
  file = "fantasy_teams.rds"
)

standings <- compute_standings(
  current_date,
  fantasy_teams
)

saveRDS(
  standings,
  file = "standings.rds"
)

# This file uses only the following functions: 

# compute_fantasy_roster_points_overall
# compute_fantasy_roster_points_per_game
# compute_standings
# generate_pwhl_roster_gt_table
# get_fantasy_teams
# get_google_sheet
# get_player_boxes_per_game
# get_season_id_of_current_date
# get_season_schedules_by_id
# get_team_info
# get_team_stats
# pwhl_player_box
# pwhl_schedule
# pwhl_season_id
# pwhl_stats_fix
# pwhl_team_roster
# pwhl_teams