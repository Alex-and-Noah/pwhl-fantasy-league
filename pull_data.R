library(tidyverse)
library(gt)
library(gtExtras)
library(bslib)
library(shiny)
library(bsicons)
library(ggimage)
library(rsvg)
library(here)
library(htmltools)
library(glue)

invisible(
  lapply(
    list.files(
      "./R",
      full.names = TRUE
    ),
    source
  )
)

environment(custom_gt_split) <- asNamespace('gt')
assignInNamespace("gt_split", custom_gt_split, ns = "gt")

current_date <- today(
  tzone = "EST"
)

current_date <- as.Date("2026-04-25")
# current_date <- as.Date("2025-11-15")
# current_date <- as.Date("2026-05-20")

season_schedules_by_id <- get_season_schedules_by_id()

season_id <- get_season_id_of_current_date(
  current_date,
  season_schedules_by_id
)

saveRDS(
  season_id,
  file = glue("season_id.rds")
)

current_schedule <- season_schedules_by_id[[
  season_id
]]$schedule

saveRDS(
  current_schedule,
  file = glue("current_schedule_season_{season_id}.rds")
)

current_date <- current_schedule |>
  select(
    game_date
  ) |>
  last() |>
  pull()

saveRDS(
  current_date,
  file = glue("current_date_season_{season_id}.rds")
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
  file = glue("next_game_day_season_{season_id}.rds")
)

team_info <- get_team_info(
  season_id
)

saveRDS(
  team_info,
  file = glue("team_info_season_{season_id}.rds")
)

player_boxes_per_game <- get_player_boxes_per_game(
  current_schedule
)

saveRDS(
  player_boxes_per_game,
  file = glue("player_boxes_per_game_season_{season_id}.rds")
)

team_stats <- get_team_stats(
  season_id,
  team_info,
  player_boxes_per_game
)

saveRDS(
  team_stats,
  file = glue("team_stats_season_{season_id}.rds")
)

fantasy_teams <- get_fantasy_teams(
  season_id,
  current_schedule,
  current_date,
  team_stats,
  player_boxes_per_game
)

saveRDS(
  fantasy_teams,
  file = glue("fantasy_teams_season_{season_id}.rds")
)

standings <- compute_standings(
  current_date,
  fantasy_teams
)

saveRDS(
  standings,
  file = glue("standings_season_{season_id}.rds")
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
