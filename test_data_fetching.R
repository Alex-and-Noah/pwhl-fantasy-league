library(tidyverse)
library(gt)
library(bslib)
library(shiny)
library(bsicons)
library(ggimage)
library(rsvg)

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

current_date <- as.Date("2026-04-25")
# current_date <- as.Date("2025-11-17")

season_schedules_by_id <- get_season_schedules_by_id()

season_id <- get_season_id_of_current_date(
  current_date,
  season_schedules_by_id
)

current_schedule <- season_schedules_by_id[[
  season_id
]]$schedule

next_game_day <- current_schedule |>
  filter(
    current_date <= game_date
  ) |>
  first() |>
  select(
    game_date
  ) |>
  pull()

team_info <- get_team_info(
  season_id
)

team_stats <- get_team_stats(
  season_id,
  team_info
)

player_boxes_per_game <- get_player_boxes_per_game(
  current_schedule
)

fantasy_teams <- get_fantasy_teams(
  current_schedule,
  team_stats,
  player_boxes_per_game
)

