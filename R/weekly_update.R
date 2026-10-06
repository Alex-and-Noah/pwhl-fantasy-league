library(blastula)

season_id <- readRDS("data/season_id.RDS")

player_boxes_per_game <- readRDS(glue(
  "data/player_boxes_per_game_season_{season_id}.rds"
))

fantasy_teams <- readRDS(glue("data/fantasy_teams_season_{season_id}.rds"))

# current_date <- Sys.Date()
current_date <- as.Date("2026-01-25")
last_week <- current_date - 7

current_schedule <- readRDS(glue(
  "data/current_schedule_season_{season_id}.rds"
)) |>
  filter(game_date < current_date, game_date >= last_week)

this_weeks_games <- current_schedule$game_id |> c()

this_weeks_skaters <- player_boxes_per_game[this_weeks_games] |>
  map(1) |>
  list_rbind()

this_weeks_goalies <- player_boxes_per_game[this_weeks_games] |>
  map(2) |>
  list_rbind()

personalized_stats <- function(team) {
  browser()
  roster_skaters <- this_weeks_skaters |>
    filter(
      player_id %in% fantasy_teams[[team]][["roster"]][["skaters"]]$player_id
    ) |>
    mutate(starting = as.numeric(starting))

  roster_goalies <- this_weeks_goalies |>
    filter(
      player_id %in% fantasy_teams[[team]][["roster"]][["goalies"]]$player_id
    )

  this_week <- bind_rows(roster_skaters, roster_goalies)
}


make_email <- function(df) {
  compose_email(
    body = md(
      glue::glue(
        "
## Hello {df$name}

This is an email.
"
      )
    ),
    footer = md(
      "
Code by Alex <3
"
    )
  )
}
