library(stringr)
library(purrr)
library(lubridate)
library(dplyr)

#' @title  **Get PWHL Schedule and Nearby Game Dates**
#' @description Get PWHL schedule and next/last game dates
#'
#' @param season_id Current season ID
#' @param team_codes Named list of shortened PWHL team codes
#' @param team_logo_urls Named list of URLs of PWHL team logos
#' @param season_dates_and_type data.frame of current season dates and type
#' @param last_game_day_date Last game date flag
#' @param next_game_day_date Next game date flag
#' @param current_date Current date
#' @return data.frames of schedule and next/last game days
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import dplyr
#' @export

get_schedule <- function(
  season_id,
  team_codes,
  team_logo_urls,
  season_dates_and_type,
  last_game_day_date,
  next_game_day_date,
  current_date
) {
  schedule <- pwhl_schedule(
    season_id = season_id
  )

  schedule$home_team_code <- team_codes[
    match(
      schedule$home_team,
      names(team_codes)
    )
  ]

  schedule$home_team_logo <- team_logo_urls[
    match(
      schedule$home_team_code,
      names(team_logo_urls)
    )
  ]

  schedule$home_team_code <- team_codes[
    match(
      schedule$home_team,
      names(team_codes)
    )
  ]

  schedule$away_team_code <- team_codes[
    match(
      schedule$away_team,
      names(team_codes)
    )
  ]

  schedule$away_team_logo <- team_logo_urls[
    match(
      schedule$away_team_code,
      names(team_logo_urls)
    )
  ]

  schedule$game_date <- schedule$game_date |>
    str_split_i(
      pattern = ",",
      i = -1
    ) |>
    trimws() |>
    map_dbl(
      parse_schedule_year,
      season_start = season_dates_and_type[
        season_id,
        "start_date"
      ]
    ) |>
    as_date()

  if (
    as.character(
      last_game_day_date
    ) ==
      "tbd"
  ) {
    last_game_day_date <- Filter(
      function(x) ymd(x) < ymd(current_date),
      schedule$game_date
    ) |>
      last()
  }

  if (
    is.null(
      last_game_day_date
    )
  ) {
    last_game_day <- FALSE
  } else {
    last_game_day <- schedule |>
      filter(
        game_date ==
          ymd(
            last_game_day_date
          )
      )
  }

  todays_games <- schedule |>
    filter(
      game_date ==
        ymd(
          current_date
        )
    )

  if (
    as.character(
      next_game_day_date
    ) ==
      "tbd"
  ) {
    next_game_day_date <- Filter(
      function(x) ymd(x) >= ymd(current_date),
      schedule$game_date
    )[[1]]
  }

  next_game_day <- schedule |>
    filter(
      game_date ==
        ymd(
          next_game_day_date
        )
    )

  schedule_to_date <- schedule |>
    filter(
      game_status %in%
        c(
          "Final",
          "Final OT"
        )
    )

  return(
    list(
      schedule = schedule,
      schedule_to_date = schedule_to_date,
      last_game_day_date = last_game_day_date,
      next_game_day_date = next_game_day_date,
      last_game_day = last_game_day,
      todays_games = todays_games,
      next_game_day = next_game_day
    )
  )
}
