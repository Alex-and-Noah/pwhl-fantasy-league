library(stringr)
library(purrr)
library(lubridate)
library(dplyr)

#' @title  **Get PWHL Schedule info and Nearby Game Dates**
#' @description Get PWHL schedule info and next game dates
#'
#' @param season_id Current season ID
#' @param season_schedules_by_id Schedules by season ID
#' @param team_codes Named list of shortened PWHL team codes
#' @param team_logo_urls Named list of URLs of PWHL team logos
#' @param season_dates_and_type data.frame of current season dates and type
#' @param next_game_day_date Next game date flag
#' @param current_date Current date
#' @return data.frames of schedule info and next game days
#' @import stringr
#' @import purrr
#' @import lubridate
#' @import dplyr
#' @export

get_schedule_info <- function(
  season_id,
  season_schedules_by_id,
  team_codes,
  team_logo_urls,
  season_dates_and_type,
  next_game_day_date,
  current_date
) {
  schedule <- season_schedules_by_id[season_id][[1]]

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
      season_start = season_dates_and_types[
        season_id,
        "start_date"
      ]
    ) |>
    as_date()

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

  grepl(
    "^\\d{1,2}:\\d{2}\\s(am|pm)\\s(EST|EDT|CST|CDT|MST|MDT|PST|PDT)$",
    "Final"
  )

  schedule_to_date <- schedule |>
    filter(
      !grepl(
        "^\\d{1,2}:\\d{2}\\s(am|pm)\\s(EST|EDT|CST|CDT|MST|MDT|PST|PDT)$",
        game_status
      ) &
        game_status != "TBD"
    )

  return(
    list(
      schedule = schedule,
      schedule_to_date = schedule_to_date,
      next_game_day_date = next_game_day_date,
      todays_games = todays_games,
      next_game_day = next_game_day
    )
  )
}
