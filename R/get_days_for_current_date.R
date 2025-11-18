library(dplyr)
library(rlang)

#' @title  **Get PWHL Next Game Dates and Game Info for Current Date**
#' @description Foir the current date, get the next game days, the season
#' ID, the season and the season type
#'
#' @param current_date Current Date
#' @param season_dates_and_types data.frame of all PWHL season dates and types
#' @return Named list of next games, season ID, season  and game type
#' @import dplyr
#' @import rlang
#' @export

get_days_for_current_date <- function(
  current_date,
  season_dates_and_types
) {
  idx_for_current_date <- season_dates_and_types |>
    (\(x) x$start_date < current_date & x$end_date >= current_date)() |>
    which()

  next_game_day_date <- "tbd"
  season_id <- season_dates_and_types[
    idx_for_current_date,
    "season_id"
  ]

  season_id_for_schedule <- season_id

  if (
    length(
      idx_for_current_date
    ) ==
      0
  ) {
    idx_for_current_date <- season_dates_and_types |>
      (\(x) x$start_date < current_date)() |>
      which() |>
      last()

    idx_for_next_date <- season_dates_and_types |>
      (\(x) x$start_date >= current_date)() |>
      which() |>
      first()

    next_game_day_date <- season_dates_and_types[
      idx_for_next_date,
      "start_date"
    ]

    season_id <- season_dates_and_types[
      idx_for_current_date,
      "season_id"
    ]

    season_id_for_schedule <- season_dates_and_types[
      idx_for_next_date,
      "season_id"
    ]
  }

  if (
    is.na(
      idx_for_current_date
    )
  ) {
    idx_for_current_date <- rownames(
      season_dates_and_types
    ) |>
      max()

    next_game_day_date <- NULL
    season_id <- NULL
    season_id_for_schedule <- NULL
  }

  if (
    !is.null(
      season_id
    )
  ) {
    season_yr = season_dates_and_types |>
      filter(
        season_id == .env$season_id
      ) |>
      select(
        season_yr
      ) |>
      as.numeric()

    game_type <- season_dates_and_types |>
      filter(
        season_id == .env$season_id
      ) |>
      select(
        game_type_label
      ) |>
      as.character()
  }

  return(
    data.frame(
      next_game_day_date = next_game_day_date,
      season_id = season_id,
      season_id_for_schedule = season_id_for_schedule,
      season_yr = season_yr,
      game_type = game_type
    )
  )
}
