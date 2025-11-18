library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)

#' @title  **Get Season Dates and Types from PWHL API**
#' @description Get seasons, generate season start and end dates, as well as season types,
#' for each season found in the PWHL API
#'
#' @return data.frames of seasons, season dates and types
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import lubridate
#' @import magrittr
#' @export

get_seasons_dates_and_types <- function() {
  season_dates_and_types <- pwhl_season_id()
  season_dates_and_types$season_yr <- as.character(
    season_dates_and_types$season_yr
  )

  season_schedules_by_id <- list()
  season_schedules_by_year <- list()

  for (season_yr in season_dates_and_types$season_yr) {
    season_schedules_by_year[[season_yr]] <- list()
  }

  for (i in seq_len(nrow(season_dates_and_types))) {
    tryCatch(
      expr = {
        season_schedules_by_year[[season_dates_and_types[
          i,
          "season_yr"
        ]]][[season_dates_and_types[
          i,
          "game_type_label"
        ]]] <- pwhl_schedule(
          season_id = season_dates_and_types[i, "season_id"]
        )
      },
      error = function(e) {
        season_schedules_by_year[[season_dates_and_types[
          i,
          "season_yr"
        ]]][[season_dates_and_types[
          i,
          "game_type_label"
        ]]] <<- FALSE
      },
      warning = function(w) {},
      finally = {}
    )
  }

  for (season_id in season_dates_and_types$season_id) {
    tryCatch(
      expr = {
        season_schedules_by_id[[as.character(season_id)]] <- pwhl_schedule(
          season_id = season_id
        )
      },
      error = function(e) {
        season_schedules_by_id[[as.character(season_id)]] <<- FALSE
      },
      warning = function(w) {},
      finally = {}
    )
  }

  season_dates_and_types$season_yr <- as.numeric(
    season_dates_and_types$season_yr
  )

  season_dates_and_types$start_date <- season_dates_and_types %>%
    mutate(
      start_date = {
        map(
          .$season_id,
          function(x) {
            if (
              typeof(season_schedules_by_id[[as.character(x)]][[1]]) !=
                "logical"
            ) {
              season_schedules_by_id[[as.character(x)]] |>
                select(
                  game_date
                ) |>
                first()
            } else {
              season_schedules_by_id[[as.character(x - 1)]] |>
                select(
                  game_date
                ) |>
                last()
            }
          }
        )
      }
    ) %>%
    mutate(
      temp = {
        mapply(
          str_split,
          .$start_date,
          pattern = ", "
        )
      } |>
        map(
          last
        )
    ) %>%
    {
      ifelse(
        .$game_type_label == "playoffs",
        paste0(
          .$season_yr,
          " ",
          .$temp
        ),
        paste0(
          .$season_yr - 1,
          " ",
          .$temp
        )
      )
    } |>
    ymd()

  season_dates_and_types$end_date <- season_dates_and_types %>%
    mutate(
      end_date = {
        map(
          .$season_id,
          function(x) {
            if (
              typeof(season_schedules_by_id[[as.character(x)]][[1]]) !=
                "logical"
            ) {
              season_schedules_by_id[[as.character(x)]] |>
                select(
                  game_date
                ) |>
                last()
            } else {
              season_schedules_by_id[[as.character(x - 1)]] |>
                select(
                  game_date
                ) |>
                last()
            }
          }
        )
      }
    ) %>%
    mutate(
      temp = {
        mapply(
          str_split,
          .$end_date,
          pattern = ", "
        )
      } |>
        map(
          last
        )
    ) %>%
    {
      ifelse(
        .$game_type_label == "preseason",
        paste0(
          .$season_yr - 1,
          " ",
          .$temp
        ),
        paste0(
          .$season_yr,
          " ",
          .$temp
        )
      )
    } |>
    ymd()

  return(
    list(
      season_dates_and_types = season_dates_and_types,
      season_schedules_by_id = season_schedules_by_id,
      season_schedules_by_year = season_schedules_by_year
    )
  )
}
