library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)

#' @title  **Get season schedules by Season ID from PWHL API**
#' @description Get season schedules, generate season start and end dates, as well as season types,
#' for each season found in the PWHL API
#'
#' @return data.frames of seasons, season dates and types, by season ID
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import lubridate
#' @import magrittr
#' @export

get_season_schedules_by_id <- function() {

  season_dates_and_types <- pwhl_season_id()

  season_schedules_by_id <- list()

  for (season_id_var in season_dates_and_types$season_id) {
    tryCatch(
      expr = {
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]] <- list()

        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]] <- season_dates_and_types |>
          filter(
            season_id == season_id_var
          )

        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["schedule"]] <- pwhl_schedule(
          season_id = season_id_var
        )
      },
      error = function(e) {
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]] <<- FALSE
      },
      warning = function(w) {},
      finally = {}
    )
  }

  for (season_id_var in season_dates_and_types$season_id) {

    season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]] <- season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]] |> mutate(
          start_date_temp = season_schedules_by_id[[
            as.character(
              season_id_var
            )
          ]][["schedule"]] |>
          select(
            game_date
          ) |>
          first() |>
          pull(),
          end_date_temp = season_schedules_by_id[[
            as.character(
              season_id_var
            )
          ]][["schedule"]] |>
          select(
            game_date
          ) |>
          last() |>
          pull()
        ) %>%
        mutate(
          start_date = str_split(
            .$start_date_temp,
            pattern = ", "
          ) |>
          map(
            last
          ),
          start_date = ifelse(
            .$game_type_label == "playoffs",
            paste0(
              .$season_year,
              " ",
              .$start_date
            ),
            paste0(
              .$season_year - 1,
              " ",
              .$start_date
            )
          ) |>
          ymd(),
          end_date = str_split(
            .$end_date_temp,
            pattern = ", "
          ) |>
          map(
            last
          ),
          end_date = ifelse(
            .$game_type_label == "preseason",
            paste0(
              .$season_year - 1,
              " ",
              .$end_date
            ),
            paste0(
              .$season_year,
              " ",
              .$end_date
            )
          ) |>
          ymd()
        ) |>
        select(
          season_id,
          season_year,
          game_type_label,
          start_date,
          end_date
        )

    season_schedules_by_id[[
      as.character(
        season_id_var
      )
    ]][["schedule"]] <- season_schedules_by_id[[
      as.character(
        season_id_var
      )
    ]][["schedule"]] %>% mutate(
      game_date = mapply(
        str_split,
        .$game_date,
        pattern = ", "
      ) |>
      map(
        last
      ),
      game_date = paste0(
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]]$season_year,
        " ",
        game_date
      ) |>
      ymd(),
      game_date = if_else(
        game_date > ymd(season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]]$end_date),
        game_date - years(1),
        game_date
      )
    )
  }

  return(
    season_schedules_by_id
  )
}
