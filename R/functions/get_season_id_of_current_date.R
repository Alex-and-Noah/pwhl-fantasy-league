library(dplyr)
library(rlang)

#' @title  **Get the season ID of the current season**
#' @description For the current date, get the season ID
#'
#' @param current_date Current Date
#' @param season_schedules_by_id All PWHL season schedules, dates and types
#' @param get_next_season Get the next season if the current date is in between seasons, otherwise get the last season
#' @return Season ID
#' @import dplyr
#' @import rlang
#' @export

get_season_id_of_current_date <- function(
  current_date,
  season_schedules_by_id,
  get_next_season = TRUE
) {

  season_id <- names(
    season_schedules_by_id %>%
      keep(
        ~ .$info$start_date <= current_date & .$info$end_date >= current_date
      )
    )

  if (
    identical(
      season_id,
      character(0)
    )
  ) {

    if (
      (
        !identical(
          names(
            season_schedules_by_id %>%
              keep(
                ~ .$info$start_date > current_date
              )
          ) |>
            first(),
          character(0)
        )
      ) && (
        get_next_season
      )
    ) {

      season_id <- names(
        season_schedules_by_id %>%
          keep(
            ~ .$info$start_date > current_date
          )
        ) |>
          first()
    } else {
        season_id <- names(
          season_schedules_by_id %>%
            keep(
              ~ .$info$end_date < current_date
            )
        ) |>
          last()
    }
  }

  return(
    season_id
  )
}
