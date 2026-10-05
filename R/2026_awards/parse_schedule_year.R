library(lubridate)

#' @title  **Assign Years to PWHL Game Dates**
#' @description Assign years appropriately to game dates in the PWHL schedules
#'
#' @param game_date Date of a given game
#' @param season_start The start of the current season
#' @return data.frame of season dates and types
#' @import lubridate
#' @export

parse_schedule_year <- function(
  game_date,
  season_start
) {
  game_date_with_year <- ymd(
    paste0(
      year(season_start),
      " ",
      game_date
    )
  )

  if (game_date_with_year < season_start) {
    year(game_date_with_year) <- year(game_date_with_year) + 1
  }

  return(
    game_date_with_year
  )
}
