#' @title  **PWHL Season IDs**
#' @description PWHL Season IDs lookup
#'
#' @param season Season (YYYY), the concluding year in XXXX-YY format
#' @param game_type Game type of the season
#' @return A data frame with season ID data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_season_id())
#' }

pwhl_season_id <- function(
  season = 2025,
  game_type = "regular"
) {
  season_id <- data.frame(
    "season_yr" = c(
      2024,
      2024,
      2024,
      2025,
      2025,
      2025,
      2026,
      2026,
      2026
    ),
    "game_type_label" = c(
      "preseason",
      "regular",
      "playoffs",
      "preseason",
      "regular",
      "playoffs",
      "preseason",
      "regular",
      "playoffs"
    ),
    "season_id" = c(
      2,
      1,
      3,
      4,
      5,
      6,
      7,
      8,
      9
    )
  )

  return(season_id)
}
