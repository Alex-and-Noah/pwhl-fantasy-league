library(dplyr)
library(purrr)
library(jsonlite)

#' @title  **Get all PWHL team's info'**
#' @description Get all PWHL player team's info'
#'
#' @param season_id Current season ID
#' @return data.frames of PWHL team's info
#' @import dplyr
#' @export
#' #' #' @examples
#' \donttest{
#'   try(get_team_info(2026,8,"regular"))
#' }

get_team_info <- function(
  season_id
) {
  team_info <- pwhl_teams(
    season_id = season_id
  )

  team_colours <- fromJSON(
    "D:/git/pwhl-fantasy-league/static/json/team_colours.json"
  )

  team_info <- team_info |>
    mutate(
      colour_1 = map(
        team_colours,
        `[[`,
        1
      ),
      colour_2 = map(
        team_colours,
        `[[`,
        2
      ),
      colour_3 = map(
        team_colours,
        `[[`,
        3
      ),
      colour_4 = map(
        team_colours,
        `[[`,
        4
      ),
      team_label = team_label |>
        recode(
          "Montreal" = "Montréal"
        )
    )

  return(
    team_info
  )
}
