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
    "C:/git/pwhl-fantasy-league/static/json/team_colours.json"
  )
  
  # team_colours <- fromJSON(
  #   "D:/git/pwhl-fantasy-league/static/json/team_colours.json"
  # )

  team_info <- team_info |>
    rowwise() |>
    mutate(
      colours_1 = team_colours[[
        team_code
      ]][[1]],
      colours_2 = team_colours[[
        team_code
      ]][[2]],
      colours_3 = team_colours[[
        team_code
      ]][[3]],
      colours_4 = team_colours[[
        team_code
      ]][[4]],
      team_label = team_label |>
        recode(
          "Montreal" = "Montréal"
        )
    )

  return(
    team_info
  )
}
