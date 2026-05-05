library(dplyr)
library(purrr)
library(jsonlite)

#' @title  **Get all PWHL team/player data**
#' @description Get all PWHL player team/player data, including team logo URLS,
#' team rosters with stats and team codes
#'
#' @param season_id Current season ID
#' @return data.frames of PWHL team/player data
#' @import dplyr
#' @export
#' #' #' @examples
#' \donttest{
#'   try(get_all_teams(2026,8,"regular"))
#' }

get_all_teams <- function(
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

  team_stats <- get_team_stats(
    season_id = season_id,
    team_info = team_info
  )

  all_teams <- all_teams %>%
    mutate(
      team_logo = teams[
        match(
          .$team_id,
          teams$team_id
        ),
        "team_logo"
      ],
      team_colour_1 = teams[
        match(
          .$team_id,
          teams$team_id
        ),
        "colour_1"
      ],
      team_colour_2 = teams[
        match(
          .$team_id,
          teams$team_id
        ),
        "colour_3"
      ]
    )

  return(
    list(
      team_logo_urls = team_logo_urls,
      teams = teams,
      team_codes = team_codes,
      all_teams = all_teams
    )
  )
}
