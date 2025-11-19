library(dplyr)

#' @title  **Get all PWHL team/player data**
#' @description Get all PWHL player team/player data, including team logo URLS,
#' team rosters with stats and team codes
#'
#' @param season_id Current season ID
#' @param season_yr Current season year
#' @param game_type Current season game type
#' @return data.frames of PWHL team/player data
#' @import dplyr
#' @export
#' #' #' @examples
#' \donttest{
#'   try(get_all_teams(2026,8,"regular"))
#' }

get_all_teams <- function(
  season_id,
  season_yr,
  game_type
) {
  team_logo_urls <- get_team_logo_urls(
    season_id
  )

  teams <- pwhl_teams(
    season_id = season_id
  )

  team_colours <- list(
    "Boston" = c(
      "#173F35",
      "#FFFFFF",
      "#B5E3D8",
      "#FFFFFF"
    ),
    "Minnesota" = c(
      "#250E62",
      "#FFFFFF",
      "#A77BCA",
      "#FFFFFF"
    ),
    "Montreal" = c(
      "#862633",
      "#FFFFFF",
      "#041E42",
      "#FFFFFF"
    ),
    "New York" = c(
      "#00BFB3",
      "#FFFFFF",
      "#041E42",
      "#FFFFFF"
    ),
    "Ottawa" = c(
      "#A6192E",
      "#FFFFFF",
      "#4B4F54",
      "#FFFFFF"
    ),
    "Seattle" = c(
      "#0C5256",
      "#FFFFFF",
      "#E1DBC9",
      "#FFFFFF"
    ),
    "Toronto" = c(
      "#0067B9",
      "#FFFFFF",
      "#0C2340",
      "#FFFFFF"
    ),
    "Vancouver" = c(
      "#0F4777",
      "#FFFFFF",
      "#EEE9D8",
      "#FFFFFF"
    )
  )

  teams <- teams |>
    mutate(
      colour_1 = unlist(
        lapply(
          team_label,
          function(x) {
            team_colours[[x]][1]
          }
        )
      ),
      colour_2 = unlist(
        lapply(
          team_label,
          function(x) {
            team_colours[[x]][2]
          }
        )
      ),
      colour_3 = unlist(
        lapply(
          team_label,
          function(x) {
            team_colours[[x]][3]
          }
        )
      ),
      colour_4 = unlist(
        lapply(
          team_label,
          function(x) {
            team_colours[[x]][4]
          }
        )
      )
    )

  team_codes <- teams$team_code

  names(team_codes) <- teams$team_label |>
    recode(
      "Montreal" = "Montréal"
    )

  all_teams <- team_stats(
    season_id = season_id,
    teams = teams
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
