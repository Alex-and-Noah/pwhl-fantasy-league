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
