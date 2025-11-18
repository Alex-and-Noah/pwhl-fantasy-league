library(dplyr)
library(magrittr)

#' @title  **Get PWHL Team Stats**
#' @description Get stats of all players in the PWHL
#'
#' @param season_id Current season ID
#' @param teams data.frame of PWHL teams
#' @return data.frame of player stats
#' @import dplyr
#' @import magrittr
#' @export

team_stats <- function(
  season_id,
  teams
) {
  all_teams <- data.frame()

  for (team_label in teams$team_label) {
    # here we use our modified functions
    df_stats <- pwhl_stats_fix(
      position = "skater",
      team_label = team_label,
      teams = teams,
      season_id = season_id
    )

    if (
      nrow(
        df_stats
      ) ==
        0
    ) {
      df_team <- pwhl_team_roster(
        teams = teams,
        team_label = team_label,
        season_id = season_id
      )

      all_teams <- rbind(all_teams, df_team)
    } else {
      df_team <- pwhl_team_roster(
        teams = teams,
        team_label = team_label,
        season_id = season_id
      ) %>%
        merge(
          df_stats,
          by = c("player_id")
        )

      all_teams <- rbind(all_teams, df_team) |>
        filter(
          current_team == 1
        )
    }
  }

  return(all_teams)
}
