library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Roster Points**
#' @description Compute PWHL fantasy roster points based on the roster's stats.
#'
#' @param fantasy_team_roster Named of data.frames representing the fantasy team's roster
#' @return fantasy team roster appended with fantasy point contributions
#' @import dplyr
#' @import magrittr
#' @export

compute_fantasy_roster_points_overall <- function(
  fantasy_team_roster,
  fantasy_team_info,
  player_boxes_per_game
) {

  fantasy_team_roster$skaters <- fantasy_team_roster$skaters %>%
    mutate(
      fantasy_points =  lapply(
        player_boxes_per_game,
        function(x) {
          x$skaters |>
            filter(
              name == .$name
            )
        }
      ) |> 
      bind_rows() |>
      summarise(
        sum(
          fantasy_points
        )
      )
    )

  fantasy_team_roster$goalies <- fantasy_team_roster$goalies |>
    mutate(
      fantasy_points = 0.05*(shots - goals_against)
    )

  return(fantasy_team_roster)

  fantasy_team_roster$skaters %>%
  lapply(
    player_boxes_per_game,
    function(x) {
      x$skaters |>
        filter(
          name %in% fantasy_team_roster$skaters$name
        )
    }
  )

  skaters = team_stats %>%
        map_df(
          ~filter(
            .$skaters,
            name %in% player_names
          )
        )
}
