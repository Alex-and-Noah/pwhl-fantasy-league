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

  fantasy_points_per_skater <- lapply(
    player_boxes_per_game,
    function(x) {
      x$skaters |>
        filter(
          name %in% fantasy_team_roster$skaters$name
        )
    }
  ) |> 
  bind_rows() |>
  group_by(
    name
  ) |>
  summarise(
    fantasy_points = sum(
      fantasy_points
    )
  )

  fantasy_points_per_goalie <- lapply(
    player_boxes_per_game,
    function(x) {
      x$goalies |>
        filter(
          name %in% fantasy_team_roster$goalies$name
        )
    }
  ) |> 
  bind_rows() |>
  group_by(
    name
  ) |>
  summarise(
    fantasy_points = sum(
      fantasy_points
    )
  )

  fantasy_team_roster$skaters <- fantasy_team_roster$skaters |>
    merge(
      fantasy_points_per_skater
    )

  fantasy_team_roster$goalies <- fantasy_team_roster$goalies |>
    merge(
      fantasy_points_per_goalie
    )

  return(fantasy_team_roster)
}
