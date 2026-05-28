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

  if (
    nrow(
      fantasy_team_roster$skaters
    ) > 0
  ) {
    fantasy_points_per_skater <- lapply(
      player_boxes_per_game,
      function(x) {

        skaters <- x$skaters |>
          filter(
            name %in% fantasy_team_roster$skaters$name
          )

          if (
            !is.na(
              fantasy_team_info$old_player
            )
          ) {
            skaters <- skaters |>
              filter(
                name != fantasy_team_info$old_player | game_date <= fantasy_team_info$trade_date
              )
          }

          if (
            !is.na(
              fantasy_team_info$new_player
            )
          ) {
            skaters <- skaters |>
              filter(
                name != fantasy_team_info$new_player | game_date > fantasy_team_info$trade_date
              )
          }

        return(skaters)
      }
    ) |> 
    keep(
      ~nrow(.x) > 0
    ) |> 
    bind_rows()
    
    if (
      nrow(
        fantasy_points_per_skater
      ) > 0
    ) {
      fantasy_points_per_skater <- fantasy_points_per_skater |>
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
    } else {
      fantasy_team_roster$skaters <- fantasy_team_roster$skaters |>
        mutate(
          fantasy_points = 0
        )
    }

  } else {

    fantasy_team_roster$skaters <- fantasy_team_roster$skaters |>
      mutate(
        fantasy_points = NA
      )
  }

  if (
    nrow(
      fantasy_team_roster$goalies
    ) > 0
  ) {
    fantasy_points_per_goalie <- lapply(
      player_boxes_per_game,
      function(x) {
        goalies <- x$goalies |>
          filter(
            name %in% fantasy_team_roster$goalies$name
          )

          if (
            !is.na(
              fantasy_team_info$old_player
            )
          ) {
            goalies <- goalies |>
              filter(
                name != fantasy_team_info$old_player | game_date <= fantasy_team_info$trade_date
              )
          }

          if (
            !is.na(
              fantasy_team_info$new_player
            )
          ) {
            goalies <- goalies |>
              filter(
                name != fantasy_team_info$new_player | game_date > fantasy_team_info$trade_date
              )
          }

        return(goalies)
      }
    ) |> 
    keep(
      ~nrow(.x) > 0
    ) |> 
    bind_rows()
    
    if (
      nrow(
        fantasy_points_per_goalie
      ) > 0
    ) {
      fantasy_points_per_goalie <- fantasy_points_per_goalie |>
        group_by(
          name
        ) |>
        summarise(
          fantasy_points = sum(
            fantasy_points
          )
        )

      fantasy_team_roster$goalies <- fantasy_team_roster$goalies |>
        merge(
          fantasy_points_per_goalie
        )
    } else {
      fantasy_team_roster$goalies <- fantasy_team_roster$goalies |>
        mutate(
          fantasy_points = 0
        )
    }

  } else {

    fantasy_team_roster$goalies <- fantasy_team_roster$goalies |>
      mutate(
        fantasy_points = NA
      )
  }

  return(fantasy_team_roster)
}
