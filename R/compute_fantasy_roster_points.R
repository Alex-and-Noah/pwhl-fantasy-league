library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Roster Points**
#' @description Compute PWHL fantasy roster points to date.
#'
#' @param roster_points_per_game list of data.frames of points earned by each fantasy
#' team, per game
#' @return A list of data frames with aggregated fantasy roster points
#' @import dplyr
#' @import magrittr
#' @export

compute_fantasy_roster_points <- function(
  roster_points_per_game
) {
  fantasy_roster_points <- lapply(
    names(roster_points_per_game),
    function(team_name) {
      roster_points_per_game[[team_name]] |>
        group_by(
          player_id
        ) |>
        mutate(
          across(
            c(
              goals,
              assists,
              wins,
              ot_losses
            ),
            sum
          )
        ) |>
        select(-game_id) |>
        slice(1) |>
        mutate(
          fantasy_points = sum(
            c(
              goals,
              assists,
              wins,
              wins,
              ot_losses
            )
          )
        ) %>%
        rbind(
          list(
            player_id = -1,
            first_name = "",
            last_name = "",
            position = "",
            team_id = -1,
            goals = sum(.$goals),
            assists = sum(.$assists),
            wins = sum(.$wins),
            ot_losses = sum(.$ot_losses),
            fantasy_points = sum(.$fantasy_points)
          )
        ) |>
        ungroup()
    }
  )

  names(fantasy_roster_points) <- names(roster_points_per_game)

  return(fantasy_roster_points)
}
