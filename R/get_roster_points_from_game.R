library(dplyr)

#' @title  **Compute PWHL Fantasy Points from Game**
#' @description Compute how many points each PWHL fantasy team earned from a
#' game
#'
#' @param team_rosters data.frame of Fantasy team rosters
#' @param player_box_for_game Player box stats for a given game
#' @param schedule Season schedule
#' @return data.frame of points earned by each fantasy team
#' @import dplyr
#' @export

get_roster_points_from_game <- function(
  team_rosters,
  player_box_for_game,
  schedule
) {
  game_points <- list()

  game_from_schedule <- schedule |>
    filter(
      game_id == player_box_for_game$skaters$game_id[[1]]
    )

  for (team_name in names(team_rosters)) {
    game_points[[team_name]] <- player_box_for_game$skaters |>
      filter(
        player_id %in% team_rosters[team_name][[1]]$player_id
      ) |>
      mutate(
        sum = rowSums(
          across(
            c(
              goals,
              assists
            )
          )
        )
      ) |>
      select(
        sum
      ) |>
      sum()

    game_points[[team_name]] <- game_points[[team_name]] +
      2 *
        team_rosters[team_name][[1]] |>
          filter(
            team ==
              game_from_schedule |>
                select(
                  winner
                ) |>
                pull()
          ) |>
          nrow()

    if (
      game_from_schedule |>
        select(
          winner
        ) |>
        pull() ==
        "Final OT"
    ) {
      if (game_from_schedule$home_team == game_from_schedule$winner) {
        loser <- game_from_schedule$away_team
      } else {
        loser <- game_from_schedule$home_team
      }

      game_points[[team_name]] <- game_points[[team_name]] +
        team_rosters[team_name][[1]] |>
          filter(
            team == loser
          ) |>
          nrow()
    }
  }

  return(game_points)
}
