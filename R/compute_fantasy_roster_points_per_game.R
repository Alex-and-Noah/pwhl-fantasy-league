library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Roster Points per game**
#' @description Compute PWHL fantasy roster points obtained each game.
#'
#' @param player_boxes_per_game Player boxes per game
#' @return Player boxes per game appended with fantasy points earned
#' @import dplyr
#' @import magrittr
#' @export

compute_fantasy_roster_points_per_game <- function(
  player_boxes_per_game
) {

  for (game_id in names(player_boxes_per_game)) {

    player_boxes_per_game[[
      game_id
    ]]$skaters <- player_boxes_per_game[[
      game_id
    ]]$skaters |> mutate(
      fantasy_points = goals + assists + 0.05*shots
    )

    player_boxes_per_game[[
      game_id
    ]]$goalies <- player_boxes_per_game[[
      game_id
    ]]$goalies |> mutate(
      fantasy_points = 0.05*(shots_against - goals_against)
    )
  }

  return(player_boxes_per_game)
}
