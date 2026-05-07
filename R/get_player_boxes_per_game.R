library(dplyr)
library(magrittr)

#' @title  **Get PWHL player boxes per game**
#' @description Get PWHL player boxes per game
#'
#' @param schedule Entire schedule for current season
#' @param team_stats All PWHL player info and stats
#' @return Player boxes per game appended with fantasy points earned
#' @import dplyr
#' @import magrittr
#' @export

get_player_boxes_per_game <- function(
  schedule
) {

  player_boxes_per_game <- list()

  for (game_id in schedule$game_id) {
    player_boxes_per_game[[game_id]] <- pwhl_player_box(
      game_id = game_id
    )
  }

  player_boxes_per_game <- compute_fantasy_roster_points_per_game(
    player_boxes_per_game
  )

  return(player_boxes_per_game)
}
