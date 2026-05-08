library(dplyr)
library(magrittr)

#' @title  **Get PWHL player boxes per game**
#' @description Get PWHL player boxes per game
#'
#' @param current_schedule Entire current_schedule for current season
#' @param team_stats All PWHL player info and stats
#' @return Player boxes per game appended with fantasy points earned
#' @import dplyr
#' @import magrittr
#' @export

get_player_boxes_per_game <- function(
  current_schedule
) {

  player_boxes_per_game <- list()

  for (game_id in current_schedule$game_id) {
    player_boxes_per_game[[game_id]] <- pwhl_player_box(
      game_id = game_id
    )
    
    player_boxes_per_game[[game_id]]$skaters <- player_boxes_per_game[[game_id]]$skaters |>
      mutate(
        name = paste0(
          first_name,
          " ",
          last_name
        )
      ) |>
      select(
        -first_name,
        -last_name
      )

    player_boxes_per_game[[game_id]]$goalies <- player_boxes_per_game[[game_id]]$goalies |>
      mutate(
        name = paste0(
          first_name,
          " ",
          last_name
        )
      ) |>
      select(
        -first_name,
        -last_name
      )
  }

  player_boxes_per_game <- compute_fantasy_roster_points_per_game(
    player_boxes_per_game
  )

  return(player_boxes_per_game)
}
