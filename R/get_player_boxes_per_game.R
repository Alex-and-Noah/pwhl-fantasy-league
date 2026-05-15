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

  for (game_id_iterator in current_schedule$game_id) {
    player_boxes_per_game[[game_id_iterator]] <- pwhl_player_box(
      game_id = game_id_iterator
    )
    
    player_boxes_per_game[[game_id_iterator]]$skaters <- player_boxes_per_game[[game_id_iterator]]$skaters |>
      mutate(
        name = paste0(
          first_name,
          " ",
          last_name
        ),
        game_date = current_schedule |>
          filter(
            game_id == game_id_iterator
          ) |>
          select(
            game_date
          ) |>
          pull()
      ) |>
      select(
        -first_name,
        -last_name
      )

    player_boxes_per_game[[game_id_iterator]]$goalies <- player_boxes_per_game[[game_id_iterator]]$goalies |>
      mutate(
        name = paste0(
          first_name,
          " ",
          last_name
        ),
        game_date = current_schedule |>
          filter(
            game_id == game_id_iterator
          ) |>
          select(
            game_date
          ) |>
          pull()
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
