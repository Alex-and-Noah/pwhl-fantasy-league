library(dplyr)
library(purrr)
library(tibble)

#' @title  **Compute PWHL Fantasy Points For Each Game So Far**
#' @description Compute how many points each PWHL fantasy team player earned for each
#' game played so far
#'
#' @param team_rosters data.frame of Fantasy team rosters
#' @param player_boxes_per_game Player box stats for each game played so far
#' @param schedule Current season schedule
#' @return list of data.frames of points earned by each fantasy team player, for each fantasy team
#' @import dplyr
#' @import purrr
#' @import tibble
#' @export

get_roster_points_per_game <- function(
  team_rosters,
  player_boxes_per_game,
  schedule
) {
  if (
    nrow(
      player_boxes_per_game[[1]]$skaters
    ) ==
      0
  ) {
    fantasy_points_per_roster <- lapply(
      names(team_rosters),
      function(team_name) {
        data.frame(
          player_id = as.integer(),
          first_name = as.character(),
          last_name = as.character(),
          position = as.character(),
          team_id = as.integer(),
          game_id = as.integer(),
          goals = as.integer(),
          assists = as.integer(),
          wins = as.integer(),
          ot_losses = as.integer(),
          acquired = as.character(),
          let_go = as.character()
        )
      }
    )

    names(fantasy_points_per_roster) <- names(team_rosters)

    return(
      fantasy_points_per_roster
    )
  } else {
    fantasy_points_per_game_id <- player_boxes_per_game |>
      map(
        get_roster_points_from_game,
        team_rosters = team_rosters,
        schedule = schedule
      )

    fantasy_points_per_roster <- lapply(
      names(team_rosters),
      function(team_name) {
        {
          map(
            fantasy_points_per_game_id,
            `[[`,
            team_name
          ) %>%
            do.call(
              rbind,
              .
            ) %>%
            `rownames<-`(NULL)
        } |>
          mutate(
            player_id = as.numeric(player_id)
          ) |>
          arrange(player_id, game_id) |>
          select(
            player_id,
            game_id,
            everything()
          )
      }
    )

    names(fantasy_points_per_roster) <- names(team_rosters)

    return(
      fantasy_points_per_roster
    )
  }
}
