library(dplyr)
library(purrr)
library(tibble)

#' @title  **Compute PWHL Fantasy Points For Each Game So Far**
#' @description Compute how many points each PWHL fantasy team earned for each
#' game played so far
#'
#' @param team_rosters data.frame of Fantasy team rosters
#' @param player_boxes_per_game Player box stats for each game played so far
#' @param schedule Current season schedule
#' @return data.frame of points earned by each fantasy team
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
    length(
      player_boxes_per_game
    ) ==
      0
  ) {
    column_names <- c(
      "game_date",
      names(
        team_rosters
      )
    )

    roster_points_per_game <- data.frame(
      matrix(
        ncol = length(
          column_names
        ),
        nrow = 0
      )
    )

    colnames(roster_points_per_game) <- column_names

    return(
      roster_points_per_game
    )
  } else {
    return(
      player_boxes_per_game |>
        map(
          get_roster_points_from_game,
          team_rosters = team_rosters,
          schedule = schedule
        ) |>
        bind_rows() |>
        mutate(
          game_id = names(
            player_boxes_per_game
          ) |>
            as.numeric()
        ) |>
        merge(
          schedule,
          by = "game_id"
        ) |>
        select(
          c(
            game_id,
            game_date,
            names(
              team_rosters
            )
          )
        ) |>
        column_to_rownames(
          "game_id"
        )
    )
  }
}
