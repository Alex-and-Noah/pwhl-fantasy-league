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
  if (
    nrow(
      player_box_for_game$skaters
    ) ==
      0
  ) {
    fantasy_points_per_player <- data.frame(
      player_id = integer(),
      first_name = character(),
      last_name = character(),
      position = character(),
      team_id = integer(),
      game_id = integer(),
      goals = integer(),
      assists = integer(),
      wins = integer(),
      ot_losses = integer()
    )
  } else {
    game_from_schedule <- schedule |>
      filter(
        game_id == player_box_for_game$skaters$game_id[[1]]
      )

    fantasy_points_per_player <- rbind(
      player_box_for_game$skaters |>
        select(
          c(
            player_id,
            first_name,
            last_name,
            position,
            team_id,
            game_id,
            goals,
            assists
          )
        ),
      player_box_for_game$goalies |>
        select(
          c(
            player_id,
            first_name,
            last_name,
            position,
            team_id,
            game_id,
            goals,
            assists
          )
        )
    ) |>
      mutate(
        wins = case_when(
          team_id ==
            game_from_schedule |>
              select(
                winner_id
              ) |>
              pull() ~ 1,
          .default = 0
        ),
        ot_losses = case_when(
          (team_id !=
            game_from_schedule |>
              select(
                winner_id
              ) |>
              pull()) &
            (game_from_schedule |>
              select(
                game_status
              ) |>
              pull() ==
              "Final OT") ~ 1,
          .default = 0
        )
      )
  }

  fantasy_points_per_game_id <- lapply(
    names(team_rosters),
    function(
      team_name
    ) {
      team_roster <- team_rosters[team_name][[1]]

      fantasy_points_per_player |>
        mutate(
          player_id = as.double(player_id)
        ) |>
        inner_join(
          team_roster |>
            select(
              c(
                "player_id",
                "acquired",
                "let_go"
              )
            ),
          by = c(
            "player_id"
          )
        ) |>
        mutate(
          goals = ifelse(
            ((is.na(acquired)) | (acquired < game_id)) &
              ((is.na(let_go)) | (game_id <= let_go)),
            goals,
            0
          ),
          assists = ifelse(
            ((is.na(acquired)) | (acquired < game_id)) &
              ((is.na(let_go)) |
                (game_id <= let_go)),
            assists,
            0
          ),
          wins = ifelse(
            ((is.na(acquired)) | (acquired < game_id)) &
              ((is.na(let_go)) |
                (game_id <= let_go)),
            wins,
            0
          ),
          ot_losses = ifelse(
            ((is.na(acquired)) | (acquired < game_id)) &
              ((is.na(let_go)) |
                (game_id <= let_go)),
            ot_losses,
            0
          )
        )
    }
  )

  names(fantasy_points_per_game_id) <- names(team_rosters)

  return(fantasy_points_per_game_id)
}
