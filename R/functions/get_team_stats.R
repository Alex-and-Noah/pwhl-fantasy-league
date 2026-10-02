library(dplyr)
library(magrittr)
library(purrr)

#' @title  **Get PWHL Team Stats**
#' @description Get stats of all players in the PWHL
#'
#' @param season_id Current season ID
#' @param team_info data.frame of PWHL team_info' info
#' @param player_boxes_per_game data.frames of each PWHL player box
#' @return data.frame of player stats
#' @import dplyr
#' @import magrittr
#' @export

get_team_stats <- function(
  season_id,
  team_info,
  player_boxes_per_game
) {
  team_stats <- list()

  for (i in seq_len(nrow(team_info))) {
    team_id <- team_info[
      i,
      "team_id"
    ][[1]]

    team_code <- team_info[
      i,
      "team_code"
    ][[1]]

    team_stats[[
      team_code
    ]] <- list()

    # here we use our modified functions
    team_stats[[
      team_code
    ]][[
      "skaters"
    ]] <- pwhl_stats_fix(
      season_id = season_id,
      team_info = team_info,
      team_id = team_id,
      position = "skater"
    )

    if (
      nrow(
        team_stats[[
          team_code
        ]][[
          "skaters"
        ]]
      ) ==
        0
    ) {
      team_stats[[
        team_code
      ]][[
        "skaters"
      ]] <- pwhl_team_roster(
        team_info = team_info,
        team_id = team_id,
        season_id = season_id
      )
    } else {
      team_stats[[
        team_code
      ]][[
        "skaters"
      ]] <- pwhl_team_roster(
        season_id = season_id,
        team_info = team_info,
        team_id = team_id
      ) %>%
        merge(
          team_stats[[
            team_code
          ]][[
            "skaters"
          ]],
          by = c(
            "player_id",
            "position",
            "name"
          )
        ) |>
        filter(
          active == 1
        )
    }

    team_stats[[
      team_code
    ]][[
      "skaters"
    ]] <- team_stats[[
      team_code
    ]][[
      "skaters"
    ]] |>
      left_join(
        player_boxes_per_game |>
          lapply(
            "[[",
            "skaters"
          ) |>
          bind_rows() |>
          group_by(
            name
          ) |>
          select(
            win,
            ot_loss
          ) |>
          summarise(
            wins = sum(win),
            ot_losses = sum(ot_loss)
          ),
        by = "name"
      )

    team_stats[[
      team_code
    ]][[
      "goalies"
    ]] <- pwhl_stats_fix(
      season_id = season_id,
      team_info = team_info,
      team_id = team_id,
      position = "goalie"
    )

    if (
      nrow(
        team_stats[[
          team_code
        ]][[
          "goalies"
        ]]
      ) ==
        0
    ) {
      team_stats[[
        team_code
      ]][[
        "goalies"
      ]] <- pwhl_team_roster(
        team_info = team_info,
        team_id = team_id,
        season_id = season_id
      )
    } else {
      team_stats[[
        team_code
      ]][[
        "goalies"
      ]] <- pwhl_team_roster(
        season_id = season_id,
        team_info = team_info,
        team_id = team_id
      ) %>%
        merge(
          team_stats[[
            team_code
          ]][[
            "goalies"
          ]],
          by = c(
            "player_id",
            "name"
          )
        ) |>
        filter(
          active == 1
        )
    }

    team_stats[[
      team_code
    ]][[
      "goalies"
    ]] <- team_stats[[
      team_code
    ]][[
      "goalies"
    ]] |>
      select(
        -wins
      ) |>
      left_join(
        player_boxes_per_game |>
          lapply(
            "[[",
            "goalies"
          ) |>
          bind_rows() |>
          group_by(
            name
          ) |>
          filter(
            toi != "0"
          ) |>
          select(
            win,
            ot_loss
          ) |>
          summarise(
            wins = sum(win),
            ot_losses = sum(ot_loss)
          ),
        by = "name"
      )
  }

  return(team_stats)
}
