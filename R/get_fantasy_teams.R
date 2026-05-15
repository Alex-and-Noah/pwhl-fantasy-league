library(dplyr)
library(tibble)

#' @title  **Get PWHL Fantasy Teams and Points**
#' @description Get PWHL Fantasy rosters, points to date and overall standings
#'
#' @param current_schedule Entire schedule for current season
#' @param team_stats All PWHL player info and stats
#' @param player_boxes_per_game Player boxes for each game of the current season
#' @return data.frames of fantasy team/roster scores and standings
#' @export

get_fantasy_teams <- function(
  current_schedule,
  team_stats,
  player_boxes_per_game
) {

  df <- get_google_sheet()

  df <- left_join(
    df,
    rownames_to_column(
      current_schedule,
      var = "last_game_row_name_of_trade_date"
    ),
    by = join_by(
      closest(
        trade_date >= game_date
      )
    )
  ) |>
  group_by(
    by = team_name
  ) |>
  summarise(
    across(
      everything(),
      last
    )
  ) |>
  select(
    c(
      names(
        df
      ),
      last_game_row_name_of_trade_date
    )
  )

  fantasy_teams <- list()

  for (i in seq_len(nrow(df))) {

    fantasy_teams[[
      df$team_name[[i]]
    ]] <- list()

    fantasy_teams[[
      df$team_name[[i]]
    ]][[
      "info"
    ]] <- df[
      i,
      c(
        "team_colour",
        "team_image",
        "trade_date",
        "old_player",
        "new_player",
        "last_game_row_name_of_trade_date"
      )
    ]

    fantasy_teams[[
      df$team_name[[i]]
    ]][[
      "roster"
    ]] <- filter_for_roster_names(
      team_stats,
      df[
        i,
        c(
          "forward_1",
          "forward_2",
          "forward_3",
          "forward_4",
          "forward_5",
          "forward_6",
          "defender_1",
          "defender_2",
          "defender_3",
          "defender_4",
          "goalie_1",
          "goalie_2"
        )
      ] |>
      as.list()
    ) |>
    compute_fantasy_roster_points_overall(
      fantasy_teams[[
        df$team_name[[i]]
      ]][[
        "info"
      ]],
      player_boxes_per_game[
        current_schedule |>
          filter(
            game_date <= current_date
          ) |>
          select(
            game_id
          ) |>
          pull()
      ]
    )

    fantasy_teams[[
      df$team_name[[i]]
    ]][[
      "info"
    ]] <- fantasy_teams[[
      df$team_name[[i]]
    ]][[
      "info"
    ]] |>
      mutate(
        fantasy_points = fantasy_teams[[
          df$team_name[[i]]
        ]][[
          "roster"
        ]]$skaters |>
        summarise(
          fantasy_points = sum(
            fantasy_points
          )
        ) |>
        pull() + fantasy_teams[[
          df$team_name[[i]]
        ]][[
          "roster"
        ]]$goalies |>
        summarise(
          sum(
            fantasy_points
          )
        ) |>
        pull()
      )
  }

  return(
    fantasy_teams
  )
}