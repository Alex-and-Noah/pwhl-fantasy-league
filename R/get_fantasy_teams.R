library(dplyr)
library(tibble)

#' @title  **Get PWHL Fantasy Teams and Points**
#' @description Get PWHL Fantasy rosters, points to date and overall standings
#'
#' @param all_teams All PWHL player info and stats
#' @param schedule_to_date Current season schedule up to current_date
#' @param schedule Entire schedule for current season
#' @param team_colours Named vector of team colours
#' @return data.frames of fantasy team/roster scores and standings
#' @export

get_fantasy_teams <- function(
  schedule,
  team_stats
) {

  df <- get_google_sheet()

  df <- left_join(
    df,
    rownames_to_column(
      schedule,
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
    compute_fantasy_roster_points()

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
    ] |>
    mutate(
      fantasy_points = sum(
        fantasy_teams[[
          df$team_name[[i]]
        ]][[
          "roster"
        ]]$skaters$fantasy_points
      ) + sum(
        fantasy_teams[[
          df$team_name[[i]]
        ]][[
          "roster"
        ]]$goalies$fantasy_points
      )
    )
  }

  return(
    fantasy_teams
  )
}