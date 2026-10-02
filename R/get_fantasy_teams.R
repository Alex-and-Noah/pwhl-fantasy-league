library(dplyr)
library(tibble)
library(purrr)
library(glue)

#' @title  **Get PWHL Fantasy Teams and Points**
#' @description Get PWHL Fantasy rosters, points to date and overall standings
#'
#' @param season_id Current season ID
#' @param current_schedule Entire schedule for current season
#' @param current_date Current date
#' @param team_stats All PWHL player info and stats
#' @param player_boxes_per_game Player boxes for each game of the current season
#' @return data.frames of fantasy team/roster scores and standings
#' @export

get_fantasy_teams <- function(
  season_id,
  current_schedule,
  current_date,
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

  fantasy_team_boxes_per_date <- list()

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
          "goalie_2",
          "new_player"
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
          )|>
          select(
            game_id
          ) |>
          pull()
      ]
    )

    days_seq <- seq(
      current_schedule$game_date |>
        first(),
      min(
        current_schedule$game_date |>
          last(),
        current_date
      ),
      by = "day"
    )

    fantasy_team_boxes_per_date_for_team <- lapply(
      days_seq,
      function(d) {
        compute_fantasy_roster_points_overall(
          fantasy_teams[[
            df$team_name[[i]]
          ]][[
            "roster"
          ]],
          fantasy_teams[[
            df$team_name[[i]]
          ]][[
            "info"
          ]],
          player_boxes_per_game[
            current_schedule |>
              filter(
                game_date == d
              )|>
              select(
                game_id
              ) |>
              pull()
          ]
        ) |>
        map(
          ~ mutate(
            .x,
            game_date = d
          )
        )
      }
    )

    fantasy_team_boxes_per_date[[
      df$team_name[[i]]
    ]] <- list(
      "skaters" = map(
        fantasy_team_boxes_per_date_for_team,
        1
      ) |>
        bind_rows(),
      "goalies" = map(
        fantasy_team_boxes_per_date_for_team,
        2
      ) |>
        bind_rows()
    )

    for (d in days_seq) {

      d_date <- as.Date(d)

      fantasy_teams[[
        df$team_name[[i]]
      ]][[
        "info"
      ]][[
        paste0(
          "fantasy_points_",
          d_date
        )
      ]] <- fantasy_team_boxes_per_date[[
        df$team_name[[i]]
      ]] %>%
      map(
        ~ .x |>
        filter(
          game_date <= d
        ) |>
        summarise(
          across(
            "fantasy_points",
            \(x) sum(x, na.rm = TRUE)
          )
        )
      ) |>
      unlist() |>
      sum()
    }
  }

  saveRDS(
    fantasy_team_boxes_per_date,
    file = glue("fantasy_team_boxes_per_date_season_{season_id}.rds")
  )

  return(
    fantasy_teams
  )
}