library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Team Standings**
#' @description Compute PWHL fantasy team standings to date.
#'
#' @param fantasy_teams Fantasy team info and rosters
#' @return A data frame with aggregated fantasy team standings
#' @import dplyr
#' @import magrittr
#' @export

compute_standings <- function(
  current_date,
  fantasy_teams
) {

  standings <- fantasy_teams %>%
    map(`[[`, 1) %>%
    bind_rows() |>
    mutate(
      fantasy_team_name = names(fantasy_teams),
      fantasy_points = !!sym(
        paste0(
          "fantasy_points_",
          current_date
        )
      )
    ) |>
    replace_na(
      list(
        fantasy_points = 0
      )
    ) |>
    arrange(
      desc(fantasy_points),
      fantasy_team_name
    ) |>
    select(
      team_colour,
      team_image,
      fantasy_team_name,
      fantasy_points
    )

  standings_yesterday <- fantasy_teams %>%
    map(`[[`, 1) %>%
    bind_rows() |>
    mutate(
      fantasy_team_name = names(fantasy_teams),
      fantasy_points_yesterday = !!sym(
        paste0(
          "fantasy_points_",
          current_date - days(1)
        )
      )
    ) |>
    replace_na(
      list(
        fantasy_points_yesterday = 0
      )
    ) |>
    arrange(
      -fantasy_points_yesterday,
      fantasy_team_name
    ) |>
    select(
      team_colour,
      team_image,
      fantasy_team_name,
      fantasy_points_yesterday
    )

    standings <- standings |>
      mutate(
        position_change_since_yesterday = (
          match(
            standings$fantasy_team_name,
            standings_yesterday$fantasy_team_name
          ) - seq_len(
            nrow(
              standings
            )
          )
        ) |>
          sign()
      ) |>
      merge(
        standings_yesterday,
        sort = FALSE
      ) |>
      mutate(
        fantasy_points_change_since_yesterday = fantasy_points - fantasy_points_yesterday
      ) |>
      select(
        -fantasy_points_yesterday
      ) |>
      arrange(
        -fantasy_points
      )

    standings_over_time <- fantasy_teams |>
      map(`[[`, 1) |>
      bind_rows() |>
      mutate(
        fantasy_team_name = names(fantasy_teams)
      ) |>
      arrange(
        desc(
          !!sym(
            paste0(
              "fantasy_points_",
              current_date
            )
          )
        ),
        fantasy_team_name
      ) |>
      mutate(
        !!paste0(
          "standings_",
          current_date
        ) := row_number()
      ) |>
      select(
        team_colour,
        team_image,
        fantasy_team_name,
        paste0(
          "standings_",
          current_date
        )
      ) |> arrange(
        fantasy_team_name
      )

    days_seq <- seq(
      current_schedule$game_date |>
        first(),
      min(
        current_schedule$game_date |>
          last(),
        current_date
      ) - days(1),
      by = "day"
    )

    for (d in rev(
      days_seq
    )) {

      d_date <- as.Date(d)

      standings_over_time <- standings_over_time |>
      mutate(
        !!sym(
          paste0(
            "standings_",
            d_date
          )
        ) := fantasy_teams |>
          map(`[[`, 1) |>
          bind_rows() |>
          mutate(
            fantasy_team_name = names(fantasy_teams)
          ) |>
          arrange(
            desc(
              !!sym(
                paste0(
                  "fantasy_points_",
                  d_date
                )
              )
            ),
            fantasy_team_name
          ) |>
          mutate(
            !!paste0(
              "standings_",
              d_date
            ) := row_number()
          ) |>
          arrange(
            fantasy_team_name
          ) |>
          select(
            !!paste0(
              "standings_",
              d_date
            )
          ) |>
          pull()
      )
    }

    standings <-standings |>
    merge(
      standings_over_time
    )
  
  return(standings)
}
