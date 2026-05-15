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
  fantasy_teams
) {

  standings <- fantasy_teams %>%
    map(`[[`, 1) %>%
    bind_rows() |>
    mutate(
      fantasy_team_name = names(fantasy_teams)
    ) |>
    replace_na(
      list(
        fantasy_points = 0
      )
    ) |>
    arrange(
      -fantasy_points,
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
      fantasy_team_name = names(fantasy_teams)
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
  
  return(standings)
}
