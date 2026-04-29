library(dplyr)


#' @title  **Stuff**
#' @description etc.
#'
#' @param fantasy_teams_and_standings
#' @return data.frame of points earned by each fantasy team
#' @import dplyr
#' @export

compute_alt_awards <- function(
  fantasy_teams_and_standings
) {
  expanded_points <- fantasy_teams_and_standings$expanded_roster_stats

  map(expanded_points, get_sums) |>
    bind_rows(.id = "Team") |>
    select(-c(player_id, game_id, team_id, faceoff_pct, acquired, let_go))
}


#--------- Helpers ----------#

get_sums <- function(df) {
  df |>
    mutate(
      acquired = as.numeric(acquired),
      let_go = as.numeric(let_go),
      game_count = case_when(
        !duplicated(game_id) ~ 1,
        .default = 0
      )
    ) |>
    filter(
      (is.na(acquired) & is.na(let_go)) |
        acquired < game_id |
        let_go >= game_id
    ) |>
    summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
}
