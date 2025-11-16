library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Team Standings**
#' @description Compute PWHL fantasy team standings to date.
#'
#' @param fantasy_roster_points data.frame of points earned by each fantasy
#' roster, per game
#' @param team_images Images associated to each team
#' @return A data frame with aggregated fantasy team standings
#' @import dplyr
#' @import magrittr
#' @export

compute_standings <- function(
  fantasy_roster_points,
  team_images
) {
  fantasy_team_scores <- lapply(
    names(fantasy_roster_points),
    function(team_name) {
      fantasy_roster_points[[team_name]] |>
        tail(1) |>
        select(
          fantasy_points
        ) |>
        pull()
    }
  )

  names(fantasy_team_scores) <- names(fantasy_roster_points)

  standings <- fantasy_team_scores[order(
    unlist(fantasy_team_scores),
    decreasing = TRUE
  )] |>
    stack() |>
    set_names(
      c(
        "points",
        "team_name"
      )
    )

  standings <- standings |>
    mutate(
      Team = recode(
        team_name,
        !!!team_images
      )
    ) |>
    rename(
      Name = team_name,
      Points = points
    ) |>
    select(
      c(
        "Name",
        "Team",
        "Points"
      )
    )

  return(standings)
}
