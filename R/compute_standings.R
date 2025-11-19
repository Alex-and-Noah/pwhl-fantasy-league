library(dplyr)
library(magrittr)

#' @title  **Compute PWHL Fantasy Team Standings**
#' @description Compute PWHL fantasy team standings to date.
#'
#' @param fantasy_roster_points data.frame of points earned by each fantasy
#' roster, per game
#' @param team_images Images associated to each team
#' @param team_colours Named vector of team colours
#' @return A data frame with aggregated fantasy team standings
#' @import dplyr
#' @import magrittr
#' @export

compute_standings <- function(
  fantasy_roster_points,
  team_images,
  team_colours
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

  standings <- fantasy_team_scores |>
    stack() |>
    set_names(
      c(
        "points",
        "team_name"
      )
    ) |>
    mutate(
      team_name = factor(
        team_name,
        levels = str_sort(
          unique(team_name),
          locale = "C"
        )
      )
    ) |>
    merge(
      data.frame(
        team_name = names(team_colours),
        Colours = team_colours
      )
    ) |>
    mutate(
      Team = recode(
        team_name,
        !!!team_images
      ),
      Team = case_when(
        is.na(Team) ~ "logo.svg",
        .default = Team
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
        "Points",
        "Colours"
      )
    ) |>
    arrange(
      desc(Points),
      Name
    )

  return(standings)
}
