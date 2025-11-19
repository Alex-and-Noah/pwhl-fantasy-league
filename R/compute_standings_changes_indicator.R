library(dplyr)
library(lubridate)

#' @title  **Compute Indicator for Changes in Standings**
#' @description Compute indicator for if standings order changed since last day
#'
#' @param all_teams All PWHL player info and stats
#' @param schedule_to_date Current season schedule up to current_date
#' @param schedule Entire schedule for current season
#' @param standings Current fantasy standings
#' @return data.frame of points earned by each fantasy team
#' @import dplyr
#' @import lubridate
#' @export

compute_standings_changes_indicator <- function(
    all_teams,
    schedule_to_date,
    schedule,
    standings
) {
    source("R/get_fantasy_teams_and_standings.R")

    if (
        length(
            schedule_to_date$game_date |>
                unique()
        ) <
            2
    ) {
        stangins <- standings |>
            mutate(
                Change = NA
            )
    } else {
        schedule_to_day_before_date <- schedule_to_date |>
            filter(
                game_date <= last(game_date) - days(1)
            )

        yesterdays_standings <- get_fantasy_teams_and_standings(
            all_teams,
            schedule_to_day_before_date,
            schedule
        )["standings"][[1]]

        standings$`Change` <- (match(
            standings$Name,
            yesterdays_standings$Name
        ) -
            seq_len(nrow(standings))) |>
            sign()

        standings <- standings |>
            mutate(
                Change = case_when(
                    Change == "-1" ~ "arrow-down",
                    Change == "0" ~ NA,
                    Change == "1" ~ "arrow-up"
                )
            )
    }

    return(standings)
}
