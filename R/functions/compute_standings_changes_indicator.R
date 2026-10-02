library(dplyr)
library(lubridate)

#' @title  **Compute Indicator for Changes in Standings**
#' @description Compute indicator for if standings order changed since last day
#'
#' @param all_teams All PWHL player info and stats
#' @param schedule_to_date Current season schedule up to current_date
#' @param current_date The current date
#' @param schedule Entire schedule for current season
#' @param standings Current fantasy standings
#' @return data.frame of points earned by each fantasy team
#' @import dplyr
#' @import lubridate
#' @export

compute_standings_changes_indicator <- function(
    all_teams,
    schedule_to_date,
    current_date,
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
        standings <- standings |>
            mutate(
                Change = "0"
            )
    } else {
        schedule_to_day_before_date <- schedule_to_date |>
            filter(
                game_date < current_date,
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
    }

    return(standings)
}
