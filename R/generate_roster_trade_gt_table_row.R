library(tidyr)
library(dplyr)
library(magrittr)
library(gt)

#' @title  **Generate Fantasy Trade table row**
#' @description Get a gt() table row of a fantasy trades
#'
#' @param name Fantasy team name
#' @param team_rosters Fantasy team rosters
#' @param fantasy_roster_points Fantasy team roster points
#' @param schedule PWHL schedule
#' @return gt() table of fantasy roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_roster_trade_gt_table_row <- function(
    name,
    team_rosters,
    fantasy_roster_points,
    schedule
) {
    tradees <- team_rosters[[name]] |>
        filter(
            !is.na(acquired) | !is.na(let_go)
        )

    if (
        nrow(
            tradees
        ) >
            0
    ) {
        df <- data.frame(
            name = name,
            date = schedule |>
                filter(
                    game_id ==
                        team_rosters[[name]] |>
                            filter(
                                !is.na(acquired)
                            ) |>
                            select(
                                acquired
                            ) |>
                            pull()
                ) |>
                select(
                    game_date
                ) |>
                pull(),
            acquired = tradees |>
                filter(
                    !is.na(acquired)
                ) |>
                select(
                    player_name
                ) |>
                pull(),
            let_go = tradees |>
                filter(
                    !is.na(let_go)
                ) |>
                select(
                    player_name
                ) |>
                pull()
        )
    } else {
        df <- data.frame(
            name = character(),
            date = as.Date(character()),
            acquired = character(),
            let_go = character()
        )
    }

    return(df)
}
