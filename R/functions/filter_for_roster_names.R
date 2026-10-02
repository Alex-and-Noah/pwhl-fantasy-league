library(magrittr)
library(purrr)

#' @title  **Filter for PWHL Fantasy Rosters**
#' @description Filter all players for those in PWHL fantasy rosters
#'
#' @param team_stats data.frame of the PWHL players
#' @param player_names List of Fantasy rosters names
#' @return A named list of the fantasy roster players
#' @import magrittr
#' @export

filter_for_roster_names <- function(
  team_stats,
  player_names
) {
  # names(roster_names) <- row_names

  # all_teams %>%
  #   filter(
  #     player_name %in% roster_names
  #   ) %>%
  #   mutate(
  #     acquired = ifelse(
  #       player_name == roster_names["new_player"],
  #       roster_names["last_game_id_of_trade_date"],
  #       NA
  #     ),
  #     let_go = ifelse(
  #       player_name == roster_names["old_player"],
  #       roster_names["last_game_id_of_trade_date"],
  #       NA
  #     )
  #   )

  return(
    list(
      skaters = team_stats %>%
        map_df(
          ~filter(
            .$skaters,
            name %in% player_names
          )
        ),
      goalies = team_stats %>%
        map_df(
          ~filter(
            .$goalies,
            name %in% player_names
          )
        )
    )
  )
}
