library(magrittr)

#' @title  **Filter for PWHL Fantasy Rosters**
#' @description Filter all players for those in PWHL fantasy rosters
#'
#' @param all_teams data.frame of the PWHL players
#' @param roster_names List of Fantasy rosters names
#' @return A data frame of the fantasy roster players
#' @import magrittr
#' @export

filter_for_roster_names <- function(
  roster_names,
  all_teams,
  row_names
) {
  names(roster_names) <- row_names

  all_teams %>%
    filter(
      player_name %in% roster_names
    ) %>%
    mutate(
      acquired = ifelse(
        player_name == roster_names["new_player"],
        roster_names["last_game_id_of_trade_date"],
        NA
      ),
      let_go = ifelse(
        player_name == roster_names["old_player"],
        roster_names["last_game_id_of_trade_date"],
        NA
      )
    )
}
