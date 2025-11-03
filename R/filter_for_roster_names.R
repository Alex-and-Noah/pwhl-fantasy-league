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
  all_teams,
  roster_names
) {
  all_teams %>%
    filter(
      player_name %in% roster_names
    )
}
