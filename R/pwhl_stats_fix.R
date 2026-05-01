library(magrittr)

#' @title  **PWHL Stats**
#' @description PWHL Stats lookup
#'
#' @param season_id Season ID to pull the roster from
#' @param teams data.frame of PWHL teams
#' @param team_id ID of the team to lookup
#' @param position either goalie or skater. If skater, need to select a team.
#' @return A data frame with roster data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @import tidyverse
#' @export

pwhl_stats_fix <- function(
  season_id = 2,
  teams = NULL,
  team_id = 1,
  position = "goalie"
) {

  tryCatch(
    expr = {
      if (position == "goalie") {

        URL <- glue::glue(
          "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team=all&position=goalies&rookies=0&statsType=expanded&rosterstatus=undefined&site_id=2&first=0&limit=20&sort=gaa&league_id=1&lang=en&division=-1&qualified=all&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._5"
        )

        res <- httr::RETRY(
          "GET",
          URL
        )

        res <- res %>%
          httr::content(as = "text", encoding = "utf-8")

        res <- gsub("angular.callbacks._5\\(", "", res)
        res <- gsub("}}]}]}])", "}}]}]}]", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data = r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {
          players <- dplyr::bind_rows(
            players,
            data.frame(
              data[[y]]$row
            )
          )
        }

        players <- players %>%
          tidyr::separate(
            "minutes_played",
            into = c("minutes_played", "seconds_played"),
            sep = ":",
            remove = FALSE
          )
      } else {
        URL <- glue::glue(
          "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=players&season={season_id}&team={team_id}&position=skaters&rookies=0&statsType=standard&rosterstatus=undefined&site_id=2&first=0&limit=20&sort=points&league_id=1&lang=en&division=-1&key=694cfeed58c932ee&client_code=pwhl&league_id=1&callback=angular.callbacks._6"
        )

        res <- httr::RETRY(
          "GET",
          URL
        )

        res <- res %>%
          httr::content(as = "text", encoding = "utf-8")

        res <- gsub("angular.callbacks._6\\(", "", res)
        res <- sub(")", "", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data = r[[1]]$sections[[1]]$data

        for (y in 1:length(data)) {
          players <- dplyr::bind_rows(
            players,
            data.frame(
              data[[y]]$row
            )
          )
        }

        players <- players %>%
          tidyr::separate(
            "ice_time_minutes_seconds",
            into = c("ice_time_minutes", "ice_time_seconds"),
            sep = ":",
            remove = FALSE
          ) %>%
          tidyr::separate(
            "ice_time_per_game_avg",
            into = c("avg_ice_time_minutes_per_game", "avg_ice_time_seconds_per_game"),
            sep = ":",
            remove = FALSE
          )
      }
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  return(players)
}
