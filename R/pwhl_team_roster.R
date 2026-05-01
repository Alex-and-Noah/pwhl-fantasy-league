library(magrittr)
library(dplyr)
library(httr)
library(jsonlite)
library(glue)
library(tidyr)
library(lubridate)

#' @title  **PWHL Rosters**
#' @description PWHL Rosters lookup
#'
#' @param season_id Season ID to pull the roster from
#' @param season_year Season year of the season ID
#' @param teams data.frame of PWHL teams
#' @param team_id ID of the team to lookup
#' @return A data frame with roster data
#' @import jsonlite
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import httr
#' @import lubridate
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_team_roster(teams = teams, team_id = 1, season_id = 8, season_year = 2025))
#' }

pwhl_team_roster <- function(
  season_id = 8,
  season_year = 2025,
  teams = NULL,
  team_id = 1
) {

  # base_url <- "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=1&season_id=2&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  full_url <- paste0(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=roster&team_id=",
    team_id,
    "&season_id=",
    season_id,
    "&key=694cfeed58c932ee&client_code=pwhl&site_id=8&league_id=1&lang=en&callback=angular.callbacks._h"
  )

  res <- RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    content(
      as = "text",
      encoding = "utf-8"
    )

  res <- gsub(
    "angular.callbacks._h\\(",
    "",
    res
  )

  res <- gsub(
    "]}]}]})",
    "]}]}]}",
    res
  )

  r <- res %>%
    parse_json()

  team_name <- r[[1]]
  team_logo <- r[[2]]
  roster_year <- r[[3]]
  league <- r[[4]]

  players <- r[[5]][[1]]$sections

  roster_data <- data.frame()
  staff_data <- data.frame()

  player_types <- c("Forwards", "Defenders", "Goalies")

  tryCatch(
    expr = {
      for (i in seq_along(players)) {

        if (players[[i]]$title %in% player_types) {

          roster_data_for_player_type <- data.frame()

          for (p in seq_along(players[[i]]$data)) {
            roster_data_for_player_type <- dplyr::bind_rows(
              roster_data_for_player_type,
              data.frame(
                players[[i]]$data[[p]]$row
              )
            )
          }

          if (is.null(players[[i]]$data[[p]]$row$shoots)) {
            roster_data_for_player_type <- roster_data_for_player_type |> mutate(
              hand = catches
            ) |>
            select(
              -catches
            )
          } else {
            roster_data_for_player_type <- roster_data_for_player_type |> mutate(
              hand = shoots
            ) |>
            select(
              -shoots
            )
          }

          roster_data <- dplyr::bind_rows(
            roster_data,
            roster_data_for_player_type
          )

        } else {
          next
        }
      }

      roster_data <- roster_data %>%
        mutate(
          league = "pwhl",
          age = round(
            time_length(
              as.Date(
                paste0(
                  season_year,
                  "-01-01"
                )
              ) -
                as.Date(
                  .data$birthdate
                ),
              "years"
            )
          ),
          player_headshot = paste0(
            "https://assets.leaguestat.com/pwhl/240x240/",
            .data$player_id,
            ".jpg"
          ),
          regular_season = ifelse(
            season_id == 1,
            TRUE,
            FALSE
          ),
          season_year = season_year,
          player_id = as.numeric(player_id),
          team_id = as.numeric(team_id)
        )
    },
    error = function(e) {
      message(
        glue(
          "{Sys.time()}: Invalid season or no roster data available! Try a season from 2023 onwards!"
        )
      )
    },
    warning = function(w) {},
    finally = {}
  )

  return(roster_data)
}
