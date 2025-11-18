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
#' @param teams data.frame of PWHL teams
#' @param season_id Season ID to pull the roster from
#' @param team Team to pull the roster data for
#' @param regular Bool for whether to pull regular or pre-season rosters
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
#'   try(pwhl_team_roster(season_yr = 2023, team = "Toronto"))
#' }

pwhl_team_roster <- function(
  teams,
  team_label,
  season_id
) {
  team_label_arg <- team_label

  team_id <- teams |>
    filter(
      team_label == team_label_arg
    ) |>
    select(team_id) |>
    pull()

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
  # res <- gsub("}}]}]}]})", "}}]}]}]}", res)
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
        # i = 1

        if (players[[i]]$title %in% player_types) {
          # print('yes')

          for (p in seq_along(players[[i]]$data)) {
            if (is.null(players[[i]]$data[[p]]$row$shoots)) {
              hand <- players[[i]]$data[[p]]$row$catches
            } else {
              hand <- players[[i]]$data[[p]]$row$shoots
            }

            "player_id" %in% names(players[[i]]$data[[p]]$row)

            suppressWarnings(
              player_info <- data.frame(
                "player_id" = c(
                  if ("player_id" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$player_id
                  } else {
                    NA
                  }
                ),
                "player_name" = c(
                  if ("name" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$name
                  } else {
                    NA
                  }
                ),
                "jersey_number" = c(
                  if (
                    "tp_jersey_number" %in% names(players[[i]]$data[[p]]$row)
                  ) {
                    players[[i]]$data[[p]]$row$tp_jersey_number
                  } else {
                    NA
                  }
                ),
                "primary_hand" = c(hand),
                "dob" = c(
                  if ("birthdate" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$birthdate
                  } else {
                    NA
                  }
                ),
                "height" = c(
                  if (
                    "height_hyphenated" %in% names(players[[i]]$data[[p]]$row)
                  ) {
                    players[[i]]$data[[p]]$row$height_hyphenated
                  } else {
                    NA
                  }
                ),
                "position" = c(
                  if ("position" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$position
                  } else {
                    NA
                  }
                ),
                "home_town" = c(
                  if ("hometown" %in% names(players[[i]]$data[[p]]$row)) {
                    players[[i]]$data[[p]]$row$hometown
                  } else {
                    NA
                  }
                )
              ) %>%
                separate(
                  player_name,
                  into = c("first_name", "last_name"),
                  remove = FALSE,
                  sep = " "
                )
            )

            # players[[i]]$data[[p]]$prop

            roster_data <- rbind(
              roster_data,
              player_info
            )
          }
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
                  season_yr,
                  "-01-01"
                )
              ) -
                as.Date(
                  .data$dob
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
          season_yr = season_yr,
          player_id = as.numeric(player_id),
          team_id = as.numeric(team_id),
          team = team_label_arg
        ) %>%
        relocate("team_id", .after = player_id) %>%
        relocate("season_yr", .after = team_id)
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
