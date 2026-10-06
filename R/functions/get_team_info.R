library(dplyr)
library(purrr)
library(jsonlite)
library(stringr)

#' @title  **Get all PWHL team's info'**
#' @description Get all PWHL player team's info'
#'
#' @param season_id Current season ID
#' @return data.frames of PWHL team's info
#' @import dplyr
#' @export
#' #' #' @examples
#' \donttest{
#'   try(get_team_info(2026,8,"regular"))
#' }

get_team_info <- function(
  season_id
) {
  team_info <- pwhl_teams(
    season_id = season_id
  )

  team_colours <- fromJSON(
    here(
      "static/json/team_colours.json"
    )
  )

  # team_colours <- fromJSON(
  #   "D:/git/pwhl-fantasy-league/static/json/team_colours.json"
  # )

  team_info <- team_info |>
    rowwise() |>
    mutate(
      colours_1 = team_colours[[
        team_code
      ]][[1]],
      colours_2 = team_colours[[
        team_code
      ]][[2]],
      colours_3 = team_colours[[
        team_code
      ]][[3]],
      colours_4 = team_colours[[
        team_code
      ]][[4]],
      team_label = team_label |>
        recode(
          "Montreal" = "Montréal"
        ),
      team_label = if_else(
        team_nickname == "PWHL",
        str_split_i(
          team_name,
          "PWHL ",
          i = 2
        ),
        team_label
      ),
      team_label = if_else(
        is.na(team_label),
        str_split_i(
          team_name,
          " ",
          i = 1
        ),
        team_label
      )
    )

  return(
    team_info
  )
}


library(dplyr)
library(magrittr)
library(purrr)

#' @title  **Get PWHL Team Stats**
#' @description Get stats of all players in the PWHL
#'
#' @param season_id Current season ID
#' @param team_info data.frame of PWHL team_info' info
#' @param player_boxes_per_game data.frames of each PWHL player box
#' @return data.frame of player stats
#' @import dplyr
#' @import magrittr
#' @export

get_team_stats <- function(
  season_id,
  team_info,
  player_boxes_per_game
) {
  team_stats <- list()

  for (i in seq_len(nrow(team_info))) {
    team_id <- team_info[
      i,
      "team_id"
    ][[1]]

    team_code <- team_info[
      i,
      "team_code"
    ][[1]]

    team_stats[[
      team_code
    ]] <- list()

    # here we use our modified functions
    team_stats[[
      team_code
    ]][[
      "skaters"
    ]] <- pwhl_stats_fix(
      season_id = season_id,
      team_info = team_info,
      team_id = team_id,
      position = "skater"
    )

    if (
      nrow(
        team_stats[[
          team_code
        ]][[
          "skaters"
        ]]
      ) ==
        0
    ) {
      team_stats[[
        team_code
      ]][[
        "skaters"
      ]] <- pwhl_team_roster(
        team_info = team_info,
        team_id = team_id,
        season_id = season_id
      )
    } else {
      team_stats[[
        team_code
      ]][[
        "skaters"
      ]] <- pwhl_team_roster(
        season_id = season_id,
        team_info = team_info,
        team_id = team_id
      ) %>%
        merge(
          team_stats[[
            team_code
          ]][[
            "skaters"
          ]],
          by = c(
            "player_id",
            "position",
            "name"
          )
        ) |>
        filter(
          active == 1
        )
    }

    team_stats[[
      team_code
    ]][[
      "skaters"
    ]] <- team_stats[[
      team_code
    ]][[
      "skaters"
    ]] |>
      left_join(
        player_boxes_per_game |>
          lapply(
            "[[",
            "skaters"
          ) |>
          bind_rows() |>
          group_by(
            name
          ) |>
          select(
            win,
            ot_loss
          ) |>
          summarise(
            wins = sum(win),
            ot_losses = sum(ot_loss)
          ),
        by = "name"
      )

    team_stats[[
      team_code
    ]][[
      "goalies"
    ]] <- pwhl_stats_fix(
      season_id = season_id,
      team_info = team_info,
      team_id = team_id,
      position = "goalie"
    )

    if (
      nrow(
        team_stats[[
          team_code
        ]][[
          "goalies"
        ]]
      ) ==
        0
    ) {
      team_stats[[
        team_code
      ]][[
        "goalies"
      ]] <- pwhl_team_roster(
        team_info = team_info,
        team_id = team_id,
        season_id = season_id
      )
    } else {
      team_stats[[
        team_code
      ]][[
        "goalies"
      ]] <- pwhl_team_roster(
        season_id = season_id,
        team_info = team_info,
        team_id = team_id
      ) %>%
        merge(
          team_stats[[
            team_code
          ]][[
            "goalies"
          ]],
          by = c(
            "player_id",
            "name"
          )
        ) |>
        filter(
          active == 1
        )
    }

    team_stats[[
      team_code
    ]][[
      "goalies"
    ]] <- team_stats[[
      team_code
    ]][[
      "goalies"
    ]] |>
      select(
        -wins
      ) |>
      left_join(
        player_boxes_per_game |>
          lapply(
            "[[",
            "goalies"
          ) |>
          bind_rows() |>
          group_by(
            name
          ) |>
          filter(
            toi != "0"
          ) |>
          select(
            win,
            ot_loss
          ) |>
          summarise(
            wins = sum(win),
            ot_losses = sum(ot_loss)
          ),
        by = "name"
      )
  }

  return(team_stats)
}


library(magrittr)

#' @title  **PWHL Stats**
#' @description PWHL Stats lookup
#'
#' @param season_id Season ID to pull the roster from
#' @param teams_info data.frame of PWHL teams
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
  team_info = NULL,
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
        # res <- gsub("}}]}]}])", "}}]}]}]", res)
        # r <- res %>%
        #   jsonlite::parse_json()

        res <- sub(")", "", res)
        r <- res %>%
          jsonlite::parse_json()

        players <- data.frame()

        data <- r[[1]]$sections[[1]]$data

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
          ) |>
          mutate(
            across(
              c(
                player_id,
                rookie,
                active,
                games_played,
                minutes_played,
                seconds_played,
                shots,
                save_percentage,
                goals_against,
                shutouts,
                wins,
                losses,
                shootout_goals_against,
                shootout_attempts,
                shootout_percentage,
                goals_against_average,
                rank
              ),
              as.numeric
            )
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

        data <- r[[1]]$sections[[1]]$data

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
            into = c(
              "avg_ice_time_minutes_per_game",
              "avg_ice_time_seconds_per_game"
            ),
            sep = ":",
            remove = FALSE
          ) |>
          mutate(
            across(
              c(
                player_id,
                active,
                rookie,
                games_played,
                goals,
                shots,
                hits,
                shots_blocked_by_player,
                ice_time_minutes,
                ice_time_seconds,
                shooting_percentage,
                assists,
                points,
                points_per_game,
                plus_minus,
                penalty_minutes,
                penalty_minutes_per_game,
                avg_ice_time_minutes_per_game,
                avg_ice_time_seconds_per_game,
                hits_per_game_avg,
                power_play_goals,
                power_play_assists,
                short_handed_goals,
                short_handed_assists,
                faceoff_attempts,
                faceoff_wins,
                faceoff_pct,
                rank
              ),
              as.numeric
            )
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
  # season_year = 2025,
  team_info = NULL,
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
            roster_data_for_player_type <- roster_data_for_player_type |>
              mutate(
                hand = catches
              ) |>
              select(
                -catches
              )
          } else {
            roster_data_for_player_type <- roster_data_for_player_type |>
              mutate(
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
          # age = round(
          #   time_length(
          #     as.Date(
          #       paste0(
          #         season_year,
          #         "-01-01"
          #       )
          #     ) -
          #       as.Date(
          #         .data$birthdate
          #       ),
          #     "years"
          #   )
          # ),
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
          # season_year = season_year,
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

library(magrittr)

#' @title  **PWHL Teams**
#' @description PWHL Teams lookup
#'
#' @param season_id Unique season identifier
#' @param season Season (YYYY), the concluding year in XXXX-YY format
#' @param game_type Game type of the season
#' @return A data frame with team data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_teams(season_id=8))
#' }

pwhl_teams <- function(
  season_id = NULL
) {
  full_url <- glue::glue(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=teamsForSeason&season={season_id}&key=694cfeed58c932ee&client_code=pwhl&site_id=2&callback=angular.callbacks._4"
  )

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")

  res <- gsub("angular.callbacks._4\\(", "", res)
  res <- gsub("}]})", "}]}", res)

  r <- res %>%
    jsonlite::parse_json()

  team_info <- r$teamsNoAll
  teams <- data.frame()

  tryCatch(
    expr = {
      for (i in seq_along(team_info)) {
        team_df <- data.frame(
          "team_id" = c(team_info[[i]]$id),
          "team_name" = c(team_info[[i]]$name),
          "team_code" = c(team_info[[i]]$team_code),
          "team_nickname" = c(team_info[[i]]$nickname),
          "division" = c(team_info[[i]]$division_id),
          "team_logo" = c(team_info[[i]]$logo)
        )

        if (season_id >= 7) {
          team_code <- c(
            "BOS",
            "MIN",
            "MTL",
            "NY",
            "OTT",
            "SEA",
            "TOR",
            "VAN"
          )

          team_label <- c(
            "Boston",
            "Minnesota",
            "Montreal",
            "New York",
            "Ottawa",
            "Seattle",
            "Toronto",
            "Vancouver"
          )
        } else {
          team_code <- c(
            "BOS",
            "MIN",
            "MTL",
            "NY",
            "OTT",
            "TOR"
          )

          team_label <- c(
            "Boston",
            "Minnesota",
            "Montreal",
            "New York",
            "Ottawa",
            "Toronto"
          )
        }

        t <- data.frame(
          team_code = team_code,
          team_label = team_label
        )

        teams <- rbind(
          teams,
          team_df %>%
            dplyr::left_join(t, by = c("team_code"))
        )

        teams <- teams %>%
          dplyr::select(
            c(
              "team_name",
              "team_id",
              "team_code",
              "team_nickname",
              "team_label",
              "division",
              "team_logo"
            )
          )
      }
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  teams <- teams |>
    filter(
      team_name != "TBD"
    )

  return(teams)
}
