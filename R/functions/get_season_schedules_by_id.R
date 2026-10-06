library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)

#' @title  **Get season schedules by Season ID from PWHL API**
#' @description Get season schedules, generate season start and end dates, as well as season types,
#' for each season found in the PWHL API
#'
#' @return data.frames of seasons, season dates and types, by season ID
#' @import dplyr
#' @import purrr
#' @import stringr
#' @import lubridate
#' @import magrittr
#' @export

get_season_schedules_by_id <- function() {
  season_dates_and_types <- pwhl_season_id()

  season_schedules_by_id <- list()

  for (season_id_var in season_dates_and_types$season_id) {
    tryCatch(
      expr = {
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]] <- list()

        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["info"]] <- season_dates_and_types |>
          filter(
            season_id == season_id_var
          )

        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]][["schedule"]] <- pwhl_schedule(
          season_id = season_id_var
        )
      },
      error = function(e) {
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]] <<- FALSE
      },
      warning = function(w) {},
      finally = {}
    )
  }

  for (season_id_var in season_dates_and_types$season_id) {
    if (
      !identical(
        season_schedules_by_id[[
          as.character(
            season_id_var
          )
        ]],
        FALSE
      )
    ) {
      season_schedules_by_id[[
        as.character(
          season_id_var
        )
      ]][["info"]] <- season_schedules_by_id[[
        as.character(
          season_id_var
        )
      ]][["info"]] |>
        mutate(
          start_date_temp = season_schedules_by_id[[
            as.character(
              season_id_var
            )
          ]][["schedule"]] |>
            select(
              game_date
            ) |>
            first() |>
            pull(),
          end_date_temp = season_schedules_by_id[[
            as.character(
              season_id_var
            )
          ]][["schedule"]] |>
            select(
              game_date
            ) |>
            last() |>
            pull()
        ) %>%
        mutate(
          start_date = str_split(
            .$start_date_temp,
            pattern = ", "
          ) |>
            map(
              last
            ),
          start_date = ifelse(
            .$game_type_label == "playoffs",
            paste0(
              .$season_year,
              " ",
              .$start_date
            ),
            paste0(
              .$season_year - 1,
              " ",
              .$start_date
            )
          ) |>
            ymd(),
          end_date = str_split(
            .$end_date_temp,
            pattern = ", "
          ) |>
            map(
              last
            ),
          end_date = ifelse(
            .$game_type_label == "preseason",
            paste0(
              .$season_year - 1,
              " ",
              .$end_date
            ),
            paste0(
              .$season_year,
              " ",
              .$end_date
            )
          ) |>
            ymd()
        ) |>
        select(
          season_id,
          season_year,
          game_type_label,
          start_date,
          end_date
        )

      season_schedules_by_id[[
        as.character(
          season_id_var
        )
      ]][["schedule"]] <- season_schedules_by_id[[
        as.character(
          season_id_var
        )
      ]][["schedule"]] %>%
        mutate(
          game_date = mapply(
            str_split,
            .$game_date,
            pattern = ", "
          ) |>
            map(
              last
            ),
          game_date = paste0(
            season_schedules_by_id[[
              as.character(
                season_id_var
              )
            ]][["info"]]$season_year,
            " ",
            game_date
          ) |>
            ymd(),
          game_date = if_else(
            game_date >
              ymd(
                season_schedules_by_id[[
                  as.character(
                    season_id_var
                  )
                ]][["info"]]$end_date
              ),
            game_date - years(1),
            game_date
          )
        )
    } else {
      season_schedules_by_id[[
        as.character(
          season_id_var
        )
      ]] <- NULL
    }
  }

  return(
    season_schedules_by_id
  )
}

library(magrittr)

#' @title  **PWHL Schedule**
#' @description PWHL Schedule lookup
#'
#' @param season_id Unique season identifier
#' @param season Season (YYYY), the concluding year in XXXX-YY format
#' @param game_type Game type of the season
#' @return A data frame with schedule data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_schedule(season_id=8))
#' }

pwhl_schedule <- function(
  season_id = NULL,
  season = 2025,
  game_type = "regular"
) {
  if (
    is.null(
      season_id
    )
  ) {
    seasons <- pwhl_season_id() %>%
      dplyr::filter(season_year == season, game_type_label == game_type)

    season_id <- seasons$season_id
  }

  base_url <- glue::glue(
    "https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=schedule&team=-1&season={season_id}&month=-1&location=homeaway&key=694cfeed58c932ee&client_code=pwhl&site_id=2&league_id=1&division_id=-1&lang=en&callback=angular.callbacks._1"
  )
  full_url <- base_url

  res <- httr::RETRY(
    "GET",
    full_url
  )

  res <- res %>%
    httr::content(as = "text", encoding = "utf-8")
  callback_pattern <- "angular.callbacks._\\d+\\("
  res <- gsub(callback_pattern, "", res)
  # res <- gsub("\\}\\]\\)$", "}}]", res)
  # res <- gsub("angular.callbacks._1\\(", "", res)
  res <- gsub("}}]}]}])", "}}]}]}]", res)

  r <- res %>%
    jsonlite::parse_json()

  gm <- r[[1]]$sections[[1]]$data

  schedule_data <- data.frame()

  tryCatch(
    expr = {
      for (i in 1:length(gm)) {
        if (is.null(gm[[i]]$prop$venue_name$venueUrl)) {
          venue <- 'TBD'
        } else {
          venue <- gm[[i]]$prop$venue_name$venueUrl
        }

        game_info <- data.frame(
          "game_id" = c(gm[[i]]$row$game_id),
          "season" = c(season),
          "game_date" = c(gm[[i]]$row$date_with_day),
          "game_status" = c(gm[[i]]$row$game_status),
          "home_team" = c(gm[[i]]$row$home_team_city),
          "home_team_id" = c(gm[[i]]$prop$home_team_city$teamLink),
          "away_team" = c(gm[[i]]$row$visiting_team_city),
          "away_team_id" = c(gm[[i]]$prop$visiting_team_city$teamLink),
          "home_score" = c(gm[[i]]$row$home_goal_count),
          "away_score" = c(gm[[i]]$row$visiting_goal_count),
          "venue" = c(gm[[i]]$row$venue_name),
          "venue_url" = c(venue)
        )

        schedule_data <- rbind(
          schedule_data,
          game_info
        )
      }

      schedule_data <- schedule_data %>%
        dplyr::mutate(
          winner = dplyr::case_when(
            .data$home_score == '' | .data$away_score == "-" ~ '-',
            .data$home_score > .data$away_score ~ .data$home_team,
            .data$away_score > .data$home_score ~ .data$away_team,
            .data$home_score == .data$away_score & .data$home_score != "-" ~
              "Tie",
            TRUE ~ NA_character_
          ),
          winner_id = dplyr::case_when(
            .data$home_score == '' | .data$away_score == "-" ~ '-',
            .data$home_score > .data$away_score ~ .data$home_team_id,
            .data$away_score > .data$home_score ~ .data$away_team_id,
            .data$home_score == .data$away_score & .data$home_score != "-" ~
              "Tie",
            TRUE ~ NA_character_
          ),
          season = season
        ) %>%
        dplyr::select(
          c(
            "game_id",
            "game_date",
            "game_status",
            "home_team",
            "home_team_id",
            "away_team",
            "away_team_id",
            "home_score",
            "away_score",
            "winner",
            "winner_id",
            "venue",
            "venue_url"
          )
        )
    },
    error = function(e) {
      message(glue::glue(
        "{Sys.time()}: Invalid season or no schedule data available! Try a season from 2023 onwards!"
      ))
    },
    warning = function(w) {},
    finally = {}
  )

  return(schedule_data)
}


#' @title  **PWHL Season IDs**
#' @description PWHL Season IDs lookup
#'
#' @param season Season (YYYY), the concluding year in XXXX-YY format
#' @param game_type Game type of the season
#' @return A data frame with season ID data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_season_id())
#' }

pwhl_season_id <- function(
  season = 2025,
  game_type = "regular"
) {
  season_id <- data.frame(
    "season_year" = c(
      2024,
      2024,
      2024,
      2025,
      2025,
      2025,
      2026,
      2026,
      2026,
      2027,
      2027,
      2027
    ),
    "game_type_label" = c(
      "preseason",
      "regular",
      "playoffs",
      "preseason",
      "regular",
      "playoffs",
      "preseason",
      "regular",
      "playoffs",
      "preseason",
      "regular",
      "playoffs"
    ),
    "season_id" = c(
      2,
      1,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12
    )
  )

  return(season_id)
}
