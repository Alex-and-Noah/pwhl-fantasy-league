library(magrittr)

#' @title  **PWHL Teams**
#' @description PWHL Teams lookup
#'
#' @return A data frame with team data
#' @import jsonlite
#' @import dplyr
#' @import httr
#' @importFrom glue glue
#' @export
#' @examples
#' \donttest{
#'   try(pwhl_teams())
#' }

pwhl_teams <- function(
  season = 2023,
  game_type = "preseason",
  season_id = NULL
) {
  if (
    is.null(
      season_id
    )
  ) {
    seasons <- pwhl_season_id() %>%
      dplyr::filter(season_yr == season, game_type_label == game_type)

    season_id <- seasons$season_id
  }

  full_url = glue::glue(
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
      for (i in 1:length(team_info)) {
        team_df <- data.frame(
          "team_id" = c(team_info[[i]]$id),
          "team_name" = c(team_info[[i]]$name),
          "team_code" = c(team_info[[i]]$team_code),
          "team_nickname" = c(team_info[[i]]$nickname),
          "division" = c(team_info[[i]]$division_id),
          "team_logo" = c(team_info[[i]]$logo)
        )

        if (season_id >= 7) {
          team_code = c(
            "BOS",
            "MIN",
            "MTL",
            "NY",
            "OTT",
            "SEA",
            "TOR",
            "VAN"
          )

          team_label = c(
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
          team_code = c(
            "BOS",
            "MIN",
            "MTL",
            "NY",
            "OTT",
            "TOR"
          )

          team_label = c(
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
