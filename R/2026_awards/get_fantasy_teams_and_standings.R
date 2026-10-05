library(dplyr)
library(purrr)
library(tibble)

#' @title  **Compute PWHL Fantasy Points For Each Game So Far**
#' @description Compute how many points each PWHL fantasy team player earned for each
#' game played so far
#'
#' @param team_rosters data.frame of Fantasy team rosters
#' @param player_boxes_per_game Player box stats for each game played so far
#' @param schedule Current season schedule
#' @return list of data.frames of points earned by each fantasy team player, for each fantasy team
#' @import dplyr
#' @import purrr
#' @import tibble
#' @export

get_expanded_roster_stats_per_game <- function(
  team_rosters,
  player_boxes_per_game,
  schedule
) {
  if (
    nrow(
      player_boxes_per_game[[1]]$skaters
    ) ==
      0
  ) {
    fantasy_points_per_roster <- lapply(
      names(team_rosters),
      function(team_name) {
        data.frame(
          player_id = as.integer(),
          first_name = as.character(),
          last_name = as.character(),
          position = as.character(),
          team_id = as.integer(),
          game_id = as.integer(),
          time_on_ice = as.integer(),
          goals = as.integer(),
          assists = as.integer(),
          shots = as.integer(),
          hits = as.integer(),
          blocked_shots = as.integer(),
          penalty_minutes = as.integer(),
          faceoff_pct = as.integer(),
          wins = as.integer(),
          ot_losses = as.integer(),
          acquired = as.character(),
          let_go = as.character()
        )
      }
    )

    names(fantasy_points_per_roster) <- names(team_rosters)

    return(
      fantasy_points_per_roster
    )
  } else {
    fantasy_points_per_game_id <- player_boxes_per_game |>
      map(
        get_roster_points_from_game,
        team_rosters = team_rosters,
        schedule = schedule,
        expanded = TRUE
      )

    fantasy_points_per_roster <- lapply(
      names(team_rosters),
      function(team_name) {
        {
          map(
            fantasy_points_per_game_id,
            `[[`,
            team_name
          ) %>%
            do.call(
              rbind,
              .
            ) %>%
            `rownames<-`(NULL)
        } |>
          mutate(
            player_id = as.numeric(player_id)
          ) |>
          arrange(player_id, game_id) |>
          select(
            player_id,
            game_id,
            everything()
          )
      }
    )

    names(fantasy_points_per_roster) <- names(team_rosters)

    return(
      fantasy_points_per_roster
    )
  }
}


#' @title  **Get PWHL Fantasy Teams and Points**
#' @description Get PWHL Fantasy rosters, points to date and overall standings
#'
#' @param all_teams All PWHL player info and stats
#' @param schedule_to_date Current season schedule up to current_date
#' @param schedule Entire schedule for current season
#' @param team_colours Named vector of team colours
#' @return data.frames of fantasy team/roster scores and standings
#' @export

get_fantasy_teams_and_standings <- function(
  all_teams,
  schedule_to_date,
  schedule,
  team_colours
) {
  is_valid_hex_color <- function(color) {
    if (
      grepl(
        "^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6}|[0-9a-fA-F]{8})$",
        color
      )
    ) {
      return(color)
    } else {
      return(sample(colors(), 1))
    }
  }

  v <- Vectorize(is_valid_hex_color)

  rosters_names_gsheet <- get_google_sheet(
    sheet_id = 0
  ) |>
    mutate(
      team_colour = v(team_colour)
    )

  team_images <- rosters_names_gsheet$team_image |>
    set_names(
      rosters_names_gsheet$team_name
    )

  team_colours <- rosters_names_gsheet$team_colour |>
    set_names(
      rosters_names_gsheet$team_name
    )

  rosters_names <- rosters_names_gsheet |>
    select(
      !(team_name:team_image)
    ) |>
    t() |>
    data.frame() |>
    set_names(
      rosters_names_gsheet$team_name
    )

  df <- as.data.frame(
    matrix(
      rep(
        NA,
        length(rosters_names)
      ),
      nrow = 1
    )
  )

  names(df) <- names(rosters_names)
  rownames(df) <- "last_game_id_of_trade_date"

  for (team_name in names(df)) {
    if (!is.na(rosters_names["trade_date", team_name])) {
      df["last_game_id_of_trade_date", team_name] <- schedule |>
        filter(
          game_date <= mdy(rosters_names["trade_date", team_name])
        ) |>
        select(game_id) |>
        last() |>
        pull()
    }
  }

  rosters_names <- rbind(
    rosters_names,
    df
  )

  row_names <- row.names(rosters_names[1])

  team_rosters <- rosters_names |>
    map(
      filter_for_roster_names,
      all_teams = all_teams,
      row_names = row_names
    )

  player_boxes_per_game <- list()

  for (game_id in schedule_to_date$game_id) {
    player_boxes_per_game[[game_id]] <- pwhl_player_box(
      game_id = game_id
    )
  }

  roster_points_per_game <- get_roster_points_per_game(
    team_rosters,
    player_boxes_per_game,
    schedule
  )

  expanded_roster_stats <- get_expanded_roster_stats_per_game(
    team_rosters,
    player_boxes_per_game,
    schedule
  )

  fantasy_roster_points <- compute_fantasy_roster_points(
    roster_points_per_game
  )

  standings <- compute_standings(
    fantasy_roster_points,
    team_images,
    team_colours
  )

  return(
    list(
      team_images = team_images,
      team_colours = team_colours,
      team_rosters = team_rosters,
      roster_points_per_game = roster_points_per_game,
      fantasy_roster_points = fantasy_roster_points,
      standings = standings,
      expanded_roster_stats = expanded_roster_stats
    )
  )
}
