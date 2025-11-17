#' @title  **Get PWHL Fantasy Teams and Points**
#' @description Get PWHL Fantasy rosters, points to date and overall standings
#'
#' @param all_teams All PWHL player info and stats
#' @param schedule_to_date Current season schedule up to current_date
#' @param schedule Entire schedule for current season
#' @return data.frames of schedule and next/last game days
#' @export

get_fantasy_teams_and_standings <- function(
  all_teams,
  schedule_to_date,
  schedule
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
    select(!(team_name:team_image)) |>
    t() |>
    data.frame() |>
    set_names(
      rosters_names_gsheet$team_name
    )

  team_rosters <- rosters_names |>
    map(
      filter_for_roster_names,
      all_teams = all_teams
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

  standings <- compute_standings(
    roster_points_per_game,
    team_images
  )

  return(
    list(
      team_images = team_images,
      team_colours = team_colours,
      team_rosters = team_rosters,
      roster_points_per_game = roster_points_per_game,
      standings = standings
    )
  )
}
