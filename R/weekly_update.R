library(blastula)
library(dplyr)
library(tidyr)
library(glue)
library(purrr)
library(gt)
library(gtExtras)
library(htmltools)
library(keyring)

season_id <- readRDS("data/season_id.RDS")

player_boxes_per_game <- readRDS(glue(
  "data/player_boxes_per_game_season_{season_id}.rds"
))

fantasy_teams <- readRDS(glue(
  "data/fantasy_teams_season_{season_id}.rds"
))

# current_date <- Sys.Date()
current_date <- as.Date("2026-01-25")
last_week <- current_date - 7

current_schedule <- readRDS(glue(
  "data/current_schedule_season_{season_id}.rds"
)) |>
  dplyr::filter(game_date < current_date, game_date >= last_week)

this_weeks_games <- current_schedule$game_id |> c()

this_weeks_skaters <- player_boxes_per_game[this_weeks_games] |>
  purrr::map(1) |>
  purrr::list_rbind()

this_weeks_goalies <- player_boxes_per_game[this_weeks_games] |>
  purrr::map(2) |>
  purrr::list_rbind()


weekly_standings <- readRDS(
  glue("data/standings_season_{season_id}.rds")
) |>
  select(
    1:4,
    glue("standings_{current_date}"),
    glue("standings_{last_week}")
  ) |>
  mutate(
    position_change = (.data[[glue("standings_{current_date}")]] -
      .data[[glue("standings_{last_week}")]]),
    position_change = case_when(
      position_change > 0 ~ "arrow-down",
      position_change == 0 ~ NA,
      position_change < 0 ~ "arrow-up"
    )
  ) |>
  select(
    team_colour,
    team_image,
    fantasy_team_name,
    fantasy_points,
    position_change
  ) |>
  mutate(
    team_image = paste0(
      "<img src=\"",
      team_image,
      "\" style=\"width:5vh;\">"
    )
  )

personalized_stats <- function(team) {
  roster_skaters <- this_weeks_skaters |>
    dplyr::filter(
      player_id %in% fantasy_teams[[team]][["roster"]][["skaters"]]$player_id,
      !(name %in% c(fantasy_teams[[team]][["info"]]$old_player))
    ) |>
    dplyr::mutate(starting = as.numeric(starting))

  roster_goalies <- this_weeks_goalies |>
    filter(
      player_id %in% fantasy_teams[[team]][["roster"]][["goalies"]]$player_id,
      !(name %in% c(fantasy_teams[[team]][["info"]]$old_player))
    )

  this_week <- bind_rows(roster_skaters, roster_goalies)

  game_outcomes <- data.frame(
    statistic = c(
      "Total Games",
      "Unique Games",
      "Unique Players",
      "Unique Wins",
      "Unique Ot Losses"
    ),
    value = c(
      nrow(this_week),
      this_week |> select(game_id) |> unique() |> nrow(),
      this_week |> select(player_id) |> unique() |> nrow(),
      this_week |>
        filter(win == 1) |>
        select(game_id) |>
        unique() |>
        nrow(),
      this_week |>
        filter(ot_loss == 1) |>
        select(game_id) |>
        unique() |>
        nrow()
    )
  )

  summary_stats <- this_week |>
    summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
    select(-c(team_id, game_id, faceoff_pct, starting)) |>
    select(fantasy_points, everything()) |>
    rename_with(
      ~ stringr::str_to_title(stringr::str_replace_all(.x, "_", " "))
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "statistic",
      values_to = "value"
    ) |>
    rbind(game_outcomes)
}

email_data <- fantasy_teams |>
  names() |>
  map(personalized_stats) |>
  set_names(fantasy_teams |> names())

weekly_points <- email_data |>
  bind_rows(.id = "fantasy_team_name") |>
  filter(statistic == "Fantasy Points") |>
  select(-statistic)

weekly_standings <- weekly_standings |> merge(weekly_points)

weekly_standings_table <- weekly_standings |>
  arrange(desc(fantasy_points)) |>
  mutate(
    fantasy_points = paste0(
      round(
        fantasy_points,
        2
      ),
      " (+",
      round(
        value,
        2
      ),
      ")"
    )
  ) |>
  gt() |>
  fmt_icon(
    columns = position_change,
    fill_color = c(
      "arrow-up" = "#128800ff",
      "arrow-down" = "#c50101ff",
      height = "1.5em"
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(
        color = from_column(
          column = "team_colour"
        )
      ),
      cell_text(
        weight = "bold"
      )
    ),
    locations = cells_body(
      columns = team_image
    )
  ) |>
  fmt_markdown(
    columns = team_image
  ) |>
  cols_hide(
    c(
      team_colour,
      value
    )
  ) |>
  cols_move(
    columns = c(
      fantasy_team_name,
      position_change,
      fantasy_points
    ),
    after = team_image
  ) |>
  cols_label(
    team_image = "",
    fantasy_team_name = "Team Name",
    value = "",
    position_change = "",
    fantasy_points = "Points"
  ) |>
  cols_align(
    columns = everything(),
    align = "center"
  ) |>
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |>
  tab_options(
    table.background.color = "#F5F5F5",
    column_labels.background.color = "#2B2D42",
    table.font.size = px(16),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table_body.hlines.color = "transparent",
    table_body.border.bottom.color = "transparent",
    column_labels.border.bottom.color = "transparent",
    column_labels.border.top.color = "transparent",
    quarto.disable_processing = TRUE
  ) |>
  tab_style_body(
    style = cell_borders(
      sides = c("top", "right", "left", "bottom"),
      weight = px(0) # Remove row borders
    ),
    fn = function(x) {
      is.numeric(x) | is.character(x)
    }
  ) |>
  opt_css(
    css = "
    table tr:nth-child(odd) {
      background-color: #e0dedeff;
    }
    "
  )


make_email <- function(team) {
  unique_players <- email_data[[team]] |>
    filter(statistic == "Unique Players") |>
    select(value) |>
    as.numeric()

  unique_games <- email_data[[team]] |>
    filter(statistic == "Unique Games") |>
    select(value) |>
    as.numeric()

  fantasy_point_table <- email_data[[team]] |>
    filter(
      statistic %in%
        c(
          "Win",
          "Ot Loss",
          "Goals",
          "Assists",
          "Shots",
          "Blocked Shots",
          "Saves",
          "Shutout"
        )
    ) |>
    mutate(
      Points = case_when(
        statistic == "Win" ~ value * 2,
        statistic == "Ot Loss" ~ value,
        statistic == "Goals" ~ value * 2,
        statistic == "Assists" ~ value,
        statistic == "Shots" ~ value * 0.1,
        statistic == "Blocked Shots" ~ value * 0.1,
        statistic == "Saves" ~ value * 0.05,
        statistic == "Shutout" ~ value
      )
    ) |>
    gt(rowname_col = "statistic") |>
    fmt_integer(
      columns = value,
      rows = c(1, 2, 5:last_col())
    ) |>
    grand_summary_rows(
      columns = c(Points),
      fns = list(
        Total ~ sum(.)
      ),
      fmt = ~ fmt_number(., use_seps = FALSE)
    ) |>
    cols_label(
      value = "Count"
    ) |>
    tab_header(
      title = div(
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        div(
          team
        ),
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        style = css(
          `display` = "flex",
          `justify-content` = "center",
          `align-items` = "center"
        )
      )
    ) |>
    tab_options(
      table.background.color = '#F5F5F5',
      heading.background.color = fantasy_teams[[team]]$info$team_colour,
      table.font.size = px(16),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'transparent',
      table_body.hlines.color = 'transparent',
      table_body.border.bottom.color = 'transparent',
      column_labels.border.bottom.color = 'transparent',
      column_labels.border.top.color = 'transparent'
    ) |>
    opt_css(
      css = '
                table tr:nth-child(odd) {
                background-color: #e0dedeff;
                }
                '
    )

  extras_table <- email_data[[team]] |>
    filter(
      !(statistic %in%
        c(
          "Fantasy Points",
          "Goals",
          "Assists",
          "Shots",
          "Blocked Shots",
          "Saves",
          "Shutout",
          "Points",
          "Unique Games",
          "Win",
          "Ot Loss",
          "Unique Players",
          "Time On Ice",
          "Unique Wins",
          "Unique Ot Losses",
          "Total Games"
        ))
    ) |>
    gt(rowname_col = "statistic") |>
    fmt_integer(
      columns = value,
      rows = c(1, 2, 5:last_col())
    ) |>
    tab_row_group(
      label = "Skater Stats",
      rows = c(
        "Hits",
        "Penalty Minutes",
        "Plus Minus",
        "Faceoff Attempts",
        "Faceoff Wins",
        "Faceoff Losses"
      )
    ) |>
    tab_row_group(
      label = "Goalie Stats",
      rows = c(
        "Goals Against",
        "Shots Against"
      )
    ) |>
    row_group_order(
      groups = c("Skater Stats", "Goalie Stats")
    ) |>
    tab_header(
      title = div(
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        div(
          team
        ),
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        style = css(
          `display` = "flex",
          `justify-content` = "center",
          `align-items` = "center"
        )
      )
    ) |>
    tab_options(
      table.background.color = '#F5F5F5',
      heading.background.color = fantasy_teams[[team]]$info$team_colour,
      table.font.size = px(16),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'transparent',
      table_body.hlines.color = 'transparent',
      table_body.border.bottom.color = 'transparent',
      column_labels.hidden = TRUE
    ) |>
    opt_css(
      css = '
                table tr:nth-child(odd) {
                background-color: #e0dedeff;
                }
                '
    )

  game_table <- email_data[[team]] |>
    filter(
      statistic %in%
        c(
          "Unique Games",
          "Unique Players",
          "Time On Ice",
          "Unique Wins",
          "Unique Ot Losses",
          "Total Games"
        )
    ) |>
    gt(rowname_col = "statistic") |>
    fmt_duration(
      columns = "value",
      rows = statistic == "Time On Ice",
      input_units = "minutes",
      output_units = c("hours", "minutes", "seconds"),
    ) |>
    fmt_integer(
      columns = value,
      rows = c(2:last_col())
    ) |>
    tab_header(
      title = div(
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        div(
          team
        ),
        HTML(
          web_image(
            fantasy_teams[[team]]$info$team_image
          )
        ),
        style = css(
          `display` = "flex",
          `justify-content` = "center",
          `align-items` = "center"
        )
      )
    ) |>
    tab_options(
      table.background.color = '#F5F5F5',
      column_labels.hidden = TRUE,
      heading.background.color = fantasy_teams[[team]]$info$team_colour,
      table.font.size = px(16),
      table.border.top.color = 'transparent',
      table.border.bottom.color = 'transparent',
      table_body.hlines.color = 'transparent',
      table_body.border.bottom.color = 'transparent',
      column_labels.border.bottom.color = 'transparent',
      column_labels.border.top.color = 'transparent'
    ) |>
    opt_css(
      css = '
                table tr:nth-child(odd) {
                background-color: #e0dedeff;
                }
                '
    )

  compose_email(
    body = md(
      glue(
        "
## Hello {team},

Here is your weekly PWHL Fantasy League Update! This week's email is sponsored by Meadow and Rambouillet. Thank you to our sponsors!

# {team}

## Games

During the week between {format(current_date-7, '%A, %B %d')} and {format(current_date, '%A, %B %d')} {team} had {unique_players} out of 12 players playing across {unique_games} game(s).

{stringr::str_squish(as_raw_html(game_table))}

## Points

{stringr::str_squish(as_raw_html(fantasy_point_table))}

## Extra Stats

{stringr::str_squish(as_raw_html(extras_table))}

# Overall

Over the past week 

{stringr::str_squish(as_raw_html(weekly_standings_table))}

"
      )
    )
  )
}

# alex <- make_email("The Cry Babies (Alex)")
# noah <- make_email("Nog's Bologs (Noah)")
# karl <- make_email("Hintonburg Sphynxes (Karl)")
# reiko <- make_email("Zillenial Zingers (Reiko)")
