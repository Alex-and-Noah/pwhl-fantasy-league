library(blastula)
library(dplyr)
library(tidyr)
library(glue)
library(purrr)
library(gt)
library(gtExtras)
library(htmltools)
library(keyring)
library(R6)

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

WeeklyUpdate <- R6::R6Class(
  classname = "WeeklyUpdate",
  public = list(
    team = NULL,
    roster_skaters = NULL,
    roster_goalies = NULL,
    this_week_roster = NULL,
    three_stars = NULL,
    all_data = NULL,
    summary_stats = NULL,
    current_date = NULL,
    game_outcomes = NULL,
    weekly_points = NULL,
    weekly_standings = NULL,
    weekly_standings_table = NULL,
    initialize = function(team = NA, current_date = Sys.Date()) {
      self$team <- team
      self$current_date <- current_date
      self$get_personalized_stats()
      self$get_all_data()
    },

    get_personalized_stats = function() {
      self$roster_skaters <- this_weeks_skaters |>
        dplyr::filter(
          player_id %in%
            fantasy_teams[[self$team]][["roster"]][["skaters"]]$player_id,
          !(name %in% c(fantasy_teams[[self$team]][["info"]]$old_player))
        ) |>
        dplyr::mutate(starting = as.numeric(starting))

      self$roster_goalies <- this_weeks_goalies |>
        filter(
          player_id %in%
            fantasy_teams[[self$team]][["roster"]][["goalies"]]$player_id,
          !(name %in% c(fantasy_teams[[self$team]][["info"]]$old_player))
        )

      self$this_week_roster <- bind_rows(
        self$roster_skaters,
        self$roster_goalies
      )

      self$summary_stats <- self$this_week_roster |>
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
        )

      self$three_stars <- self$this_week_roster |>
        arrange(desc(fantasy_points)) |>
        head(3)

      self$game_outcomes <- data.frame(
        statistic = c(
          "Total Games",
          "Unique Games",
          "Unique Players",
          "Unique Wins",
          "Unique Ot Losses"
        ),
        value = c(
          nrow(self$this_week_roster),
          self$this_week_roster |> select(game_id) |> unique() |> nrow(),
          self$this_week_roster |> select(player_id) |> unique() |> nrow(),
          self$this_week_roster |>
            filter(win == 1) |>
            select(game_id) |>
            unique() |>
            nrow(),
          self$this_week_roster |>
            filter(ot_loss == 1) |>
            select(game_id) |>
            unique() |>
            nrow()
        )
      )
    },
    get_all_data = function() {
      get_team_data <- function(team) {
        roster_skaters <- this_weeks_skaters |>
          dplyr::filter(
            player_id %in%
              fantasy_teams[[team]][["roster"]][["skaters"]]$player_id,
            !(name %in% c(fantasy_teams[[self$team]][["info"]]$old_player))
          ) |>
          dplyr::mutate(starting = as.numeric(starting))

        roster_goalies <- this_weeks_goalies |>
          filter(
            player_id %in%
              fantasy_teams[[self$team]][["roster"]][["goalies"]]$player_id,
            !(name %in% c(fantasy_teams[[team]][["info"]]$old_player))
          )

        this_week_roster <- bind_rows(
          roster_skaters,
          roster_goalies
        )

        summary_stats <- this_week_roster |>
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
          )
      }

      self$all_data <- fantasy_teams |>
        names() |>
        map(get_team_data) |>
        set_names(fantasy_teams |> names())

      self$weekly_points <- self$all_data |>
        bind_rows(.id = "fantasy_team_name") |>
        filter(statistic == "Fantasy Points") |>
        select(-statistic)

      self$weekly_standings <- self$weekly_standings |>
        merge(self$weekly_points)
    },

    make_weekly_table = function() {
      self$weekly_standings_table <- self$weekly_standings |>
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
    },

    make_extras_table = function() {
      self$summary_stats |>
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
    },

    make_game_table = function() {
      self$game_outcomes |>
        gt() |>
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
                fantasy_teams[[self$team]]$info$team_image
              )
            ),
            div(
              self$team
            ),
            HTML(
              web_image(
                fantasy_teams[[self$team]]$info$team_image
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
          heading.background.color = fantasy_teams[[
            self$team
          ]]$info$team_colour,
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
    },

    make_point_table = function() {
      self$summary_stats |>
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
                fantasy_teams[[self$team]]$info$team_image
              )
            ),
            div(
              self$team
            ),
            HTML(
              web_image(
                fantasy_teams[[self$team]]$info$team_image
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
          heading.background.color = fantasy_teams[[
            self$team
          ]]$info$team_colour,
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
    },

    make_three_stars_table = function() {
      browser()
      self$three_stars |>
        merge(
          fantasy_teams[[self$team]]$roster$skaters,
          all.x = TRUE
        ) |>
        merge(
          fantasy_teams[[self$team]]$roster$goalies,
          all.x = TRUE
        ) |>
        mutate(
          position = replace_values(
            position,
            c(
              "C",
              "LW",
              "RW"
            ) ~ "F",
            c(
              "LD",
              "RD",
              "D"
            ) ~ "D",
            "G" ~ "G"
          )
        ) |>
        select(
          tp_jersey_number,
          player_headshot,
          name,
          position,
          goals,
          assists,
          shots,
          blocked_shots,
          saves,
          shutout,
          win,
          ot_loss,
          fantasy_points
        ) |>
        mutate(
          role = position,
          team_logo = fantasy_teams[[self$team]]$info$team_image,
          colours_1 = fantasy_teams[[self$team]]$info$team_colour
        ) |>
        rename(
          c(
            "#" = "tp_jersey_number",
            Headshot = "player_headshot",
            Name = "name",
            Pos = "position",
            Role = "role",
            Logo = "team_logo",
            Colour = "colours_1",
            G = "goals",
            A = "assists",
            SH = "shots",
            BLK = "blocked_shots",
            SVS = "saves",
            SO = "shutout",
            W = "win",
            OTL = "ot_loss",
            Pts = "fantasy_points"
          )
        ) |>
        mutate(
          Headshot = paste0(
            "<img src='",
            Headshot,
            "' style='width:40px;height:40px;border:1px solid",
            Colour,
            ";border-radius:50%;'/>"
          ),
          Logo = paste0(
            "<img src='",
            Logo,
            "' style='width:30px;height:30px;'/>"
          )
        ) |>
        select(
          "#",
          Headshot,
          Name,
          Pos,
          Role,
          Logo,
          G,
          A,
          SH,
          BLK,
          SVS,
          SO,
          W,
          OTL,
          Pts,
        ) |>
        mutate(
          G = as.numeric(G),
          A = as.numeric(A),
          SH = as.numeric(SH),
          BLK = as.numeric(BLK),
          SVS = as.numeric(SVS),
          SO = as.numeric(SO),
          W = as.numeric(W),
          OTL = as.numeric(OTL),
          Pts = as.numeric(Pts)
        ) |>
        gt() |>
        cols_hide(
          c(
            Pos,
            Role
          )
        ) |>
        fmt_markdown(
          columns = c(
            Headshot,
            Logo
          )
        ) |>
        cols_align(
          align = "center",
          columns = c(
            `#`,
            Headshot,
            G,
            A,
            SH,
            BLK,
            SVS,
            SO,
            W,
            OTL,
            Pts
          )
        ) |>
        cols_align(
          align = "left",
          columns = Name
        ) |>
        tab_options(
          table.background.color = '#F5F5F5',
          column_labels.background.color = '#2B2D42',
          table.font.size = px(16),
          table.border.top.color = 'transparent',
          table.border.bottom.color = 'transparent',
          table_body.hlines.color = 'transparent',
          table_body.border.bottom.color = 'transparent',
          column_labels.border.bottom.color = 'transparent',
          column_labels.border.top.color = 'transparent'
        ) |>
        cols_label(
          `#` = "",
          Headshot = ""
        ) |>
        opt_css(
          css = '
      table tr:nth-child(odd) {
      background-color: #e0dedeff;
      }
      .gt_col_heading {
      position: sticky !important;
      top: 0px !important;
      z-index: 10 !important;
      }
      '
        ) |>
        cols_width(
          "#" ~ px(40),
          Headshot ~ px(50),
          Name ~ px(160),
          G ~ px(40),
          A ~ px(40),
          SH ~ px(40),
          BLK ~ px(40),
          SVS ~ px(40),
          SO ~ px(40),
          W ~ px(40),
          OTL ~ px(40),
          Pts ~ px(60)
        ) |>
        tab_header(
          title = div(
            HTML(
              web_image(
                fantasy_teams[[self$team]]$info$team_image
              )
            ),
            div(
              self$team
            ),
            HTML(
              web_image(
                fantasy_teams[[self$team]]$info$team_image
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
          heading.background.color = fantasy_teams[[
            self$team
          ]]$info$team_colour,
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
    },

    make_email = function() {
      unique_players <- self$game_outcomes |>
        filter(statistic == "Unique Players") |>
        select(value) |>
        as.numeric()

      unique_games <- self$game_outcomes |>
        filter(statistic == "Unique Games") |>
        select(value) |>
        as.numeric()

      game_table <- self$make_game_table()

      fantasy_point_table <- self$make_point_table()

      three_stars_table <- self$make_three_stars_table()

      compose_email(
        body = md(
          glue(
            "
# Hello {self$team},

Here is your weekly PWHL Fantasy League Update! This week's email is sponsored by Meadow and Rambouillet. Thank you to our sponsors!

## Games

During the week between {format(current_date-7, '%A, %B %d')} and {format(current_date, '%A, %B %d')} {self$team} had {unique_players} out of 12 players playing across {unique_games} game(s).

{stringr::str_squish(as_raw_html(game_table))}

## Points

{stringr::str_squish(as_raw_html(fantasy_point_table))}

# Overall

Over the past week 

{stringr::str_squish(as_raw_html(weekly_standings_table))}

"
          )
        )
      )
    }
  )
)


WeeklyUpdate$new("The Cry Babies (Alex)", current_date)$make_email()
# noah <- make_email("Nog's Bologs (Noah)")
# karl <- make_email("Hintonburg Sphynxes (Karl)")
# reiko <- make_email("Zillenial Zingers (Reiko)")
