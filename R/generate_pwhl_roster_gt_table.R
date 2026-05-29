library(tidyr)
library(dplyr)
library(magrittr)
library(gt)

#' @title  **Generate Fantasy Roster table**
#' @description Get a gt() table of a fantasy roster
#'
#' @param fantasy_team_name Fantasy team name
#' @param fantasy_teams Fantasy team info and rosters
#' @param team_info PWHL team info
#' @return gt() table of fantasy roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_pwhl_roster_gt_table <- function(
    fantasy_team_name,
    fantasy_teams,
    team_info
) {
  fantasy_team_info <- fantasy_teams[[
    fantasy_team_name
  ]]$info

  skaters <- fantasy_teams[[
    fantasy_team_name
  ]]$roster$skaters |>
  select(
    name,
    tp_jersey_number,
    position,
    player_headshot,
    team_id,
    fantasy_points
  )

  goalies <- fantasy_teams[[
    fantasy_team_name
  ]]$roster$goalies |>
  select(
    name,
    tp_jersey_number,
    position,
    player_headshot,
    team_id,
    fantasy_points
  )

  data <- bind_rows(
    skaters,
    goalies
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
    merge(
      team_info
    ) |> select(
      name,
      tp_jersey_number,
      position,
      player_headshot,
      team_id,
      fantasy_points,
      team_logo,
      colours_1
    ) |>
    arrange(
        factor(
          position,
          levels = c(
            "F",
            "D",
            "G"
          )
        ),
        tp_jersey_number
    ) |>
    rename(
      c(
        Name = "name",
        "#" = "tp_jersey_number",
        Pos = "position",
        Headshot = "player_headshot",
        Team = "team_id",
        Pts = "fantasy_points",
        Logo = "team_logo",
        colour  ="colours_1"
      )
    ) |> mutate(
        Headshot = paste0(
          "<img src='",
          Headshot,
          "' style='width:50px;height:50px;border:1px solid",
          colour,
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
      Logo,
      Headshot,
      Name,
      Pos,
      Pts,
    ) %>%
    rbind(
      c(
        "",
        "",
        "",
        "",
        "Total",
        sum(.$Pts)
      )
    ) |>
    gt() |>
    fmt_markdown(
        columns = c(
            Headshot,
            Logo
        )
    ) |>
    cols_align(
      align = "center",
      columns = c(
        "#",
        Logo,
        Headshot,
        Name,
        Pos,
        Pts
      )
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
    tab_style_body(
      style = cell_borders(
        sides = c('top', 'right', 'left', 'bottom'),
        weight = px(0) # Remove row borders
      ),
      fn = function(x) {
        is.numeric(x) | is.character(x)
      }
    ) |>
    cols_label(
      Headshot = "",
      Logo = "Team"
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
    )

  return(data)
}
