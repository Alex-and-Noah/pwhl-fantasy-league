library(tidyr)
library(dplyr)
library(magrittr)
library(gt)
library(gtExtras)
library(htmltools)

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
      goals,
      assists,
      shots,
      fantasy_points
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
    )

  goalies <- fantasy_teams[[
    fantasy_team_name
  ]]$roster$goalies |>
    mutate(
      saves = shots - goals_against
    ) |>
    select(
      name,
      tp_jersey_number,
      position,
      player_headshot,
      team_id,
      saves,
      fantasy_points
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
    )

  gt_table <- bind_rows(
    skaters,
    goalies
  ) |>
    merge(
      team_info
    ) |>
    select(
      tp_jersey_number,
      player_headshot,
      name,
      position,
      team_id,
      team_logo,
      colours_1,
      goals,
      assists,
      shots,
      saves,
      fantasy_points
    ) |>
    mutate(
      role = position
    ) |>
    rename(
      c(
        "#" = "tp_jersey_number",
        Headshot = "player_headshot",
        Name = "name",
        Pos = "position",
        Role = "role",
        Team = "team_id",
        Logo = "team_logo",
        Colour = "colours_1",
        G = "goals",
        A = "assists",
        SH = "shots",
        SVS = "saves",
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
      SVS,
      Pts,
    ) |>
    mutate(
      G = as.numeric(G),
      A = as.numeric(A),
      SH = as.numeric(SH),
      SVS = as.numeric(SVS)
    ) %>%
    rbind(
      c(
        "",
        "",
        "",
        "",
        "F",
        "Total (F)",
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(G)
        ),
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(A)
        ),
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(SH)
        ),
        "",
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(Pts)
        )
      ),
      c(
        "",
        "",
        "",
        "",
        "D",
        "Total (D)",
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(G)
        ),
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(A)
        ),
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(SH)
        ),
        "",
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(Pts)
        )
      ),
      c(
        "",
        "",
        "",
        "",
        "G",
        "Total (G)",
        "",
        "",
        "",
        sum(
          . |>
            filter(
              Role == "G"
            ) |>
            select(SVS)
        ),
        sum(
          . |>
            filter(
              Role == "G"
            ) |>
            select(Pts)
        )
      ),
      c(
        "",
        "",
        "",
        "",
        "all",
        "Total",
        "",
        "",
        "",
        "",
        sum(
          .$Pts
        )
      )
    ) |>
    arrange(
      factor(
        Role,
        levels = c(
          "F",
          "D",
          "G"
        )
      ),
      "#"
    ) |>
    gt() |>
    fmt_markdown(
      columns = c(
        Headshot,
        Logo
      )
    ) |>
    tab_header(
      title = div(
        HTML(
          web_image(
            fantasy_team_info$team_image
          )
        ),
        div(
          fantasy_team_name
        ),
        HTML(
          web_image(
            fantasy_team_info$team_image
          )
        ),
        style = css(
          `display` = "flex",
          `justify-content` = "center",
          `align-items` = "center"
        )
      )
    ) |>
    cols_hide(
      c(
        "#",
        Pos,
        Role
      )
    ) |>
    cols_align(
      align = "center",
      columns = c(
        # "#",
        Logo,
        Headshot,
        G,
        A,
        SH,
        SVS,
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
    tab_style(
      style = list(
        cell_fill(
          color = '#2B2D42'
        ),
        cell_text(
          color = "white"
        )
      ),
      locations = cells_body(
        rows = c(
          nrow(
            skaters |>
              filter(
                position == "F"
              )
          ) +
            1,
          nrow(
            skaters
          ) +
            2,
          nrow(
            skaters
          ) +
            nrow(
              goalies
            ) +
            3
        )
      )
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
    ) |>
    cols_width(
      Headshot ~ px(50),
      Name ~ px(160),
      Logo ~ px(70),
      Pts ~ px(60),
      everything() ~ px(40)
    ) |>
    sub_missing(
      everything(),
      missing_text = "-"
    )

  return(
    gt_table
  )
}
