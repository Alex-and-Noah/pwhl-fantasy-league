library(tidyr)
library(dplyr)
library(magrittr)
library(gt)
library(gtExtras)
library(htmltools)

#' @title  **Generate PWHL Roster table**
#' @description Get a gt() table of a PWHL roster
#'
#' @param team_stats PWHL team stats
#' @param team_code PWHL team code
#' @param team_info PWHL team info
#' @return gt() table of PWHL roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_pwhl_roster_gt_table <- function(
  team_stats,
  team_code,
  team_info
) {
  pwhl_team_stats <- team_stats[[
    team_code
  ]]

  pwhl_team_info <- team_info |>
    filter(
      team_code == .env$team_code
    )

  skaters <- team_stats[[
    team_code
  ]]$skaters |>
    select(
      name,
      tp_jersey_number,
      position,
      player_headshot,
      team_id,
      goals,
      assists,
      shots
      # fantasy_points
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
      ),
      fantasy_points = as.numeric(goals) + as.numeric(assists) + 0.05*as.numeric(shots)
    )

  goalies <- team_stats[[
    team_code
  ]]$goalies |>
    mutate(
      saves = shots - goals_against
    ) |>
    select(
      name,
      tp_jersey_number,
      position,
      player_headshot,
      team_id,
      saves
      # fantasy_points
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
      ),
      fantasy_points = 0.05*as.numeric(saves)
    )

  gt_table <- bind_rows(
    skaters,
    goalies
  ) |>
    mutate(
      team_logo = pwhl_team_info$team_logo,
      colours_1 = pwhl_team_info$colours_1
    ) |>
    select(
      tp_jersey_number,
      player_headshot,
      name,
      position,
      team_id,
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
      )
    ) |>
    select(
      "#",
      Headshot,
      Name,
      Pos,
      Role,
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
        "Total",
        "F",
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
        NA,
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
        "Total",
        "D",
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
        NA,
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
        "Total",
        "G",
        NA,
        NA,
        NA,
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
        "Overall team stats",
        "",
        "all",
        sum(
          .$G,
          na.rm = TRUE
        ),
        sum(
          .$A,
          na.rm = TRUE
        ),
        sum(
          .$SH,
          na.rm = TRUE
        ),
        sum(
          .$SVS,
          na.rm = TRUE
        ),
        sum(
          .$Pts,
          na.rm = TRUE
        )
      )
    ) %>%
    arrange(
      factor(
        Role,
        levels = c(
          "all",
          "F",
          "D",
          "G"
        )
      ),
      as.numeric(
        `#`
      )
      # factor(
      #   `#`,
      #   levels = c(
      #     unique(
      #       .$`#`
      #     )[
      #       nzchar(
      #         unique(
      #           .$`#`
      #         )
      #       )
      #     ],
      #     ""
      #   )
      # )
    ) |>
    gt() |>
    fmt_markdown(
      columns = c(
        Headshot
      )
    ) |>
    # tab_header(
    #   title = div(
    #     HTML(
    #       web_image(
    #         pwhl_team_info$team_logo
    #       )
    #     ),
    #     div(
    #       pwhl_team_info$team_name
    #     ),
    #     HTML(
    #       web_image(
    #         pwhl_team_info$team_logo
    #       )
    #     ),
    #     style = css(
    #       `display` = "flex",
    #       `justify-content` = "center",
    #       `align-items` = "center"
    #     )
    #   )
    # ) |>
    cols_hide(
      c(
        Pos,
        Role
      )
    ) |>
    cols_align(
      align = "center",
      columns = c(
        # "#",
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
    # tab_style(
    #   style = list(
    #     cell_fill(
    #       color = '#2B2D42'
    #     ),
    #     cell_text(
    #       color = "white"
    #     )
    #   ),
    #   locations = cells_body(
    #     rows = c(
    #       nrow(
    #         skaters |>
    #           filter(
    #             position == "F"
    #           )
    #       ) +
    #         1,
    #       nrow(
    #         skaters
    #       ) +
    #         2,
    #       nrow(
    #         skaters
    #       ) +
    #         nrow(
    #           goalies
    #         ) +
    #         3
    #     )
    #   )
    # ) |>
    # tab_style_body(
    #   style = cell_borders(
    #     sides = c('top', 'right', 'left', 'bottom'),
    #     weight = px(0) # Remove row borders
    #   ),
    #   fn = function(x) {
    #     is.numeric(x) | is.character(x)
    #   }
    # ) |>
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
      Headshot ~ px(50),
      Name ~ px(160),
      Pts ~ px(60),
      everything() ~ px(40)
    ) |>
    gt::gt_split(
      row_slice_i = c(
        1,
        nrow(
          skaters |>
            filter(
              position == "F"
            )
        ) + 2,
        nrow(
          skaters
        ) + 3
      )
    ) |>
    grp_options(
      table.width = pct(100)
    ) %>%
      grp_replace(
        grp_pull(
          .,
          which = 1
        ) |> sub_missing(
            everything(),
            missing_text = "-"
          ) |>
          tab_header(
            title = div(
              HTML(
                web_image(
                  pwhl_team_info$team_logo
                )
              ),
              div(
                pwhl_team_info$team_name
              ),
              HTML(
                web_image(
                  pwhl_team_info$team_logo
                )
              ),
              style = css(
                `display` = "flex",
                `justify-content` = "center",
                `align-items` = "center"
              )
            )
          ) |> 
          cols_label(
            Name = ""
          ),
        .which = 1
      ) %>%
      grp_replace(
        grp_pull(
          .,
          which = 2
        ) |> sub_missing(
            everything(),
            missing_text = "-"
          ) |>
          tab_header(
            title = "Forwards"
          ) |>
           opt_align_table_header(align = "left"),
        .which = 2
      ) %>%
      grp_replace(
        grp_pull(
          .,
          which = 3
        ) |> sub_missing(
            everything(),
            missing_text = "-"
          ) |>
          tab_header(
            title = "Defenders"
          ) |>
           opt_align_table_header(align = "left"),
        .which = 3
      ) %>%
      grp_replace(
        grp_pull(
          .,
          which = 4
        ) |> sub_missing(
            everything(),
            missing_text = "-"
          ) |>
          tab_header(
            title = "Goalies"
          ) |>
           opt_align_table_header(align = "left"),
        .which = 4
      )
    # sub_missing(
    #   everything(),
    #   missing_text = "-"
    # )

  return(
    gt_table
  )
}
