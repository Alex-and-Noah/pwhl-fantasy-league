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
#' @param fantasy_teams Fantasy team info
#' @return gt() table of PWHL roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_pwhl_roster_gt_table <- function(
  team_stats,
  team_code,
  team_info,
  fantasy_teams
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
      shots,
      shots_blocked_by_player,
      wins,
      ot_losses
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
      fantasy_points = 2*goals +
      1*assists +
      0.1*shots + 
      0.1*shots_blocked_by_player +
      2*wins +
      1*ot_losses
    )

  for (fantasy_team_name in names(fantasy_teams)) {
    skaters <- skaters |>
      mutate(
        {{fantasy_team_name}} := if_else(
          name %in% fantasy_teams[[fantasy_team_name]]$roster$skaters$name,
          fantasy_teams[[fantasy_team_name]]$info$team_image,
          ""
        )
      )
  }

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
      shots,
      goals_against,
      shutouts,
      wins,
      ot_losses
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
      saves = shots - goals_against,
      fantasy_points = 1*shutouts + #1*goals +
      # 1*assists +
      0.05*saves +
      2*wins +
      1*ot_losses
    )

  for (fantasy_team_name in names(fantasy_teams)) {
    goalies <- goalies |>
      mutate(
        {{fantasy_team_name}} := if_else(
          name %in% fantasy_teams[[fantasy_team_name]]$roster$goalies$name,
          fantasy_teams[[fantasy_team_name]]$info$team_image,
          ""
        )
      )
  }

  data <- bind_rows(
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
      shots_blocked_by_player,
      saves,
      shutouts,
      wins,
      ot_losses,
      fantasy_points,
      names(
        fantasy_teams
      )
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
        BLK = "shots_blocked_by_player",
        SVS = "saves",
        SO = "shutouts",
        W = "wins",
        OTL = "ot_losses",
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
      across(
        all_of(
          names(
            fantasy_teams
          )
        ),
        ~ {
          if_else(
            . != "",
            paste0(
              "<img src='",
              .,
              "' style='width:40px;height:40px;'/>"
            ),
            .
          )
        }
      ),
      Teams_temp = do.call(
        paste0,
        c(
          pick(
            names(
              fantasy_teams
            )
          )
        )
      ),
      Teams = paste0(
        "<div style='display:flex;justify-content: center;gap:5px;width:100%;'>",
        Teams_temp,
        "</div>"
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
      BLK,
      SVS,
      SO,
      W,
      OTL,
      Pts,
      Teams
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
    ) %>%
    rbind(
      c(
        "",
        "",
        "Total",
        "F",
        "",
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
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(BLK)
        ),
        NA,
        NA,
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(W)
        ),
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(OTL)
        ),
        sum(
          . |>
            filter(
              Role == "F"
            ) |>
            select(Pts)
        ),
        ""
      ),
      c(
        "",
        "",
        "Total",
        "D",
        "",
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
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(BLK)
        ),
        NA,
        NA,
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(W)
        ),
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(OTL)
        ),
        sum(
          . |>
            filter(
              Role == "D"
            ) |>
            select(Pts)
        ),
        ""
      ),
      c(
        "",
        "",
        "Total",
        "G",
        "",
        NA,
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
            select(SO)
        ),
        sum(
          . |>
            filter(
              Role == "G"
            ) |>
            select(W)
        ),
        sum(
          . |>
            filter(
              Role == "G"
            ) |>
            select(OTL)
        ),
        sum(
          . |>
            filter(
              Role == "G"
            ) |>
            select(Pts)
        ),
        ""
      ),
      c(
        "",
        "",
        "Overall team stats",
        "all",
        "",
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
          .$BLK,
          na.rm = TRUE
        ),
        sum(
          .$SVS,
          na.rm = TRUE
        ),
        sum(
          .$BLK,
          na.rm = TRUE
        ),
        sum(
          .$W,
          na.rm = TRUE
        ),
        sum(
          .$OTL,
          na.rm = TRUE
        ),
        sum(
          .$Pts,
          na.rm = TRUE
        ),
        ""
      )
    ) |>
    arrange(
      factor(
        Pos,
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
    )

  gt_table <- data |>
    gt() |>
    fmt_markdown(
      columns = c(
        Headshot,
        Teams
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
        Pts,
        Teams
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
        data |>
          filter(
            Name == "Overall team stats"
          ) |>
          select(
            G,
            A,
            SH,
            BLK,
            SVS,
            SO,
            W,
            OTL,
            Pts
          ) |>
          rename(
            "Goals (G)" = G,
            "Assists (A)" = A,
            "Shots (SH)" = SH,
            "Blocked shots (BLK)" = BLK,
            "Saves (SVS)" = SVS,
            "Shutouts (SO)" = SO,
            "Wins (W)" = W,
            "Overtime Losses (OTL)" = OTL,
            "Overall Points (Pts)" = Pts,
          ) |>
          mutate(
            stat = "Value"
          ) |>
          pivot_longer(
            cols = -stat,
            names_to = "Stat",
            values_to = "Value"
          ) %>%
          pivot_wider(
            names_from = stat,
            values_from = Value
          ) |>
          gt() |>
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
          # cols_align(
          #   align = "right",
          #   columns = Stat
          # ) |>
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
          ) |> opt_css(
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
          cols_hide(
            c(
              SVS,
              SO
            )
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
          cols_hide(
            c(
              SVS,
              SO
            )
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
          cols_hide(
            c(
              G,
              A,
              SH,
              BLK
            )
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
