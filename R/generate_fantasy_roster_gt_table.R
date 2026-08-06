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

generate_fantasy_roster_gt_table <- function(
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
      shots_blocked_by_player,
      wins,
      ot_losses,
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
      shutouts,
      wins,
      ot_losses,
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

  data <- bind_rows(
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
      shots_blocked_by_player,
      saves,
      shutouts,
      wins,
      ot_losses,
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
    ) %>%
    rbind(
      c(
        "",
        "",
        "",
        "",
        "F",
        "Total",
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
        )
      ),
      c(
        "",
        "",
        "",
        "",
        "D",
        "Total",
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
        )
      ),
      c(
        "",
        "",
        "",
        "",
        "G",
        "Total",
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
        )
      ),
      c(
        "",
        "",
        "Overall team stats",
        "",
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
        )
      )
    ) |>
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
    )

    # grp_replace(
      #   grp_pull(
      #     .,
      #     which = 1
      #   ) |> sub_missing(
      #       everything(),
      #       missing_text = "-"
      #     ) |>
      #     tab_header(
      #       title = div(
      #         HTML(
      #           web_image(
      #             fantasy_team_info$team_image
      #           )
      #         ),
      #         div(
      #           fantasy_team_name
      #         ),
      #         HTML(
      #           web_image(
      #             fantasy_team_info$team_image
      #           )
      #         ),
      #         style = css(
      #           `display` = "flex",
      #           `justify-content` = "center",
      #           `align-items` = "center"
      #         )
      #       )
      #     ) |> 
      #     cols_label(
      #       Name = "",
      #       Logo = ""
      #     ),
      #   .which = 1
      # ) %>%

  # data |>
  #   filter(
  #     Name == "Overall team stats"
  #   ) |>
  #   select(
  #     G,
  #     A,
  #     SH,
  #     BLK,
  #     SVS,
  #     SO,
  #     Pts
  #   ) |>
  #   rename(
  #     "Goals (G)" = G,
  #     "Assists (A)" = A,
  #     "Shots (SH)" = SH,
  #     "Blocked shots (BLK)" = BLK,
  #     "Saves (SVS)" = SVS,
  #     "Shutouts (SO)" = SO,
  #     "Overall Points (Pts)" = Pts,
  #   ) |>
  #   mutate(
  #     stat = "Value"
  #   ) |>
  #   pivot_longer(
  #     cols = -stat,
  #     names_to = "Stat",
  #     values_to = "Value"
  #   ) %>%
  #   pivot_wider(
  #     names_from = stat,
  #     values_from = Value
  #   ) |>
  #   gt() |>
  #   tab_header(
  #     title = div(
  #       HTML(
  #         web_image(
  #           fantasy_team_info$team_image
  #         )
  #       ),
  #       div(
  #         fantasy_team_name
  #       ),
  #       HTML(
  #         web_image(
  #           fantasy_team_info$team_image
  #         )
  #       ),
  #       style = css(
  #         `display` = "flex",
  #         `justify-content` = "center",
  #         `align-items` = "center"
  #       )
  #     )
  #   ) |>
  #   # cols_align(
  #   #   align = "right",
  #   #   columns = Stat
  #   # ) |>
  #   tab_options(
  #     table.background.color = '#F5F5F5',
  #     column_labels.background.color = '#2B2D42',
  #     table.font.size = px(16),
  #     table.border.top.color = 'transparent',
  #     table.border.bottom.color = 'transparent',
  #     table_body.hlines.color = 'transparent',
  #     table_body.border.bottom.color = 'transparent',
  #     column_labels.border.bottom.color = 'transparent',
  #     column_labels.border.top.color = 'transparent'
  #   ) |> opt_css(
  #     css = '
  #     table tr:nth-child(odd) {
  #     background-color: #e0dedeff;
  #     }
  #     .gt_col_heading {
  #     position: sticky !important;
  #     top: 0px !important;
  #     z-index: 10 !important;
  #     }
  #     '
  #   )

  

  gt_table <- data |>
    gt() |>
    fmt_markdown(
      columns = c(
        Headshot,
        Logo
      )
    ) |>
    # tab_header(
    #   title = div(
    #     HTML(
    #       web_image(
    #         fantasy_team_info$team_image
    #       )
    #     ),
    #     div(
    #       fantasy_team_name
    #     ),
    #     HTML(
    #       web_image(
    #         fantasy_team_info$team_image
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
