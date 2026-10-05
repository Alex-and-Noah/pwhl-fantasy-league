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
        ) +
          2,
        nrow(
          skaters
        ) +
          3
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
        ),
      .which = 1
    ) %>%
    grp_replace(
      grp_pull(
        .,
        which = 2
      ) |>
        sub_missing(
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
      ) |>
        sub_missing(
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
      ) |>
        sub_missing(
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
      fantasy_points = 2 *
        goals +
        1 * assists +
        0.1 * shots +
        0.1 * shots_blocked_by_player +
        2 * wins +
        1 * ot_losses
    )

  for (fantasy_team_name in names(fantasy_teams)) {
    skaters <- skaters |>
      mutate(
        {{ fantasy_team_name }} := if_else(
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
      fantasy_points = 1 *
        shutouts + #1*goals +
        # 1*assists +
        0.05 * saves +
        2 * wins +
        1 * ot_losses
    )

  for (fantasy_team_name in names(fantasy_teams)) {
    goalies <- goalies |>
      mutate(
        {{ fantasy_team_name }} := if_else(
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
        ) +
          2,
        nrow(
          skaters
        ) +
          3
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
        ),
      .which = 1
    ) %>%
    grp_replace(
      grp_pull(
        .,
        which = 2
      ) |>
        sub_missing(
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
      ) |>
        sub_missing(
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
      ) |>
        sub_missing(
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


library(tidyr)
library(dplyr)
library(magrittr)
library(gt)

#' @title  **Generate Fantasy Trade table row**
#' @description Get a gt() table row of a fantasy trades
#'
#' @param name Fantasy team name
#' @param team_rosters Fantasy team rosters
#' @param fantasy_roster_points Fantasy team roster points
#' @param schedule PWHL schedule
#' @return gt() table of fantasy roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_roster_trade_gt_table_row <- function(
  name,
  team_rosters,
  fantasy_roster_points,
  schedule
) {
  tradees <- team_rosters[[name]] |>
    filter(
      !is.na(acquired) | !is.na(let_go)
    )

  if (
    nrow(
      tradees
    ) >
      0
  ) {
    df <- data.frame(
      name = name,
      date = schedule |>
        filter(
          game_id ==
            team_rosters[[name]] |>
              filter(
                !is.na(acquired)
              ) |>
              select(
                acquired
              ) |>
              pull()
        ) |>
        select(
          game_date
        ) |>
        pull(),
      acquired = tradees |>
        filter(
          !is.na(acquired)
        ) |>
        select(
          player_name
        ) |>
        pull(),
      let_go = tradees |>
        filter(
          !is.na(let_go)
        ) |>
        select(
          player_name
        ) |>
        pull()
    )
  } else {
    df <- data.frame(
      name = character(),
      date = as.Date(character()),
      acquired = character(),
      let_go = character()
    )
  }

  return(df)
}

# From: https://github.com/rstudio/gt/pull/2132/changes
#------------------------------------------------------------------------------#
#
#                /$$
#               | $$
#     /$$$$$$  /$$$$$$
#    /$$__  $$|_  $$_/
#   | $$  \ $$  | $$
#   | $$  | $$  | $$ /$$
#   |  $$$$$$$  |  $$$$/
#    \____  $$   \___/
#    /$$  \ $$
#   |  $$$$$$/
#    \______/
#
#  This file is part of the 'rstudio/gt' project.
#
#  Copyright (c) 2018-2026 gt authors
#
#  For full copyright and license information, please look at
#  https://gt.rstudio.com/LICENSE.html
#
#------------------------------------------------------------------------------#

# gt_split() -------------------------------------------------------------------
#' Split a table into a group of tables (a `gt_group`)
#'
#' @description
#'
#' With a **gt** table, you can split it into multiple tables and get that
#' collection in a `gt_group` object. This function is useful for those cases
#' where you want to section up a table in a specific way and print those
#' smaller tables across multiple pages (in RTF and Word outputs, primarily via
#' [gtsave()]), or, with breaks between them when the output context is HTML.
#'
#' @inheritParams fmt_number
#'
#' @param row_every_n *Split at every n rows*
#'
#'   `scalar<numeric|integer>` // *default:* `NULL` (`optional`)
#'
#'   A directive to split at every *n* number of rows. This argument expects a
#'   single numerical value.
#'
#' @param row_slice_i *Row-slicing indices*
#'
#'   `vector<numeric|integer>` // *default:* `NULL` (`optional`)
#'
#'   An argument for splitting at specific row indices. Here, we expect either a
#'   vector of index values or a function that evaluates to a numeric vector.
#'
#' @param col_slice_at *Column-slicing locations*
#'
#'   `<column-targeting expression>` // *default:* `NULL` (`optional`)
#'
#'   Any columns where vertical splitting across should occur. The splits occur
#'   to the right of the resolved column names. Can either be a series of column
#'   names provided in `c()`, a vector of column indices, or a select helper
#'   function (e.g. [starts_with()], [ends_with()], [contains()], [matches()],
#'   [num_range()], and [everything()]).
#'
#' @return An object of class `gt_group`.
#'
#' @section Examples:
#'
#' Use a subset of the [`gtcars`] dataset to create a **gt** table. Format the
#' `msrp` column to display numbers as currency values, set column widths with
#' [cols_width()], and split the table at every five rows with `gt_split()`.
#' This creates a `gt_group` object containing two tables. Printing this object
#' yields two tables separated by a line break.
#'
#' ```r
#' gtcars |>
#'   dplyr::slice_head(n = 10) |>
#'   dplyr::select(mfr, model, year, msrp) |>
#'   gt() |>
#'   fmt_currency(columns = msrp) |>
#'   cols_width(
#'     year ~ px(80),
#'     everything() ~ px(150)
#'   ) |>
#'   gt_split(row_every_n = 5)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_gt_split_1.png")`
#' }}
#'
#' Use a smaller subset of the [`gtcars`] dataset to create a **gt** table.
#' Format the `msrp` column to display numbers as currency values, set the table
#' width with [tab_options()] and split the table at the `model` column This
#' creates a `gt_group` object again containing two tables but this time we get
#' a vertical split. Printing this object yields two tables of the same width.
#'
#' ```r
#' gtcars |>
#'   dplyr::slice_head(n = 5) |>
#'   dplyr::select(mfr, model, year, msrp) |>
#'   gt() |>
#'   fmt_currency(columns = msrp) |>
#'   tab_options(table.width = px(400)) |>
#'   gt_split(col_slice_at = "model")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_gt_split_2.png")`
#' }}
#'
#' @family table group functions
#' @section Function ID:
#' 14-2
#'
#' @section Function Introduced:
#' `v0.9.0` (Mar 31, 2023)
#'
#' @export
custom_gt_split <- function(
  data,
  row_every_n = NULL,
  row_slice_i = NULL,
  col_slice_at = NULL
) {
  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Resolution of columns as character vectors
  col_slice_at <-
    resolve_cols_c(
      expr = {{ col_slice_at }},
      data = data,
      null_means = "nothing"
    )

  gt_tbl_built <- build_data(data = data, context = "html")

  # Get row count for table (data rows)
  n_rows_data <- nrow(gt_tbl_built[["_stub_df"]])

  row_slice_vec <- rep.int(1L, n_rows_data)

  row_every_n_idx <- NULL
  if (!is.null(row_every_n)) {
    row_every_n_idx <- seq_len(n_rows_data)[seq(0, n_rows_data, row_every_n)]
  }

  row_slice_i_idx <- NULL
  if (!is.null(row_slice_i)) {
    row_slice_i_idx <- row_slice_i
  }

  row_idx <- sort(unique(c(row_every_n_idx, row_slice_i_idx)))

  group_i <- 0L

  for (i in seq_along(row_slice_vec)) {
    if (i %in% (row_idx + 1)) {
      group_i <- group_i + 1L
    }

    row_slice_vec[i] <- row_slice_vec[i] + group_i
  }

  row_range_list <-
    split(
      seq_len(n_rows_data),
      row_slice_vec
    )

  gt_tbl_main <- data

  gt_group <- gt_group(.use_grp_opts = FALSE)

  for (i in seq_along(row_range_list)) {
    gt_tbl_i <- gt_tbl_main

    gt_tbl_i[["_data"]] <- gt_tbl_i[["_data"]][row_range_list[[i]], ]
    gt_tbl_i[["_stub_df"]] <- gt_tbl_i[["_stub_df"]][
      seq_along(row_range_list[[i]]),
    ]

    if (!is.null(col_slice_at)) {
      # Get all visible vars in their finalized order
      visible_col_vars <- dt_boxhead_get_vars_default(data = data)

      # Stop function if any of the columns to split at aren't visible columns
      if (!all(col_slice_at %in% visible_col_vars)) {
        cli::cli_abort(
          "All values provided in `col_slice_at` must correspond to visible columns."
        )
      }

      # Obtain all of the column indices for vertical splitting
      col_idx <- which(visible_col_vars %in% col_slice_at)

      col_slice_vec <- rep.int(1L, length(visible_col_vars))

      group_j <- 0L

      for (i in seq_along(col_slice_vec)) {
        if (i %in% (col_idx + 1)) {
          group_j <- group_j + 1L
        }

        col_slice_vec[i] <- col_slice_vec[i] + group_j
      }

      col_range_list <-
        split(
          seq_along(visible_col_vars),
          col_slice_vec
        )

      for (j in seq_along(col_range_list)) {
        gt_tbl_j <- gt_tbl_i

        gt_tbl_j[["_data"]] <-
          gt_tbl_j[["_data"]][, visible_col_vars[col_range_list[[j]]]]

        gt_tbl_j[["_boxhead"]] <-
          gt_tbl_j[["_boxhead"]][
            gt_tbl_j[["_boxhead"]]$var %in%
              visible_col_vars[col_range_list[[j]]],
          ]

        gt_group <- grp_add(gt_group, gt_tbl_j)
      }
    } else {
      gt_group <- grp_add(gt_group, gt_tbl_i)
    }
  }

  gt_group
}
