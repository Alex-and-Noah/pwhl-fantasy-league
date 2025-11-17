library(tidyr)
library(dplyr)
library(magrittr)
library(gt)

#' @title  **Generate Fantasy Roster table**
#' @description Get a gt() table of a fantasy roster
#'
#' @param name Fantasy team name
#' @param team_rosters Fantasy team rosters
#' @param fantasy_roster_points Fantasy team roster points
#' @return gt() table of fantasy roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_pwhl_roster_gt_table <- function(
    name,
    team_rosters,
    fantasy_roster_points
) {
    data <- merge(
        team_rosters[[name]],
        fantasy_roster_points[[name]],
        by = "player_id",
        all.x = TRUE
    ) |>
        select(
            c(
                jersey_number,
                team_logo,
                player_headshot,
                player_name,
                position.x,
                fantasy_points,
                goals,
                assists,
                wins,
                ot_losses
            )
        ) |>
        replace_na(
            list(
                fantasy_points = 0,
                goals = 0,
                assists = 0,
                wins = 0,
                ot_losses = 0
            )
        ) |>
        arrange(
            factor(
                position.x,
                levels = c(
                    "F",
                    "D",
                    "G"
                )
            ),
            jersey_number
        ) |>
        rename(
            c(
                '#' = 'jersey_number',
                Logo = 'team_logo',
                Headshot = 'player_headshot',
                Name = 'player_name',
                Pos = 'position.x',
                Pts = 'fantasy_points',
                G = 'goals',
                A = 'assists',
                W = 'wins',
                OTL = 'ot_losses'
            )
        ) %>%
        rbind(
            c(
                "",
                "",
                "",
                "",
                "Total",
                sum(.$Pts),
                sum(.$G),
                sum(.$A),
                2 * sum(.$W),
                sum(.$OTL)
            )
        ) |>
        gt() |>
        text_transform(
            locations = cells_body(
                columns = Headshot,
                rows = 1:(length(Headshot) - 1)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = '50px'
                )
            }
        ) |>
        text_transform(
            locations = cells_body(
                columns = Logo,
                rows = 1:(length(Headshot) - 1)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = '30px'
                )
            }
        ) |>
        cols_align(
            align = "center",
            columns = c(
                "#",
                Logo,
                Headshot,
                Pos,
                Pts,
                G,
                A,
                OTL
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
