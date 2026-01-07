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
#' @param schedule PWHL schedule
#' @return gt() table of fantasy roster
#' @import tidyr
#' @import dplyr
#' @import magrittr
#' @import gt
#' @export

generate_pwhl_roster_gt_table <- function(
    name,
    team_rosters,
    fantasy_roster_points,
    schedule
) {
    data <- merge(
        team_rosters[[name]],
        fantasy_roster_points[[name]],
        by = c(
            "player_id",
            "acquired",
            "let_go",
            "first_name",
            "last_name",
            "position",
            "team_id"
        ),
        all.x = TRUE
    ) |>
        select(
            c(
                jersey_number,
                team_logo,
                team_colour_1,
                player_headshot,
                player_name,
                position,
                fantasy_points,
                goals,
                assists,
                wins,
                ot_losses,
                acquired,
                let_go
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
                position,
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
                Pos = 'position',
                Pts = 'fantasy_points',
                G = 'goals',
                A = 'assists',
                W = 'wins',
                OTL = 'ot_losses'
            )
        ) |>
        mutate(
            Headshot = paste0(
                "<img src='",
                Headshot,
                "' style='width:50px;height:50px;border:1px solid",
                team_colour_1,
                ";border-radius:50%;'/>"
            ),
            Logo = paste0(
                "<img src='",
                Logo,
                "' style='width:30px;height:30px;'/>"
            )
        ) |>
        select(
            -team_colour_1
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
                sum(.$OTL),
                NA,
                NA
            )
        ) |>
        gt() |>
        fmt_markdown(
            columns = c(
                Headshot,
                Logo
            )
        ) |>
        # text_transform(
        #     locations = cells_body(
        #         columns = Headshot,
        #         rows = 1:(length(Headshot) - 1)
        #     ),
        #     fn = function(x) {
        #         web_image(
        #             url = x,
        #             height = '50px'
        #         )
        #     }
        # ) |>
        # text_transform(
        #     locations = cells_body(
        #         columns = Logo,
        #         rows = 1:(length(Headshot) - 1)
        #     ),
        #     fn = function(x) {
        #         web_image(
        #             url = x,
        #             height = '30px'
        #         )
        #     }
        # ) |>
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
        )

    if (
        nrow(
            team_rosters[[name]] |>
                filter(
                    !is.na(acquired)
                )
        ) >
            0
    ) {
        trade_date <- schedule |>
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
            pull()

        data <- data |>
            tab_footnote(
                footnote = paste0(
                    "Acquired ",
                    format(
                        trade_date,
                        "%b %d, %Y"
                    )
                ),
                locations = cells_body(
                    columns = Name,
                    rows = !is.na(acquired)
                ),
            ) |>
            tab_footnote(
                footnote = paste0(
                    "Let go ",
                    format(
                        trade_date,
                        "%b %d, %Y"
                    )
                ),
                locations = cells_body(
                    columns = Name,
                    rows = !is.na(let_go)
                ),
            )
    }

    data <- data |>
        cols_hide(
            c(
                acquired,
                let_go
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
