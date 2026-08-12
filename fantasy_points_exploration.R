library(dplyr)
library(ggplot2)

team_stats <- readRDS(
  "team_stats.rds"
)

all_skaters <- bind_rows(
  lapply(
    team_stats,
    `[[`,
    1
  )
) |>
  mutate(
    fantasy_points = 2*(
      goals - short_handed_goals
    ) +
    3*short_handed_goals +
    1*(
      assists - short_handed_assists
    ) +
    2*short_handed_assists +
    0.1*shots + 
    0.1*shots_blocked_by_player
    # 0.05*faceoff_wins
  ) |>
  select(
    name,
    team_code,
    rookie,
    position,
    goals,
    assists,
    shots,
    fantasy_points
  )

all_goalies <- bind_rows(
  lapply(
    team_stats,
    `[[`,
    2
  )
) |> 
  mutate(
    fantasy_points = 1*shutouts +
    0.05*(
      shots - goals_against
    )
  ) |>
  select(
    name,
    team_code,
    rookie,
    position,
    shots,
    goals_against,
    fantasy_points
  )

all_fantasy_points <- bind_rows(
  all_skaters |>
    select(
      name,
      team_code,
      rookie,
      position,
      fantasy_points
    ),
  all_goalies |>
    select(
      name,
      team_code,
      rookie,
      position,
      fantasy_points
    )
) %>%
  mutate(
    position = replace_values(
      .$position,
      "C" ~ "F",
      "LW" ~ "F",
      "RW" ~ "F",
      "F" ~ "F",
      "LD" ~ "D",
      "RD" ~ "D",
      "D" ~ "D",
      "G" ~ "G"
    ),
    draft_round = 0,
    draft_position = 0
  )



bind_rows(
  all_skaters |>
    filter(
      rookie == 0
    ) |>
    summarise(
      name = "non_rookie_skaters",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_skaters |>
    filter(
      rookie == 1
    ) |>
    summarise(
      name = "rookie_skaters",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_skaters |>
    summarise(
      name = "all_skaters",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    filter(
      rookie == 0
    ) |>
    summarise(
      name = "non_rookie_goalies",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    filter(
      rookie == 1
    ) |>
    summarise(
      name = "rookie_goalies",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    summarise(
      name = "all_goalies",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    ),
  all_fantasy_points |>
    summarise(
      name = "all_players",
      min = min(
        fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        fantasy_points,
        na.rm = TRUE
      )
    )
)

summary(
  all_skaters |>
    filter(
      rookie == 1
    ) |>
    select(
      fantasy_points
    )
)

summary(
  all_skaters |>
    filter(
      rookie == 1
    ) |>
    select(
      fantasy_points
    )
)


# Look at upper half
ggplot(
  all_fantasy_points,
  aes(
    x = fantasy_points,
    after_stat(density),
    colour = position
  )
) +
  geom_freqpoly(
  ) +
  labs(
    title = "Fantasy points",
    x = "Values",
    y = "Count"
  ) +
  theme_minimal()

hist(all_skaters$fantasy_points)
hist(all_goalies$fantasy_points)
