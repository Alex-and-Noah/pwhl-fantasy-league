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
    shots,
    goals_against,
    fantasy_points
  )

all_fantasy_points <- bind_rows(
  all_skaters |>
    select(
      name,
      team_code,
      fantasy_points
    ) |>
    mutate(
      position = "skater"
    ) |>
    filter(
      fantasy_points > median(
        fantasy_points
      )
    ),
  all_goalies |>
    select(
      name,
      team_code,
      fantasy_points
    ) |>
    mutate(
      position = "goalie"
    ) |>
    filter(
      fantasy_points > median(
        fantasy_points
      )
    )
)

summary(all_skaters$fantasy_points)
summary(all_goalies$fantasy_points)

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
