# PWHL Fantasy League

Our PWHL Fantasy League organization tool.

## Weekly Updates

Weekly updates are a special snapshot of the fantasy league done via github action each sunday. Using `pull_data.R`, weekly changes and news is computed in `weekly_update.R`. This file also creates html blastula email contents and saves it as an .RDS file. These changes are committed to a special data branch that is used for futher automation.
