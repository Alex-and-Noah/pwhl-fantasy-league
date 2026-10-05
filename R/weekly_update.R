library(blastula)

df <- read.csv("static/data/emails.csv")

for (team in fantasy_teams |> names()) {
  make_email(team)
}

make_email <- function(team) {
  browser()

  compose_email(
    body = md(
      glue::glue(
        "
## Hello {}

This is an email.
"
      )
    ),
    footer = md(
      "
Code by Alex <3
"
    )
  )
}
