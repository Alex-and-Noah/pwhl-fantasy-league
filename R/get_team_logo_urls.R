#' @title  **Get PWHL Team Logos**
#' @description Get high quality logos of PWHL teams in the current season
#'
#' @param season_id Current season ID
#' @return Named list of PWHL team logos
#' @export
#' #' @examples
#' \donttest{
#'   try(get_team_logo_urls(season_id=8))
#' }

get_team_logo_urls <- function(
  season_id
) {
  team_logo_urls <- list(
    BOS = "https://upload.wikimedia.org/wikipedia/en/b/bc/BostonFleet.png",
    MIN = "https://upload.wikimedia.org/wikipedia/en/a/ae/MinnesotaFrost.png",
    MTL = "https://upload.wikimedia.org/wikipedia/en/thumb/9/93/Montreal_Victoire_Logo.svg/800px-Montreal_Victoire_Logo.svg.png",
    NY = "https://upload.wikimedia.org/wikipedia/en/thumb/f/f9/New_York_Sirens_Logo.svg/1024px-New_York_Sirens_Logo.svg.png",
    OTT = "https://upload.wikimedia.org/wikipedia/en/thumb/4/40/Ottawa_Charge_Logo.svg/1280px-Ottawa_Charge_Logo.svg.png",
    TOR = "https://upload.wikimedia.org/wikipedia/en/thumb/5/51/Toronto_Sceptres.svg/800px-Toronto_Sceptres.svg.png"
  )

  if (season_id >= 7) {
    team_logo_urls <- c(
      team_logo_urls,
      SEA = "https://upload.wikimedia.org/wikipedia/en/6/6c/Seattle_Torrent_logo.png",
      VAN = "https://upload.wikimedia.org/wikipedia/en/thumb/7/73/Vancouver_Goldeneyes_logo.png/250px-Vancouver_Goldeneyes_logo.png"
    )
  }

  return(team_logo_urls)
}
