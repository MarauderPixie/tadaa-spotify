#' Get an OAuth token from spotify
#'
#' credentials can be looked up at your spotify dev / app
#' client_id and client_secret are stored in my .Renviron
#'
#' this function(s) are humbly taken in about equal parts from:
#' https://github.com/tiagomendesdantas/Rspotify
#' https://github.com/jemus42/tRakt/blob/master/R/api.R
#'
#' @param app_id the name of your spotify app as
#' @param client_id your client id, semi-optimally stored in your .Renviron
#' @param client_secret your client secret, semi-optimally stored in your .Renviron
#'
#' @return OAuth token
#' @export
#'
#' @examples
#' # none yet
spotifyOAuth <- function(app_id, client_id, client_secret){
  client_id     <- ifelse(missing(client_id), Sys.getenv("client_id"), client_id)
  client_secret <- ifelse(missing(client_secret), Sys.getenv("client_secret"), client_secret)

  spotifyR <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access    = "https://accounts.spotify.com/api/token"
  )
  # "R Web Scraper" ist der name meiner App bei spotify; die app_id also
  myapp <- httr::oauth_app("R Web Scraper", client_id, client_secret)
  return(httr::oauth2.0_token(spotifyR, myapp, scope = "user-top-read user-library-read"))
}
