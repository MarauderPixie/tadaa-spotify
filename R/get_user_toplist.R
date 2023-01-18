#' Get JSON file of current users top tracks or artists
#'
#' @param type can be "artists" or "tracks"
#' @param time_range can be "short_term" (last ~4 weeks), "medium_term" (last ~6 months) or "long_term" (several years)
#' @param n number of entries to return (must be between 1 and 50)
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_user_toplist(token = my_token) # get users top 20 artists of the last 6 months
#' # get_user_toplist("tracks", "short_term", 10, my_token) # get users top 10 songs of the last 4 weeks
get_user_toplist <- function(type = "artists", time_range = "medium_term", n = 20, token) {
  if (!(type %in% c("artists", "tracks"))){
    stop("type must be one of: artists, tracks")
  }

  if (!(time_range %in% c("long_term", "medium_term", "short_term"))){
    stop("`time_range` must be one of: long_term, medium_term, short_term\n")
  }

  if (n > 50 | n < 1) {
    stop("n must be between 1 and 50")
  }

  url <- ifelse(type == "artists",
                "https://api.spotify.com/v1/me/top/artists",
                "https://api.spotify.com/v1/me/top/tracks")

  json <- httr::GET(url,
                    httr::config(token = token),
                    query = list(time_range = time_range, limit = n)) %>%
    httr::content() %>%
    purrr::pluck("items")
    # purrr::flatten()

  return(json)
}
