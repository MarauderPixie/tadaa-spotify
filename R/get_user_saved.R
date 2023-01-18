#' Get JSON file of current users saved tracks or albums
#'
#' @param type can be "albums" (default) or "tracks"
#' @param n number of entries to return
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @import progress
#' @export
#'
#' @examples
#' # nothing to see for now
get_user_saved <- function(type = "albums", n = 20, token) {
  url <- ifelse(type == "albums",
                "https://api.spotify.com/v1/me/albums",
                "https://api.spotify.com/v1/me/tracks")
  
  if (n > 50){
    teiler <- floor(n / 50)
    rest   <- n %% 50
    
    full_batches  <- rep(50, teiler)
    actual_offset <- cumsum(full_batches)
    # probably map2 at some point?
    # this block an the next need some cleaning, anyway
    o <- c(0, actual_offset)
    l <- c(full_batches, rest)
    # cat(i, "\r\n", l)
  } else {
    o <- 0
    l <- n
  }
  
  if (n > 500){
    pb <- progress::progress_bar$new(
      total = n,
      format = paste("fetching", type, "[:bar] (:percent)")
    )
  }
  
  json <- map2(o, l, function(offset, limit) {
    if (n > 500){
      # for some reason it breaks on 50
      pb$tick(49)
    }
    
    httr::GET(url,
              httr::config(token = token),
              query = list(limit = limit, offset = offset)) %>%
      httr::content() %>%
      purrr::pluck("items")
  }) %>%
    purrr::flatten()
  
  return(json)
}
