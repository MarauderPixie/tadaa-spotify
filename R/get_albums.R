#' Get JSON file of albums
#'
#' @param x a spotify album id, e.g. as returned by get_tracks()
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_albums(saved_tracks$album_id, token = my_token)
get_albums <- function(x, token) {
  
  ### only call for every album once
  # call for several albums accepts max 20 IDs,
  # see comment in get_song_features() for details
  uni_albs <- unique(x)
  n_albums <- length(uni_albs)
  matrixable <- c(uni_albs, rep(0, 20 - (n_albums %% 20)))
  calls    <- ceiling(n_albums / 20)
  
  # as of May 17th, 2020, I'm pretty sure this is rather
  # suboptimal, but too lazy to optimize
  id_strings <- matrix(matrixable, ncol = calls) %>%
    as.data.frame() %>%
    purrr::map_chr(paste, collapse = ",")
  
  json <- purrr::map(id_strings, function(id_x) {
    httr::GET("https://api.spotify.com/v1/albums",
              httr::config(token = token),
              # one or several of "album, single, appears_on, compilation"
              query = list(ids = id_x)) %>%
      httr::content() 
  }) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    purrr::discard(is.null)
  
  return(json)
}
