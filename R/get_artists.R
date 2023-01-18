#' Get JSON file of artists
#'
#' blabla
#'
#' @param artists either spotify artist_ids or strings of artist names (if given actual names, a search will be done)
#' @param by can be "name" or "id"
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_artists("James Last Orchestra", token = my_token)
#'
#' # x <- c("James Last", "Britney Spears", "Metallica")
#' # get_artists(x, token = my_token)
get_artists <- function(artists, by = "name", token) {
  if (by == "id") {
    # call for several artists returns max 50,
    # see comment in get_song_features() for details
    n_artists      <- length(artists)
    matrixable_ids <- c(artists, rep(0, 50 - (n_artists %% 50)))
    calls          <- ceiling(n_artists / 50)

    id_strings <- matrix(matrixable_ids, ncol = calls) %>%
      as.data.frame() %>%
      purrr::map_chr(paste, collapse = ",")

    # call spotify
    json <- purrr::map(id_strings, function(x) {
      httr::GET("https://api.spotify.com/v1/artists",
                httr::config(token = token),
                query = list(ids = x)) %>%
        httr::content() %>%
        # enter first level of json
        purrr::pluck("artists")
        # enter second level
        # get rid of url and other meta data (next, prev, call size...)
        # .$items
    }) %>%
      purrr::flatten()
  }

  if (by == "name") {
    json <- purrr::map(artists, function(x) {
      httr::GET("https://api.spotify.com/v1/search",
                httr::config(token = token),
                query = list(q = x, type = "artist")) %>%
        httr::content() %>%
        purrr::pluck("artists", "items", 1)
        # enter first level of json
        # .$artists %>%
        # enter second level
        # get rid of url and other meta data (next, prev, call size...)
        # .$items
    })
  }

  return(json)
}


#' Create a tidy dataframe of artists from JSON
#'
#' @param artists_json a JSON object of artists, e.g. as returned by get_artists()
#' @param image_size 1 (640x640 pixel image), 2 (320x320) or 3 (160x160)
#'
#' @return dataframe
#' @importFrom purrr pluck
#' @export
#'
#' @examples
#' # tidy_artists(artists_json, 1)
tidy_artists <- function(artists_json, image_size = 2) {
  i <- seq_along(artists_json)

  tidy_artists <- purrr::map_df(i, function(iter){
    tibble::tibble(
      artist     = pluck(artists_json, iter, "name", .default = NA),
      # pick first 3 genres, if available, for now
      genre1     = pluck(artists_json, iter, "genres", 1, .default = NA),
      genre2     = pluck(artists_json, iter, "genres", 2, .default = NA),
      genre3     = pluck(artists_json, iter, "genres", 3, .default = NA),
      followers  = pluck(artists_json, iter, "followers", "total", .default = NA),
      popularity = pluck(artists_json, iter, "popularity", .default = NA),
      image_url  = pluck(artists_json, iter, "images", image_size, "url", .default = NA),
      artist_id  = pluck(artists_json, iter, "id", .default = NA)
    )
  })

  # as of 24.03.2019, someting#s amiss with search strings, for example:
  # 'C.W. Stoneking' (will become 'C.w. Stoneking') via str_to_title()
  return(tidy_artists)
}
