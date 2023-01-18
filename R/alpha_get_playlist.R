#' Get JSON file of a playlist's tracks
#'
#' @param playlist_id a spotify id, e.g. as returned by get_songs_from_albums()
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_song_features(tidy_tracks$track_id, my_token)
get_playlist <- function(playlist_id, token) {
  url <- paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks")
  # call spotify
  json <- httr::GET(url, httr::config(token = token)) %>%
      httr::content() %>%
      # enter first level of json
      # purrr::pluck("artists")
    purrr::flatten()

  return(json)
}


#' Create a tidy dataframe from song informations and features from JSON and df
#'
#' @param playlist_json a JSON object of playlist tracks, e.g. as returned by get_playlist()
#'
#' @return dataframe
#' @importFrom purrr pluck
#' @export
#'
#' @examples
#' # tidy_track_features(playlist_json, tidy_tracks)
tidy_playlist <- function(playlist_json) {
  i <- seq_along(playlist_json)

  tidy_playlist <- purrr::map_df(i, function(iter){
    tibble::tibble(
      artist = pluck(playlist_json, iter, "track", "artists", 1, "name", .default = NA),
      track  = pluck(playlist_json, iter, "track", "name", .default = NA),
      album  = pluck(playlist_json, iter, "track", "album", "name", .default = NA),
      dur_ms = pluck(playlist_json, iter, "track", "duration_ms", .default = NA),
      popularity = pluck(playlist_json, iter, "track", "duration_ms", .default = NA),
      explicit   = pluck(playlist_json, iter, "track", "explicit", .default = NA),
      added      = pluck(playlist_json, iter, "added_at", .default = NA),
      artist_id  = pluck(playlist_json, iter, "track", "artists", 1, "id", .default = NA),
      album_id   = pluck(playlist_json, iter, "track", "album", "id", .default = NA),
      track_id   = pluck(playlist_json, iter, "track", "id", .default = NA)
    )
  }) %>%
    filter(!is.na(track))

  return(tidy_playlist)
}
