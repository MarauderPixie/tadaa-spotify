#' Get JSON file of song features
#'
#' @param track_id a spotify id, e.g. as returned by get_songs_from_albums()
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_song_features(tidy_tracks$track_id, my_token)
get_track_features <- function(track_id, token) {
  # spotify call needs a string of max. 100 track_ids seperated by comma
  # this creates a matrix of 100 rows and as many columns as needed given
  # the number of tracks, then collapses the id-colmns into the needed strings
  n_tracks <- length(track_id)

  if (n_tracks > 100) {
    if (n_tracks %% 100 == 0) {
      id_strings <- matrix(track_id, nrow = 100) %>%
        as.data.frame() %>%
        purrr::map_chr(paste, collapse = ",")
    } else {
      matrixable_ids <- c(track_id, rep(0, 100 - (n_tracks %% 100)))

      id_strings <- matrix(matrixable_ids, nrow = 100) %>%
        as.data.frame() %>%
        purrr::map_chr(paste, collapse = ",")
    }
  } else {
    id_strings <- paste(track_id, collapse = ",")
  }

  ## call spotify
  json <- purrr::map(id_strings, function(trk_id) {
    httr::GET("https://api.spotify.com/v1/audio-features",
              httr::config(token = token),
              query = list(ids = trk_id)) %>%
      httr::content() %>%
      # enter first level of json
      purrr::pluck("audio_features")
  }) %>%
    purrr::flatten()

  return(json)
}

#' Create a tidy dataframe from song informations and features from JSON and df
#'
#' @param track_features_json a JSON object of song features, e.g. as returned by get_song_features()
#' @param tidy_tracks_from_albums a tidy dataframe of song information, e.g. as returned by tidy_songs_from_albums()
#'
#' @return dataframe
#' @importFrom purrr pluck
#' @export
#'
#' @examples
#' # tidy_track_features(track_features_json, tidy_tracks)
tidy_track_features <- function(track_features_json, tidy_tracks_from_albums) {
  i <- seq_along(track_features_json)

  raw_features <- purrr::map_df(i, function(iter){
    tibble::tibble(
      danceability = pluck(track_features_json, iter, "danceability", .default = NA),
      energy       = pluck(track_features_json, iter, "energy", .default = NA),
      key          = pluck(track_features_json, iter, "key", .default = NA),
      loudness     = pluck(track_features_json, iter, "loudness", .default = NA),
      mode         = pluck(track_features_json, iter, "mode", .default = NA),
      speechiness  = pluck(track_features_json, iter, "speechiness", .default = NA),
      acousticness = pluck(track_features_json, iter, "acousticness", .default = NA),
      instrumentalness = pluck(track_features_json, iter, "instrumentalness", .default = NA),
      liveness     = pluck(track_features_json, iter, "liveness", .default = NA),
      valence      = pluck(track_features_json, iter, "valence", .default = NA),
      tempo        = pluck(track_features_json, iter, "tempo", .default = NA),
      time_signature = pluck(track_features_json, iter, "time_signature", .default = NA),
      track_id     = pluck(track_features_json, iter, "id", .default = NA)
    )
  })

  tidy_track_features <- dplyr::left_join(tidy_tracks_from_albums, raw_features, by = "track_id")

  return(tidy_track_features)
}
