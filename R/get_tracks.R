#' Get JSON file of an album's songs
#'
#' @param track_ids a spotify id, e.g. as returned by get_artists_album()
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @import progress
#' @export
#'
#' @examples
#' # get_songs_from_albums(tidy_albums$album_id, my_token)
get_tracks <- function(track_ids, token){
  ##############
  # ACTUALLY....
  # I might need to attach a list of track_id strings to the tidy_albums df
  # and then make this a "get_songs" function with param "by = album / search"
  # or sth like that.
  
  if (length(track_ids) > 50) {
    cliapp::cli_h2(paste("fetching", length(track_ids), "items -- this may take a few seconds\n"))
  }
  
  # initialize progress bar
  pb <- progress::progress_bar$new(
    total = length(track_ids),
    format = "fetching songs [:bar] :current/:total (:percent)"
  )
  
  json <- purrr::map(track_ids, function(trk_ids) {
    pb$tick(1)
    httr::GET("https://api.spotify.com/v1/tracks",
              httr::config(token = token),
              query = list(ids = trk_ids)) %>%
      httr::content() %>%
      # enter first level of json
      purrr::pluck("tracks")
  }) %>%
    purrr::flatten()
  
  return(json)
}

#' Create a tidy dataframe songs from JSON
#'
#' @param tracks_json a JSON object of songs, e.g. as returned by get_songs_from_albums()
#'
#' @return dataframe
#' @importFrom purrr pluck
#' @import progress
#' @export
#'
#' @examples
#' # tidy_songs_from_albums(songs_json)
tidy_tracks <- function(tracks_json) {
  i <- seq_along(tracks_json)
  
  if (length(i) > 500){
    pb <- progress::progress_bar$new(
      total = length(i),
      format = "tidying... [:bar] :current/:total (:percent)"
    )
  }
  
  # check if json from is length 2 
  # user_saved_tracks is (it has a timestamp for when it was added)
  if (length(tracks_json[[1]]) == 2){
    tidy_tracks <- purrr::map_df(i, function(iter){
      if (length(i) > 500){pb$tick(1)}
      tibble::tibble(
        artist     = pluck(tracks_json, iter, "track", "artists", 1, "name", .default = NA),
        track      = pluck(tracks_json, iter, "track", "name", .default = NA),
        album      = pluck(tracks_json, iter, "track", "album", "name", .default = NA),
        released   = pluck(tracks_json, iter, "track", "album", "release_date", .default = NA),
        dur_ms     = pluck(tracks_json, iter, "track", "duration_ms", .default = NA),
        track_no   = pluck(tracks_json, iter, "track", "track_number", .default = NA),
        explicit   = pluck(tracks_json, iter, "track", "explicit", .default = NA),
        popularity = pluck(tracks_json, iter, "track", "popularity", .default = NA),
        artist_id  = pluck(tracks_json, iter, "track", "artists", 1, "id", .default = NA),
        track_id   = pluck(tracks_json, iter, "track", "id", .default = NA),
        album_id   = pluck(tracks_json, iter, "track", "album", "id", .default = NA)
      )
    })
  } else {
    tidy_tracks <- purrr::map_df(i, function(iter){
      if (length(i) > 500){pb$tick(1)}
      tibble::tibble(
        artist     = pluck(tracks_json, iter, "artists", 1, "name", .default = NA),
        track      = pluck(tracks_json, iter, "name", .default = NA),
        album      = pluck(tracks_json, iter, "album", "name", .default = NA),
        released   = pluck(tracks_json, iter, "album", "release_date", .default = NA),
        dur_ms     = pluck(tracks_json, iter, "duration_ms", .default = NA),
        track_no   = pluck(tracks_json, iter, "track_number", .default = NA),
        explicit   = pluck(tracks_json, iter, "explicit", .default = NA),
        popularity = pluck(tracks_json, iter, "popularity", .default = NA),
        artist_id  = pluck(tracks_json, iter, "artists", 1, "id", .default = NA),
        track_id   = pluck(tracks_json, iter, "id", .default = NA),
        album_id   = pluck(tracks_json, iter, "album", "id", .default = NA)
      )
    })
  }
  
  return(tidy_tracks)
}
