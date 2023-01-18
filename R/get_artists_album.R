#' Get JSON file of albums
#'
#' @param x a spotify id, e.g. as returned by get_artists()
#' @param by either "artist_id" or "album_id"; DEFUNCT FOR NOW
#' @param n number of albums to return per `artist_id` (if `n` is bigger than number available albums, all albums will be returned)
#' @param token token returned by spotifyOAuth()
#'
#' @return JSON
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # get_artists_albums(tidy_artists$album_id, n = 5, token = my_token)
get_artists_albums <- function(x, by = "artist_id", n = 20, token) {

  if (by == "artist_id"){
    album_ids <- purrr::map(x, function(art_id) {
      httr::GET(paste0("https://api.spotify.com/v1/artists/", art_id, "/albums"),
                httr::config(token = token),
                # one or several of "album, single, appears_on, compilation"
                query = list(include_groups = "album", limit = n)) %>%
        httr::content()%>%
        # enter first level of json
        purrr::pluck("items")
    }) %>%
      purrr::flatten() %>%
      purrr::map_chr(function(x) {pluck(x, "id", .default = NA)})

    expected <- n * length(x)
    remains  <- expected - (length(album_ids) %% expected)
    album_string <- c(album_ids, rep(NA, remains))

    id_mat  <- matrix(album_string, n, length(x)) %>% as.data.frame()
    to_call <- purrr::map_chr(id_mat, paste, collapse = ",")

    json <- purrr::map(to_call, function(id_strings){
      httr::GET("https://api.spotify.com/v1/albums",
                httr::config(token = token),
                # one or several of "album, single, appears_on, compilation"
                query = list(ids = id_strings)) %>%
        httr::content()
      # enter first level of json
      # purrr::pluck("items")
    }) %>%
      purrr::flatten() %>%
      purrr::flatten() %>%
      purrr::discard(is.null)
  }

  return(json)
}

#' Create a tidy dataframe of albums from JSON
#'
#' @param albums_json a JSON object of albums, e.g. as returned by get_artists_albums()
#' @param image_size 1, 2, 3 /shrug
#'
#' @return dataframe
#' @importFrom purrr pluck
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' # tidy_albums(albums_json)
tidy_albums <- function(albums_json, image_size = 2) {
  i <- seq_along(albums_json)

  if (length(albums_json[[1]]) == 2){
    # this is a mess, I'm rly sorry /o\
    total_tracks <- purrr::map_int(i, function(x){
      pluck(albums_json, x, "album", "total_tracks", .default = NA)
    })

    j    <- purrr::map(total_tracks, seq_len) %>% purrr::as_vector()
    hm   <- rep(i, total_tracks)
    long <- purrr::map2_chr(hm, j, function(x, y){
      pluck(albums_json, x, "album", "tracks", "items", y, "id", .default = NA)
    })
    tids <- split(long, rep(i, total_tracks)) %>% purrr::map_chr(paste, collapse = ",")

    # murks. since the json looks different, having two blocks basically
    # doing the same is the only solution I got for now...
    tidy_albums <- purrr::map_df(i, function(iter){
      tibble::tibble(
        artist    = pluck(albums_json, iter, "album", "artists", 1, "name", .default = NA),
        album     = pluck(albums_json, iter, "album", "name", .default = NA),
        label     = pluck(albums_json, iter, "album", "label", .default = NA),
        release_date = pluck(albums_json, iter, "album", "release_date", .default = NA),
        date_added   = pluck(albums_json, iter, "added_at", .default = NA),
        popularity   = pluck(albums_json, iter, "album", "popularity", .default = NA),
        image_url = pluck(albums_json, iter, "album", "images", image_size, "url", .default = NA),
        artist_id = pluck(albums_json, iter, "album", "artists", 1, "id", .default = NA),
        album_id  = pluck(albums_json, iter, "album", "id", .default = NA),
        track_ids = tids[iter]
      )
    })
  } else {
    # yes, still horrifying /o\
    total_tracks <- purrr::map_int(i, function(x){
      pluck(albums_json, x, "total_tracks", .default = NA)
    })

    j    <- purrr::map(total_tracks, seq_len) %>% purrr::as_vector()
    hm   <- rep(i, total_tracks)
    long <- purrr::map2_chr(hm, j, function(x, y){
      pluck(albums_json, x, "tracks", "items", y, "id", .default = NA)
    })
    tids <- split(long, rep(i, total_tracks)) %>% purrr::map_chr(paste, collapse = ",")

    tidy_albums <- purrr::map_df(i, function(iter){
      tibble::tibble(
        artist    = pluck(albums_json, iter, "artists", 1, "name", .default = NA),
        album     = pluck(albums_json, iter, "name", .default = NA),
        label     = pluck(albums_json, iter, "label", .default = NA),
        release_date = pluck(albums_json, iter, "release_date", .default = NA),
        popularity   = pluck(albums_json, iter, "popularity", .default = NA),
        image_url = pluck(albums_json, iter, "images", image_size, "url", .default = NA),
        artist_id = pluck(albums_json, iter, "artists", 1, "id", .default = NA),
        album_id  = pluck(albums_json, iter, "id", .default = NA),
        track_ids = tids[iter]
      )
    })
  }

  return(tidy_albums)
}
