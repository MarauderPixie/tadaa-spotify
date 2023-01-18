###### pipeline
# get_*() %>% tidy_*() also works ^_^
token <- spotifyOAuth("")

## starting by search for artsts
test_artists <- c("Uncle Acid", "Solstafir", "Necro Deathmort",
                  "Le Butcherettes", "Jon Everist", "Sleaford Mods", "Melvins")

artists <- get_artists(test_artists, token = token) %>%
  tidy_artists()

albums <- get_artists_albums(artists$artist_id, token = token) %>%
  tidy_albums()

songs <- get_tracks(albums$track_ids, token = token) %>%
  tidy_tracks()

features <- get_track_features(songs$track_id, token = token) %>%
  tidy_track_features(songs)

## starting with toplists
top_tracks  <- get_user_toplist(type = "tracks", token = token) %>% tidy_tracks()
top_artists <- get_user_toplist(token = token) %>% tidy_artists()

## starting with saved objects
savd_tracks <- get_user_saved(type = "tracks", n = 1000, token = token) %>% tidy_tracks()
savd_albums <- get_user_saved(n = 25, token = token) %>% tidy_albums()

# inspect results a little
songs %>%
  count(artist) %>%
  ggplot(aes(reorder(artist, n), n)) +
    geom_col() +
    coord_flip()

songs %>%
  gather(feature, value, 5, 8:19) %>%
  group_by(artist_id, feature) %>%
  mutate(value_norm = (value - mean(value)) / sd(value)) %>%
  ggplot(aes(x = artist, y = value_norm)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~feature, scales = "free")
