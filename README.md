# tadaa_spotify

'bout time dis gonna get serial

## Installation

```
remotes::install_github("MarauderPixie/tadaa-spotify")
```

## Current Problems

- find a way to deal with different release_date precisions in `tidy_tracks()`
- make `tidy_tracks()` have a column `date_added` when tidying from `get_user_saved()`
- `tidy_albums()` needs overhaul:
  - ~~add `track_id`-string!~~ (rather clumsy right now; 07.04.2019)
  - ~~find a way to work with objects from `get_tracks()` as well as from `get_user_saved()`~~ (could be nicer, tho, like have `get_albums()` & `get_user_saved()` return identically structured object)

## To-Do

**rather specific already:**

- add search function (as in 30S30D Project)
- ~~add `get_albums()` - provide album ids, get data~~
  - for now; merge `artists_albums` and `albums` sometime later
- add function `get_user_following()` (Artists the User follows)
- split scripts into `user.R`, `tidy.R` and... `general.R`? 
- Warning message like `There have been {n} NAs assigned to columns: ...` in all `tidy_` functions

**loose ideas:**

- `all(is.na(x))` -- Note to self: don't ever make unspecific notes pls. 
- add feedback (`cat` message?) about artists not returned by `get_artists()` (or even in general)?
- something with spotify [categories](https://developer.spotify.com/documentation/web-api/reference/browse/get-category/)?  
- ~~something something playlists...?~~


## sth sth changelog

- `0.3`: add `get_albums()` function to get albums by actual album id (before there was only `get_artists_album()`, which took only `artist_id`s)
