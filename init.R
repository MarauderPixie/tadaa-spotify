library(tidyverse)
# library(jsonlite)
library(lubridate)
library(httr)
# library(broom)
library(tadaaspotify)

theme_set(hrbrthemes::theme_modern_rc())
theme_update(strip.text = element_text(color = "white"))

token <- spotifyOAuth("")
