library(usethis)

options(usethis.full_name = "Tobias Anton")

use_cc0_license()

use_package("dplyr")
use_package("httr")
use_package("purrr")
use_package("tibble")

use_roxygen_md()
# needs actual client_id and client_secret in R/OAuth.R
