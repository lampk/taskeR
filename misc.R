## packages need to be import
pkg <- c("lubridate", "shiny", "miniUI", "rstudioapi")
sapply(pkg, function(x) devtools::use_package(x))

## build documentation
devtools::document()
