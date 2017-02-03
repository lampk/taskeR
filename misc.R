## packages need to be import
pkg <- c("lubridate", "shiny", "miniUI", "rstudioapi")
sapply(pkg, function(x) devtools::use_package(x))

## build documentation
devtools::document()

## git set up
# git remote add origin https://github.com/lampk/taskeR.git
# git config remote.origin.url git@github.com:lampk/taskeR.git
# git pull origin master
# git push origin master
