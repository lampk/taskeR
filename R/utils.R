#' To pick default ggplot colors
ggplot_color <- function(n){
  ## thanks to John Colby (http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette)
  hcl(h = seq(15, 375, length = n + 1), l = 65, c = 100)[1:n]
}

#' A modify version of file.choose
file.choose2 <- function(...) {
  pathname <- NULL
  tryCatch({pathname <- file.choose()}, error = function(ex) {})
  pathname
}

#' To copy snippets to computers
#'
#' @export
copy_snippets <- function(from = file.path(system.file(package = "taskeR"), "snippets"),
                          to = file.path(Sys.getenv("HOME"), ".R", "snippets")) {
  tmp <- list.files(from)
  for (i in (1:length(tmp))) {
    file.copy(from = file.path(path_from, tmp[i]),
              to = to,
              overwrite = TRUE)
  }
}
