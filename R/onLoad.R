#' Adds the content of www to where/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("where", system.file('www', package = "where"))
}
