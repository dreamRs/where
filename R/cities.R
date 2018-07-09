
#' Get cities around the world
#'
#' @param country_name Optional, filter cities by country(ies).
#' @param continent_name Optional, filter cities by continent(s).
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table as.data.table %chin%
#'
#' @examples
#'
#' all_cities <- get_cities()
#'
#' asia_cities <- get_cities(continent_name = "Asia")
#'
get_cities <- function(country_name = NULL, continent_name = NULL) {

  cities <- as.data.table(cities1000)
  if (!is.null(continent_name)) {
    f_continent_name <- continent_name
    cities <- cities[continent_name %chin% f_continent_name]
  }

  if (!is.null(country_name)) {
    f_country_name <- country_name
    cities <- cities[country_name %chin% f_country_name]
  }

  return(cities)
}



#' Get a summary of the countries of the cities
#'
#' @param continent_name Optional, filter cities by continent(s).
#'
#' @return a \code{data.table}
#' @export
#'
#' @examples
#'
#' list_countries <- get_countries()
#'
get_countries <- function(continent_name = NULL) {
  cities <- get_cities(continent_name = continent_name)
  cities[, list(
    n = .N,
    population = sum(population, na.rm = TRUE)
  ), by = list(country_name, country_code)]
}



#' Get a summary of the continents of the cities
#'
#' @return a \code{data.table}
#' @export
#'
#' @examples
#'
#' list_continents <- get_continents()
#'
get_continents <- function() {
  cities <- get_cities()
  cities[, list(
    n = .N,
    population = sum(population, na.rm = TRUE)
  ), by = continent_name]
}



