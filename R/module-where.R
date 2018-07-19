


#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS tagList tags absolutePanel uiOutput
where_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(outputId = ns("map"), width = "100%", height = "100%"),
    tags$style(".leaflet {cursor: crosshair;}"),
    absolutePanel(
      top = 5, left = 5, width = "35%",
      uiOutput(outputId = ns("searched_area"))
    )
  )
}

#' @importFrom leaflet renderLeaflet leaflet leafletOptions addProviderTiles
#'  leafletProxy fitBounds addMarkers clearMarkers %>% labelOptions
#' @importFrom shiny reactiveValues reactiveVal observeEvent req renderUI tags observe
#' @importFrom htmltools htmlEscape
where_city_server <- function(input, output, session, rv_area, rv_time) {

  ns <- session$ns

  dat_r <- reactiveValues(lng = NULL, lat = NULL, points = -1, total = 0, timestamp = Sys.time())

  count <- reactiveVal(1)

  cities_r <- reactiveValues(data = NULL, names = NULL, n_played = NULL)
  observeEvent(rv_area$timestamp, {
    dat_r$total <- 0
    count(1)
    req(rv_area$area)
    if (rv_area$area %in% get_continents()[[1]]) {
      cities <- get_cities(continent_name = rv_area$area)
    } else {
      cities <- get_cities(country_name = rv_area$area)
    }
    cities <- cities[order(population, decreasing = TRUE)]
    cities <- cities[seq_len(min(c(150, nrow(cities))))]
    cities_r$data <- cities
    cities_r$names <- sample(cities$name, size = 150, replace = nrow(cities) < 150)
  })

  output$searched_area <- renderUI({
    if (!is.null(cities_r$names)) {
      i <- count()
      tags$div(
        style = "background-color: white; border-radius: 10px; padding: 10px;",
        "Where is", tags$b(cities_r$names[i], style = "font-size: 140%;"), "?"
      )
    }
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = FALSE,
      maxZoom = 9,
      doubleClickZoom = FALSE
    )) %>%
      addProviderTiles(provider = "Stamen.TerrainBackground")
  })


  observeEvent(cities_r$data, {
    bounds <- cities_r$data[, list(
      lat_min = min(latitude, na.rm = TRUE),
      lat_max = max(latitude, na.rm = TRUE),
      lon_min = min(longitude, na.rm = TRUE),
      lon_max = max(longitude, na.rm = TRUE)
    )]
    leafletProxy(mapId = "map") %>%
      fitBounds(
        lng1 = bounds$lon_min,
        lat1 = bounds$lat_min,
        lng2 = bounds$lon_max,
        lat2 = bounds$lat_max
      )
  }, ignoreNULL = TRUE)


  observeEvent(input$map_click, {
    if (rv_time$time > 0) {
      # indice city to find
      newCount <- count() + 1
      count(newCount)
      # point count
      points <- count_city_points(
        data = cities_r$data,
        name_city = cities_r$names[newCount - 1],
        coords = c(input$map_click$lng, input$map_click$lat)
      )
      dat_r$points <- points
      dat_r$total <- dat_r$total + points
      dat_r$timestamp <- Sys.time()
      # update map
      dat_r$lng <- input$map_click$lng
      dat_r$lat <- input$map_click$lat
    }
    # leafletProxy(mapId = "map") %>%
    #   removeMarker(layerId = "points") %>%
    #   addMarkers(lng = input$map_click$lng, lat = input$map_click$lat, layerId = "points")
  })

  observe({
    if (rv_time$time < 1) {
      i <- count()
      dat_r$n_played <- i
      cities_played <- cities_r$data
      cities_names <- cities_r$names[seq_len(i)]
      cities_played <- cities_played[name %in% cities_names]
      leafletProxy(mapId = "map") %>%
        addMarkers(
          data = cities_played,
          lng = ~longitude,
          lat = ~latitude,
          label = ~htmlEscape(asciiname),
          labelOptions = labelOptions(textsize = "12px", sticky = FALSE)
        )
    } else {
      leafletProxy(mapId = "map") %>%
        clearMarkers()
    }
  })

  return(dat_r)
}


#' @importFrom geosphere distGeo
count_city_points <- function(data, name_city, coords) {
  data <- data[name %in% name_city]
  distance <- distGeo(p1 = data[, c(longitude, latitude)], p2 = coords)
  distance <- distance / 1000
  return(floor(100/distance))
}






#' @importFrom sf st_as_sf
#' @importFrom leaflet addPolygons clearShapes
#' @importFrom rnaturalearth ne_countries
where_country_server <- function(input, output, session, rv_area, rv_time) {

  ns <- session$ns

  dat_r <- reactiveValues(lng = NULL, lat = NULL, points = -1, total = 0, timestamp = Sys.time())

  count <- reactiveVal(1)

  countries_r <- reactiveValues(data = NULL, names = NULL, n_played = 0)
  observeEvent(rv_area$timestamp, {
    dat_r$total <- 0
    dat_r$n_played <- 0
    count(1)
    req(rv_area$area)
    if (rv_area$area == "World") {
      countries <- ne_countries(scale = 110, returnclass = "sf")
    } else {
      countries <- ne_countries(scale = 110, returnclass = "sf", continent = rv_area$area)
    }
    alea <- sample(seq_len(nrow(countries)))
    countries_r$data <- countries
    countries_r$names <- countries$name[alea]
    countries_r$code_flag <- tolower(countries$iso_a2[alea])
  })

  output$searched_area <- renderUI({
    if (!is.null(countries_r$names)) {
      i <- count()
      tags$div(
        style = "background-color: white; border-radius: 10px; padding: 10px;",
        "Where is",
        tags$b(countries_r$names[i], style = "font-size: 140%;"),
        "?",
        tags$span(
          class = sprintf("flag-icon flag-icon-%s", countries_r$code_flag[i]),
          style = "font-size: 180%;"
        )
      )
    }
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = FALSE,
      maxZoom = 9,
      doubleClickZoom = FALSE
    )) %>%
      addProviderTiles(provider = "Stamen.TerrainBackground")
  })


  observeEvent(countries_r$data, {
    bounds <- as.list(sf::st_bbox(countries_r$data))
    leafletProxy(mapId = "map") %>%
      fitBounds(
        lng1 = bounds$xmin,
        lat1 = bounds$ymin,
        lng2 = bounds$xmax,
        lat2 = bounds$ymax
      )
  }, ignoreNULL = TRUE)


  observeEvent(input$map_click, {
    if (rv_time$time > 0) {
      dat_r$n_played <- dat_r$n_played + 1
      # indice city to find
      newCount <- count() + 1
      if (newCount >= length(countries_r$names)) {
        count(1)
      } else {
        count(newCount)
      }
      # point count
      points <- count_country_points(
        data = countries_r$data,
        name_country = countries_r$names[newCount - 1],
        coords = c(input$map_click$lng, input$map_click$lat)
      )
      dat_r$points <- points
      dat_r$total <- dat_r$total + points
      dat_r$timestamp <- Sys.time()
      # update map
      dat_r$lng <- input$map_click$lng
      dat_r$lat <- input$map_click$lat
    }
    # leafletProxy(mapId = "map") %>%
    #   removeMarker(layerId = "points") %>%
    #   addMarkers(lng = input$map_click$lng, lat = input$map_click$lat, layerId = "points")
  })

  observe({
    if (rv_time$time < 1) {
      i <- count()
      countries_played <- countries_r$data
      countries_names <- countries_r$names[seq_len(i)]
      countries_played <- countries_played[countries_played$name %in% countries_names, ]
      leafletProxy(mapId = "map") %>%
        addPolygons(
          data = sf::st_as_sf(countries_played),
          label = ~htmlEscape(name),
          labelOptions = labelOptions(textsize = "12px", sticky = TRUE)
        )
    } else {
      leafletProxy(mapId = "map") %>%
        clearShapes()
    }
  })

  return(dat_r)
}


#' @importFrom sf st_within st_sfc st_point
count_country_points <- function(data, name_country, coords) {
  any(
    suppressMessages(sf::st_within(
      x = sf::st_sfc(sf::st_point(coords), crs = 4326),
      y = data[data$name == name_country, ],
      sparse = FALSE
    ))
  ) * 10
}

