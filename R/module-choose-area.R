



#' @importFrom shiny NS tagList singleton tags actionButton
choose_area_ui <- function(id, ...) {
  ns <- NS(id)
  tagList(
    singleton(
      tags$link(
        rel="stylesheet",
        type="text/css",
        href="where/flags/css/flag-icon.min.css"
      )
    ),
    actionButton(
      inputId = ns("choose_area"),
      label = "Choose an area", ...
    )
  )
}



#' @importFrom shiny reactiveValues showModal observeEvent updateSelectizeInput removeUI insertUI
choose_area_server <- function(input, output, session, launch = TRUE, mode = c("city", "country")) {

  mode <- match.arg(mode)

  ns <- session$ns

  dat_r <- reactiveValues(area = NULL, play = FALSE, timestamp = Sys.time())

  if (launch) {
    showModal(choose_area_modal(ns, mode = mode))
  }

  observeEvent(input$choose_area, {
    dat_r$play <- FALSE
    dat_r$timestamp <- Sys.time()
    showModal(choose_area_modal(ns, mode = mode))
  })

  # observeEvent(input$area, {
  #   removeUI(selector = paste0("#", ns("select-sub-area")), immediate = TRUE)
  #   area <- input$area
  #   if (!area %in% make_continents_list() & mode == "city") {
  #     area <- get_country_name(area)
  #     counties <- get_cities(country_name = area)
  #     insertUI(
  #       selector = paste0("#", ns("placeholder-sub-area")),
  #       ui = tags$div(
  #         id = ns("select-sub-area"),
  #         selectizeInput(
  #           inputId = ns("sub_area"),
  #           label = "Choose a sub-level (optional) :",
  #           choices = c("None", sort(unique(counties$county_name))),
  #           selected = "None",
  #           multiple = FALSE,
  #           width = "100%"
  #         )
  #       )
  #     )
  #   }
  # }, ignoreNULL = TRUE, ignoreInit = FALSE)

  observeEvent(input$validate_area, {
    area <- input$area
    if (!area %in% make_continents_list() & mode == "city") {
      area <- get_country_name(area)
    }
    dat_r$area <- area
    dat_r$sub_area <- input$sub_area
    dat_r$play <- TRUE
    dat_r$timestamp <- Sys.time()
  })

  return(dat_r)
}




#' @importFrom shiny modalDialog selectizeInput actionButton
choose_area_modal <- function(ns, mode = c("city", "country")) {
  mode <- match.arg(mode)
  if (mode == "city") {
    select_tag <- tagList(
      selectizeInput(
        inputId = ns("area"), label = "Select an area:",
        multiple = FALSE, width = "100%",
        # choices = NULL,
        choices = list(
          Continent = make_continents_list(),
          Country = make_countries_list()
        ),
        selected = getOption("where.code"),
        options = list(
          render = I(paste(
            "{",
            "option: function(item, escape) {",
            "return '<div><span class=\"flag-icon flag-icon-' + item.value + '\"></span>' + ' ' + escape(item.label) + '</div>'",
            "}",
            "}",
            collapse = "\n"
          ))
        )
      ),
      tags$div(
        id = ns("placeholder-sub-area")
      )
    )
  } else {
    select_tag <- selectizeInput(
      inputId = ns("area"), label = "Select an area:",
      multiple = FALSE, width = "100%",
      choices = list(
        "World" = list("World"),
        "Continents" = list("Africa", "Antarctica", "Asia",
                         "Europe", "North America",
                         "Oceania", "South America")
      ),
      selected = getOption("where.code")
    )
  }
  modalDialog(
    title = "Choose an area to play !",
    fade = FALSE,
    easyClose = FALSE,
    select_tag,
    footer = actionButton(
      inputId = ns("validate_area"), label = "Play!",
      class = "btn-primary", `data-dismiss` = "modal"
    )
  )
}




make_continents_list <- function() {
  cont <- sort(get_continents()[[1]])
  return(cont)
}

make_countries_list <- function() {
  countries <- get_countries()
  countries_names <- unique(countries$country_name)
  countries_codes <- tolower(unique(countries$country_code))
  countries_list <- as.list(countries_codes)
  names(countries_list) <- countries_names
  return(countries_list)
}


get_country_name <- function(code) {
  code <- toupper(code)
  countries <- get_countries()
  countries[country_code == code, c(country_name)]
}

get_country_code <- function(name) {
  countries <- get_countries()
  countries[country_name == name, c(country_code)]
}


