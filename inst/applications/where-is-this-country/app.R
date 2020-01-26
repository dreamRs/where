
#  ------------------------------------------------------------------------
#
# Title : Where is this country ?
#    By : Victor
#  Date : 2020-01-26
#
#  ------------------------------------------------------------------------


library(shiny)
library(miniUI)
library(where)

mode <- "country"
options("where.playtime" = 60)

ui <- miniPage(
  # style sheet
  tags$link(rel="stylesheet", type="text/css", href="where/styles.css"),
  # title
  tags$div(
    class="gadget-title dreamrs-title-box",
    tags$h1(icon("map-marker"), "Where is this ", mode, "?", class = "dreamrs-title"),
    actionButton(
      inputId = "close", label = "Close",
      class = "btn-sm pull-left"
    ),
    tags$div(
      class = "pull-right",
      where:::choose_area_ui(id = "area", class = "btn-sm")
    )
  ),
  # body
  miniContentPanel(
    padding = 0,
    where:::where_ui(id = "map"),
    where:::time_ui(id = "time")
  )
)

server <- function(input, output, session) {

  area_r <- callModule(where:::choose_area_server, id = "area", launch = TRUE, mode = mode)
  time_r <- callModule(where:::time_server, id = "time", rv_area = area_r)
  if (mode == "city") {
    where_r <- callModule(where:::where_city_server, id = "map", rv_area = area_r, rv_time = time_r)
  } else {
    where_r <- callModule(where:::where_country_server, id = "map", rv_area = area_r, rv_time = time_r)
  }

  observeEvent(where_r$timestamp, {
    if (!is.null(where_r$points) && where_r$points >= 0 & time_r$time > 0) {
      showNotification(
        ui = sprintf("%s points! Total: %s", where_r$points, where_r$total),
        duration = 2,
        type = ifelse(where_r$points < 1, "error", "message")
      )
    }
  })

  observe({
    if (time_r$time < 1) {
      showModal(modalDialog(
        title = "Time elapsed!",
        fade = FALSE,
        easyClose = FALSE,
        tags$div(
          style = "text-align: center;",
          tags$br(),
          tags$h4("You played with:", isolate(area_r$area)),
          tags$br(),
          tags$h2("You scored", tags$b(isolate(where_r$total)), "points!"),
          tags$h5("(", isolate(where_r$n_played),
                  ifelse(mode == "city", "cities", "countries"), "in",
                  getOption("where.playtime"), "seconds )")
        )
      ))
    }
  })

  observeEvent(input$close, stopApp())
  onStop(stopApp)
}

shinyApp(ui = ui, server = server)
