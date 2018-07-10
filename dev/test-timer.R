


library(shiny)
library(shinyWidgets)

source("../R/module-choose-area.R")

ui <- fluidPage(
  choose_area_ui("area"),
  uiOutput(outputId = "timer")
)

server <- function(input, output, session) {

  area_r <- callModule(choose_area_server, id = "area", launch = TRUE)

  playtime <- getOption("where.playtime", default = 60)

  timer_r <- reactiveValues(time = playtime)
  observeEvent(area_r$timestamp, {
    timer_r$time <- playtime
  })

  observe({
    if (isTRUE(area_r$play)) {
      invalidateLater(1000)
      timer_r$time <- isolate(timer_r$time) - 1
    }
  })

  observeEvent(timer_r$time, {
    if (timer_r$time < 1) {
      area_r$play <- FALSE
    }
  })

  output$timer <- renderUI({
    if (timer_r$time >= 1) {
      tags$div(
        "Remaining time:", tags$b(timer_r$time)
      )
    } else {
      tags$div(
        tags$b("Finish!", style = "color: red;")
      )
    }
  })

}

shinyApp(ui, server)

