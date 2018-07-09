

#' @importFrom shiny NS absolutePanel
#' @importFrom shinyWidgets progressBar
time_ui <- function(id) {
  ns <- NS(id)
  playtime <- getOption("where.playtime", default = 10)
  absolutePanel(
    top = 5, right = 5,
    style = "background-color: white; padding: 5px; border-radius: 10px;",
    width = "30%",
    progressBar(
      id = ns("time_progress"),
      title = "Remaining time:",
      value = playtime,
      total = playtime,
      status = "primary"
    )
  )
}

#' @importFrom shiny reactiveValues observeEvent observe invalidateLater isolate
#' @importFrom shinyWidgets updateProgressBar
time_server <- function(input, output, session, rv_area) {

  ns <- session$ns

  playtime <- getOption("where.playtime", default = 10)

  timer_r <- reactiveValues(time = playtime)
  observeEvent(rv_area$timestamp, {
    timer_r$time <- playtime + 1
    updateProgressBar(
      session = session,
      id = ns("time_progress"),
      value = playtime,
      total = playtime
    )
  })

  observe({
    if (isTRUE(rv_area$play)) {
      invalidateLater(1000)
      timer_r$time <- isolate(timer_r$time) - 1
      updateProgressBar(
        session = session,
        id = ns("time_progress"),
        value = timer_r$time,
        total = playtime
      )
    }
  })

  observeEvent(timer_r$time, {
    if (timer_r$time < 1) {
      rv_area$play <- FALSE
    }
  })

  return(timer_r)
}



