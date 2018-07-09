
#' @title Where is this City?
#'
#' @description An addin to test your geographical skills !
#'
#' @param time Playing time, default to 50 seconds.
#'
#' @export
#'
#' @importFrom miniUI miniPage miniContentPanel
#' @importFrom shiny icon actionButton callModule observeEvent
#'  showNotification observe showModal modalDialog tags isolate
#'  stopApp dialogViewer browserViewer runGadget
#'
#' @examples
#' \dontrun{
#'
#' where()
#'
#' }
where <- function(time = getOption("where.playtime", default = 60)) {

  options("where.playtime" = time)

  ui <- miniPage(
    # style sheet
    tags$link(rel="stylesheet", type="text/css", href="where/styles.css"),
    # title
    tags$div(
      class="gadget-title dreamrs-title-box",
      tags$h1(icon("map-marker"), "Where is this City?", class = "dreamrs-title"),
      actionButton(
        inputId = "close", label = "Close",
        class = "btn-sm pull-left"
      ),
      tags$div(
        class = "pull-right",
        choose_area_ui(id = "area", class = "btn-sm")
      )
    ),
    # body
    miniContentPanel(
      padding = 0,
      where_ui(id = "map"),
      time_ui(id = "time")
    )
  )

  server <- function(input, output, session) {

    area_r <- callModule(choose_area_server, id = "area", launch = TRUE)
    time_r <- callModule(time_server, id = "time", rv_area = area_r)
    where_r <- callModule(where_server, id = "map", rv_area = area_r, rv_time = time_r)

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
            tags$h2("You scored", tags$b(isolate(where_r$total)), "points!")
          )
        ))
      }
    })

    observeEvent(input$close, stopApp())
  }

  viewer_opts <- getOption(x = "where.viewer", default = "dialog")
  if (viewer_opts == "dialog") {
    viewer <- dialogViewer(
      "Test your geography skills!",
      width = 1000, height = 750
    )
  } else {
    viewer <- browserViewer()
  }
  runGadget(app = ui, server = server, viewer = viewer)
}



