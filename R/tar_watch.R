# nocov start # Tested in tests/interactive/test-tar_watch.R
#' @title Shiny app to watch the dependency graph.
#' @export
#' @description Calls [tar_visnetwork()] and offers a GUI
#'   to select settings.
#' @examples
#' if (FALSE) { # Only run interactively.
#' tar_dir({
#'   tar_script()
#'   tar_watch()
#' })
#' }
tar_watch <- function() {
  assert_package("bs4Dash")
  assert_package("shiny")
  assert_package("shinycssloaders")
  assert_package("visNetwork")
  assert_target_script()
  shiny::shinyApp(tar_watch_app_ui(), tar_watch_app_server)
}

tar_watch_app_ui <- function() {
  bs4Dash::bs4DashPage(
    title = "",
    body = bs4Dash::bs4DashBody(tar_watch_ui("tar_watch_id")),
    navbar = bs4Dash::bs4DashNavbar(controlbarIcon = NULL),
    sidebar = bs4Dash::bs4DashSidebar(disable = TRUE)
  )
}

tar_watch_app_server <- function(input, output, session) {
  tar_watch_server("tar_watch_id")
}

#' @title Shiny module UI for tar_watch()
#' @export
#' @description Use `tar_watch_ui()` and [tar_watch_server()]
#'   to include [tar_watch()] as a Shiny module in an app.
#' @examples
#' str(tar_watch_ui("my_id"))
#' @inheritParams shiny::moduleServer
#' @param label Label for the module. 
tar_watch_ui <- function(id, label = "tar_watch") {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 4,
      bs4Dash::bs4Card(
        inputID = ns("control"),
        title = "Control",
        status = "primary",
        closable = FALSE,
        width = 12,
        shiny::textInput(
          ns("seconds"),
          "seconds",
          value = "5"
        ),
        shiny::selectInput(
          ns("targets_only"),
          "targets_only",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        shiny::selectInput(
          ns("outdated"),
          "outdated",
          choices = c("TRUE", "FALSE"),
          selected = "TRUE"
        ),
        shiny::selectInput(
          ns("label"),
          "label",
          choices = c("time", "size", "branches"),
          selected = character(0),
          multiple = TRUE
        ),
        shiny::sliderInput(
          ns("level_separation"),
          "level_separation",
          value = 150,
          min = 0,
          max = 1000,
          step = 10
        )
      )
    ),
    shiny::column(
      width = 8,
      bs4Dash::bs4Card(
        inputID = ns("graph"),
        title = "Graph",
        status = "primary",
        closable = FALSE,
        width = 12,
        shinycssloaders::withSpinner(
          visNetwork::visNetworkOutput(ns("graph"))
        )
      )
    )
  )
}

#' @title Shiny module server for tar_watch()
#' @export
#' @description Use [tar_watch_ui()] and `tar_watch_server()`
#'   to include [tar_watch()] as a Shiny module in an app.
#' @inheritParams shiny::moduleServer
#' @param height Character of length 1,
#'   height of the `visNetwork` widget.
#' @examples
#' # tar_watch_server("my_id") # Only call inside an app.
tar_watch_server <- function(id, height = "600px") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$graph <- visNetwork::renderVisNetwork({
        shiny::invalidateLater(millis = 1000 * as.numeric(input$seconds))
        tar_visnetwork(
          targets_only = as.logical(input$targets_only),
          outdated = as.logical(input$outdated),
          label = as.character(input$label),
          level_separation = as.numeric(input$level_separation)
        )
      })
    }
  )
}
# nocov end
