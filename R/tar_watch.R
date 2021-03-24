# nocov start # Tested in tests/interactive/test-tar_watch.R
#' @title Shiny app to watch the dependency graph.
#' @export
#' @description Launches a background process with a Shiny app
#'   that calls [tar_visnetwork()] every few seconds.
#'   To embed this app in other apps, use the Shiny module
#'   in [tar_watch_ui()] and [tar_watch_server()].
#' @details The controls of the app are in the left panel.
#'   The `seconds` control is the number of seconds between
#'   refreshes of the graph, and the other settings match
#'   the arguments of [`tar_visnetwork()`].
#' @return A handle to `callr::r_bg()` background process running the app.
#' @inheritParams callr::r_bg
#' @inheritParams tar_watch_ui
#' @param label Label argument to [tar_visnetwork()].
#' @param background Logical, whether to run the app in a background process
#'   so you can still use the R console while the app is running.
#' @param browse Whether to open the app in a browser when the app is ready.
#'   Only relevant if `background` is `TRUE`.
#' @param host Character of length 1, IPv4 address to listen on.
#'   Only relevant if `background` is `TRUE`.
#' @param port Positive integer of length 1, TCP port to listen on.
#'   Only relevant if `background` is `TRUE`.
#' @param verbose whether to print a spinner and informative messages.
#'   Only relevant if `background` is `TRUE`.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   sleep_run <- function(...) {
#'     Sys.sleep(10)
#'   }
#'   list(
#'     tar_target(settings, sleep_run()),
#'     tar_target(data1, sleep_run(settings)),
#'     tar_target(data2, sleep_run(settings))
#'   )
#' }, ask = FALSE)
#' # Launch the app in a background process.
#' tar_watch(seconds = 10, outdated = FALSE, targets_only = TRUE)
#' # Run the pipeline.
#' tar_make()
#' })
#' }
tar_watch <- function(
  seconds = 15,
  seconds_min = 1,
  seconds_max = 60,
  seconds_step = 1,
  targets_only = FALSE,
  outdated = TRUE,
  label = NULL,
  level_separation = 150,
  height = "650px",
  background = TRUE,
  browse = TRUE,
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port", targets::tar_random_port()),
  verbose = TRUE,
  supervise = TRUE
) {
  pkgs <- c(
    "bs4Dash",
    "gt",
    "pingr",
    "shiny",
    "shinycssloaders",
    "shinyWidgets",
    "visNetwork"
  )

  assert_package(pkgs)
  assert_dbl(seconds, "seconds must be numeric.")
  assert_dbl(seconds_min, "seconds_min must be numeric.")
  assert_dbl(seconds_max, "seconds_max must be numeric.")
  assert_dbl(seconds_step, "seconds_step must be numeric.")
  assert_scalar(seconds, "seconds must have length 1.")
  assert_scalar(seconds_min, "seconds_min must have length 1.")
  assert_scalar(seconds_max, "seconds_max must have length 1.")
  assert_scalar(seconds_step, "seconds_step must have length 1.")
  seconds_min <- min(seconds_min, seconds)
  seconds_max <- max(seconds_max, seconds)
  seconds_step <- min(seconds_step, seconds_max)
  args <- list(
    seconds = seconds,
    seconds_min = seconds_min,
    seconds_max = seconds_max,
    seconds_step = seconds_step,
    targets_only = targets_only,
    outdated = outdated,
    label = label,
    level_separation = level_separation,
    height = height,
    host = host,
    port = port
  )
  if (!background) {
    return(do.call(tar_watch_app, args))
  }
  process <- callr::r_bg(
    func = tar_watch_app,
    args = args,
    stdout = "|",
    stderr = "|",
    supervise = supervise
  )
  if (browse) {
    url_port(host = host, port = port,  process = process, verbose = verbose)
  }
  if (verbose) {
    cli_port(host = host, port = port)
  }
  invisible(process)
}

tar_watch_app <- function(
  seconds,
  seconds_min,
  seconds_max,
  seconds_step,
  targets_only,
  outdated,
  label,
  level_separation,
  height,
  host,
  port
) {
  ui <- targets::tar_watch_app_ui(
    seconds = seconds,
    seconds_min = seconds_min,
    seconds_max = seconds_max,
    seconds_step = seconds_step,
    targets_only = targets_only,
    outdated = outdated,
    label = label,
    level_separation = level_separation,
    height = height
  )
  server <- function(input, output, session) {
    targets::tar_watch_server("tar_watch_id", height = height)
  }
  options <- list(host = host, port = port)
  print(shiny::shinyApp(ui = ui, server = server, options = options))
}

#' @title Create the full [tar_watch()] app UI.
#' @export
#' @keywords internal
#' @description Only exported for infrastructure purposes.
#'   Not a user-side function. Users should instead
#'   call [tar_watch()] directly.
#' @return A Shiny UI.
#' @inheritParams tar_watch_ui
#' @param label Label argument to [tar_visnetwork()].
tar_watch_app_ui <- function(
  seconds,
  seconds_min,
  seconds_max,
  seconds_step,
  targets_only,
  outdated,
  label,
  level_separation,
  height
) {
  body <- bs4Dash::bs4DashBody(
    tar_watch_ui(
      id = "tar_watch_id",
      label = "tar_watch_label",
      seconds = seconds,
      seconds_min = seconds_min,
      seconds_max = seconds_max,
      seconds_step = seconds_step,
      targets_only = targets_only,
      outdated = outdated,
      label_tar_visnetwork = label,
      level_separation = level_separation,
      height = height
    )
  )
  # TODO: update when bs4Dash 2 is on CRAN:
  trn(
    utils::packageVersion("bs4Dash") >= 2L,
    bs4Dash::bs4DashPage(
      title = "",
      body = body,
      header = bs4Dash::bs4DashNavbar(controlbarIcon = NULL),
      sidebar = bs4Dash::bs4DashSidebar(disable = TRUE),
      dark = FALSE
    ),
    bs4Dash::bs4DashPage(
      title = "",
      body = body,
      navbar = bs4Dash::bs4DashNavbar(controlbarIcon = NULL),
      sidebar = bs4Dash::bs4DashSidebar(disable = TRUE)
    )
  )
}

#' @title Shiny module UI for tar_watch()
#' @export
#' @description Use `tar_watch_ui()` and [tar_watch_server()]
#'   to include [tar_watch()] as a Shiny module in an app.
#' @return A Shiny module UI.
#' @inheritParams shiny::moduleServer
#' @inheritParams tar_watch_server
#' @inheritParams tar_visnetwork
#' @param label Label for the module.
#' @param seconds Numeric of length 1,
#'   default number of seconds between refreshes of the graph.
#'   Can be changed in the app controls.
#' @param seconds_min Numeric of length 1, lower bound of `seconds`
#'   in the app controls.
#' @param seconds_max Numeric of length 1, upper bound of `seconds`
#'   in the app controls.
#' @param seconds_step Numeric of length 1, step size of `seconds`
#'   in the app controls.
#' @param label_tar_visnetwork Character vector, `label` argument to
#'   [tar_visnetwork()].
tar_watch_ui <- function(
  id,
  label = "tar_watch_label",
  seconds = 5,
  seconds_min = 1,
  seconds_max = 60,
  seconds_step = 1,
  targets_only = FALSE,
  outdated = TRUE,
  label_tar_visnetwork = NULL,
  level_separation = 150,
  height = "650px"
) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    bs4Dash::bs4Card(
      inputID = ns("control"),
      title = "Control",
      status = "primary",
      closable = FALSE,
      collapsible = FALSE,
      width = 3,
      # TODO: update when bs4Dash 2 is on CRAN:
      solidHeader = utils::packageVersion("bs4Dash") >= 2L,
      shinyWidgets::radioGroupButtons(
        inputId = ns("display"),
        label = NULL,
        status = "primary",
        choiceNames = c("graph", "branches"),
        choiceValues = c("graph", "branches"),
        selected = "graph"
      ),
      shinyWidgets::actionBttn(
        inputId = ns("refresh"),
        label = "refresh",
        style = "simple",
        color = "primary",
        size = "sm",
        block = FALSE,
        no_outline = TRUE
      ),
      shiny::br(),
      shiny::br(),
      shinyWidgets::materialSwitch(
        inputId = ns("watch"),
        label = "watch",
        value = TRUE,
        status = "primary",
        right = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("targets_only"),
        label = "targets only",
        value = targets_only,
        status = "primary",
        right = TRUE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns("outdated"),
        label = "outdated",
        value = outdated,
        status = "primary",
        right = TRUE
      ),
      shinyWidgets::pickerInput(
        inputId = ns("label"),
        label = NULL,
        choices = c("time", "size", "branches"),
        selected = as.character(label_tar_visnetwork),
        multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          deselectAllText = "none",
          selectAllText = "all",
          noneSelectedText = "no label"
        )
      ),
      shinyWidgets::chooseSliderSkin("Flat", color = "blue"),
      shiny::sliderInput(
        inputId = ns("seconds"),
        label = "seconds",
        value = seconds,
        min = seconds_min,
        max = seconds_max,
        step = seconds_step,
        ticks = FALSE
      ),
      shiny::sliderInput(
        inputId = ns("level_separation"),
        label = "level_separation",
        value = as.numeric(level_separation),
        min = 0,
        max = 1000,
        step = 10,
        ticks = FALSE
      )
    ),
    bs4Dash::bs4Card(
      inputID = ns("progress"),
      title = "Progress",
      status = "primary",
      closable = FALSE,
      collapsible = FALSE,
      # TODO: update when bs4Dash 2 is on CRAN:
      solidHeader = utils::packageVersion("bs4Dash") >= 2L,
      width = 9,
      shiny::uiOutput(ns("display"))
    )
  )
}

#' @title Shiny module server for tar_watch()
#' @export
#' @description Use [tar_watch_ui()] and `tar_watch_server()`
#'   to include [tar_watch()] as a Shiny module in an app.
#' @return A Shiny module server.
#' @inheritParams shiny::moduleServer
#' @param height Character of length 1,
#'   height of the `visNetwork` widget and branches table.
tar_watch_server <- function(id, height = "650px") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      interval <- 200
      refresh <- shiny::reactiveValues(refresh = tempfile())
      react_millis <- shiny::reactive(1000 * as.numeric(input$seconds))
      react_targets <- shiny::reactive(as.logical(input$targets_only))
      react_outdated <- shiny::reactive(as.logical(input$outdated))
      react_label <- shiny::reactive(input$label)
      react_ls <- shiny::reactive(as.numeric(input$level_separation))
      millis <- shiny::debounce(r = react_millis, millis = interval)
      targets_only <- shiny::debounce(r = react_targets, millis = interval)
      outdated_tl <- shiny::debounce(r = react_outdated, millis = interval)
      label <- shiny::debounce(r = react_label, millis = interval)
      level_separation <- shiny::debounce(r = react_ls, millis = interval)
      shiny::observe({
        if (identical(input$watch, TRUE)) {
          shiny::invalidateLater(millis = millis())
          refresh$refresh <- tempfile()
        }
      })
      shiny::observeEvent(input$refresh, refresh$refresh <- tempfile())
      output$graph <- visNetwork::renderVisNetwork({
        shiny::req(refresh$refresh)
        trn(
          tar_exist_script(),
          tar_visnetwork(
            targets_only = targets_only(),
            outdated = outdated_tl(),
            label = label(),
            level_separation = level_separation()
          ),
          visNetwork::visNetwork(
            data_frame(
              label = "No _targets.R file detected.",
              shape = "text",
              font.size = "30"
            )
          )
        )
      })
      output$branches <- gt::render_gt({
        shiny::req(refresh$refresh)
        trn(
          tar_exist_progress(),
          tar_progress_branches_gt(),
          gt_borderless(data_frame(progress = "No progress recorded."))
        )
      }, height = height)
      output$display <- shiny::renderUI({
        switch(
          input$display %|||% "graph",
          graph = shinycssloaders::withSpinner(
            visNetwork::visNetworkOutput(session$ns("graph"), height = height)
          ),
          branches = shinycssloaders::withSpinner(
            gt::gt_output(session$ns("branches"))
          )
        )
      })
    }
  )
}
# nocov end
