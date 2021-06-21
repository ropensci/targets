# nocov start # Tested in tests/interactive/test-tar_watch.R
#' @title Shiny app to watch the dependency graph.
#' @export
#' @family progress
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
#' @inheritParams tar_watch_server
#' @param exclude Character vector of nodes to omit from the graph.
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
  seconds = 10,
  seconds_min = 1,
  seconds_max = 60,
  seconds_step = 1,
  targets_only = FALSE,
  exclude = ".Random.seed",
  outdated = TRUE,
  label = NULL,
  level_separation = 150,
  degree_from = 1L,
  degree_to = 1L,
  config = "_targets.yaml",
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
    "markdown",
    "pingr",
    "shiny",
    "shinybusy",
    "shinyWidgets",
    "visNetwork"
  )
  assert_package(pkgs)
  assert_chr(exclude)
  assert_dbl(seconds)
  assert_dbl(seconds_min)
  assert_dbl(seconds_max)
  assert_dbl(seconds_step)
  assert_scalar(seconds)
  assert_scalar(seconds_min)
  assert_scalar(seconds_max)
  assert_scalar(seconds_step)
  assert_scalar(degree_from)
  assert_scalar(degree_to)
  assert_dbl(degree_from)
  assert_dbl(degree_to)
  assert_ge(degree_from, 0L)
  assert_ge(degree_to, 0L)
  seconds_min <- min(seconds_min, seconds)
  seconds_max <- max(seconds_max, seconds)
  seconds_step <- min(seconds_step, seconds_max)
  args <- list(
    seconds = seconds,
    seconds_min = seconds_min,
    seconds_max = seconds_max,
    seconds_step = seconds_step,
    targets_only = targets_only,
    exclude = exclude,
    outdated = outdated,
    label = label,
    level_separation = level_separation,
    degree_from = degree_from,
    degree_to = degree_to,
    config = config,
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
  exclude,
  outdated,
  label,
  level_separation,
  degree_from,
  degree_to,
  config,
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
    degree_from = degree_from,
    degree_to = degree_to,
    height = height
  )
  server <- function(input, output, session) {
    targets::tar_watch_server(
      id = "tar_watch_id",
      height = height,
      exclude = exclude,
      config = config
    )
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
  degree_from,
  degree_to,
  height
) {
  body <- bs4Dash::bs4DashBody(
    shinybusy::add_busy_spinner(position = "top-left"),
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
      degree_from = degree_from,
      degree_to = degree_to,
      height = height
    )
  )
  # TODO: update when bs4Dash 2 is on CRAN:
  if_any(
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
# nocov end
