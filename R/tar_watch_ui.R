# nocov start # Tested in tests/interactive/test-tar_watch.R
#' @title Shiny module UI for tar_watch()
#' @export
#' @family progress
#' @description Use `tar_watch_ui()` and [tar_watch_server()]
#'   to include [tar_watch()] as a Shiny module in an app.
#' @return A Shiny module UI.
#' @inheritParams tar_watch_server
#' @inheritParams tar_visnetwork
#' @param id Character of length 1, ID corresponding to the UI function
#'   of the module.
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
#' @param display Character of length 1, which display to show first.
#' @param displays Character vector of choices for the display.
#'   Elements can be any of
#'   `"graph"`, `"summary"`, `"branches"`, or `"about"`.
tar_watch_ui <- function(
  id,
  label = "tar_watch_label",
  seconds = 10,
  seconds_min = 1,
  seconds_max = 60,
  seconds_step = 1,
  targets_only = FALSE,
  outdated = FALSE,
  label_tar_visnetwork = NULL,
  level_separation = 150,
  degree_from = 1L,
  degree_to = 1L,
  height = "650px",
  display = "summary",
  displays = c("summary", "branches", "progress", "graph", "about")
) {
  tar_assert_watch_packages()
  tar_assert_dbl(seconds)
  tar_assert_dbl(seconds_min)
  tar_assert_dbl(seconds_max)
  tar_assert_dbl(seconds_step)
  tar_assert_scalar(seconds)
  tar_assert_scalar(seconds_min)
  tar_assert_scalar(seconds_max)
  tar_assert_scalar(seconds_step)
  tar_assert_scalar(degree_from)
  tar_assert_scalar(degree_to)
  tar_assert_dbl(degree_from)
  tar_assert_dbl(degree_to)
  tar_assert_ge(degree_from, 0L)
  tar_assert_ge(degree_to, 0L)
  tar_assert_in(
    displays,
    c("summary", "branches", "progress", "graph", "about")
  )
  tar_assert_in(display, displays)
  seconds_min <- min(seconds_min, seconds)
  seconds_max <- max(seconds_max, seconds)
  seconds_step <- min(seconds_step, seconds_max)
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
        choiceNames = displays,
        choiceValues = displays,
        selected = display,
        direction = "vertical"
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
        inputId = ns("label_tar_visnetwork"),
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
      ),
      shiny::numericInput(
        inputId = ns("degree_from"),
        label = "degree_from",
        value = as.numeric(degree_from),
        min = 0,
        step = 1
      ),
      shiny::numericInput(
        inputId = ns("degree_to"),
        label = "degree_to",
        value = as.numeric(degree_to),
        min = 0,
        step = 1
      )
    ),
    bs4Dash::bs4Card(
      inputID = ns("output"),
      title = "Output",
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
# nocov end
