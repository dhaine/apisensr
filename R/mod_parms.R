# Module UI for parameters

#' @title   Module to provide parameters
#' @description  A shiny Module to manage bias analysis parameters.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @return Called for side effects.
#'
#' @rdname mod_parms
#'
#' @keywords internal
#' @noRd
#' @importFrom shiny NS tagList
mod_parms_ui <- function(id, label_parms, value){
  ns <- NS(id)
  tagList(
      sliderInput(ns("bias_parms"), label_parms,
                  value = value, min = 0, max = 1,
                  width = "600px")
  )
}

mod_parms2_ui <- function(id, label_parms, value){
  ns <- NS(id)
  tagList(
      sliderInput(ns("bias_parms"), label_parms,
                  value = value, min = 0, max = 20, step = 0.01,
                  width = "600px")
  )
}

mod_parms2a_ui <- function(id, label_parms, value, min, max){
  ns <- NS(id)
  tagList(
      sliderInput(ns("bias_parms"), label_parms,
                  value = value, min = min, max = max, step = 0.1,
                  width = "600px")
  )
}

mod_parms3_ui <- function(id, label_parms, value) {
    ns <- NS(id)
    tagList(
        numericInput(ns("bias_parms"), label_parms,
                     value = value, min = -100, max = 100, step = 0.01,
                     width = "100%")
    )
}

mod_parms3a_ui <- function(id, label_parms, value) {
    ns <- NS(id)
    tagList(
        numericInput(ns("bias_parms"), label_parms,
                     value = value, min = 0, max = 1, step = 0.01,
                     width = "100%")
    )
}

mod_parms3b_ui <- function(id, label_parms, value) {
    ns <- NS(id)
    tagList(
        numericInput(ns("bias_parms"), label_parms,
                     value = value, min = 0, max = 20, step = 0.01,
                     width = "100%")
    )
}

mod_parmsrge_ui <- function(id, label_parms, lo, hi) {
    ns <- NS(id)
    tagList(
        sliderInput(ns("bias_parms"), label_parms,
                        min = 0, max = 1, value = c(lo, hi),
                        width = "100%")
    )
}

mod_parmsrge2_ui <- function(id, label_parms, lo, hi, step) {
    ns <- NS(id)
    tagList(
        sliderInput(ns("bias_parms"), label_parms,
                        min = 0, max = 20, value = c(lo, hi), step = step,
                        width = "100%")
    )
}

# Module Server

#' @rdname mod_parms
#' @noRd
#' @keywords internal

mod_parms_server <- function(input, output, session){
    ns <- session$ns
    input$bias_parms
}

mod_parms2_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parms2a_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parms3_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parms3a_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parms3b_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parmsrge_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parmsrge2_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

## To be copied in the UI
# mod_parms_ui("parms_ui_1")

## To be copied in the server
# callModule(mod_parms_server, "parms_ui_1")
