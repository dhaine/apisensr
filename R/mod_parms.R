# Module UI
  
#' @title   Module to provide simple bias parameters
#' @description  A shiny Module to treat simple Se/Sp and prevalence bias analysis
#' parameters (between 0 and 1), risk (from 0 to 20).
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_parms
#'
#' @keywords internal
#' @export 
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

mod_parms3_ui <- function(id, label_parms, value) {
    ns <- NS(id)
    tagList(
        material_number_box(ns("bias_parms"), label_parms,
                            min_value = -100, max_value = 100,
                            step_size = 0.01, initial_value = value,
                            color = "#0277bd")
    )
}

# Module Server
    
#' @rdname mod_parms
#' @export
#' @keywords internal
    
mod_parms_server <- function(input, output, session){
    ns <- session$ns
    input$bias_parms
}

mod_parms2_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}

mod_parms3_server <- function(input, output, session) {
    ns <- session$ns
    input$bias_parms
}
    
## To be copied in the UI
# mod_parms_ui("parms_ui_1")
    
## To be copied in the server
# callModule(mod_parms_server, "parms_ui_1")
 
