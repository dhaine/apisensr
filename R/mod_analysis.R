# Analysis Module UI
  
#' @title   UI Module for Analysis tab
#' @description  A shiny Module to render the analysis tab.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_analysis
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_analysis_ui <- function(id, label = "tab_analysis"){
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 3,
              material_card(
                  material_dropdown(
                      ns("type"),
                      label = NULL,
                      choices = c(
                          "Choose bias analysis:" = "bias_choice",
                          "Selection bias" = "selection",
                          "Misclassification bias" = "misclass"
                      ),
                      color = "#ff1744"
                  )
              )
          ),
          material_column(
              width = 9,
              renderText("Text!")
          )
      )
  )
}

# Module Server
    
#' @rdname mod_analysis
#' @export
#' @keywords internal
    
mod_analysis_server <- function(input, output, session){
    ns <- session$ns
#    input$bias_parms
}
    
## To be copied in the UI
# mod_analysis_ui("tab_analysis")
    
## To be copied in the server
# callModule(mod_analysis_server, "tab_analysis")
