# About Module UI
  
#' @title   UI Module for About Analysis tab
#' @description  A shiny Module to render the About analysis tab.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id, label = "tab_about"){
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 3,
              material_card(
                  material_dropdown(
                      ns("about_type"),
                      label = "Information about which bias analysis?",
                      choices = c(
                          "Selection bias" = "about_selection",
                          "Misclassification bias" = "about_misclass"
                      ),
                      color = "#ff1744"
                  )
              )
          ),
          material_column(
              width = 9,
              uiOutput(ns("about_bias_choice"))
          )
      )
  )
}

# Module Server
    
#' @rdname mod_about
#' @export
#' @keywords internal
    
mod_about_server <- function(input, output, session){
    ns <- session$ns

    output$about_bias_choice <- renderUI({
                                             bias_file <- switch(input$about_type,
                                                                 about_selection = "inst/app/www/selection_bias.md",
                                                                 about_misclass = "inst/app/www/misclassification.md"
                                                                 )
                                             includeMarkdown(bias_file)
                                         })
}
    
## To be copied in the UI
# mod_about_ui("tab_about")
    
## To be copied in the server
# callModule(mod_about_server, "tab_about")
