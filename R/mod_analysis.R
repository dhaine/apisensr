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
              width = 4,
              material_card(
                  material_dropdown(
                      ns("type"),
                      label = "Choose bias analysis:",
                      choices = c(
                          "Selection bias" = "selection",
                          "Misclassification bias" = "misclass"
                      ),
                      color = "#ff1744"
                  ),
                  "Observed data",
                  rHandsontableOutput(ns('two_by_two')),
                  br(),
                  div(
                      id = "side-panel",
                      conditionalPanel(
#                      condition = paste0('input[\'', ns('type'), "\' == \'selection\'"),
                          condition = 'input.type == "selection"',
                          ns = ns,
                          material_checkbox(
                              input_id = ns("parms_controller"),
                              label = "Providing Selection-bias factor instead of Selection probabilities",
                              initial_value = FALSE,
                              color = "#ff1744"
                          ),
                          conditionalPanel(
                              condition = 'input.parms_controller == 0',
                              ns = ns,
                              mod_parms_ui("parms_sel1",
                                           "Selection probability among cases exposed:", 0.94),
                              mod_parms_ui("parms_sel2",
                                           "Selection probability among cases unexposed:", 0.85),
                              mod_parms_ui("parms_sel3",
                                           "Selection probability among noncases exposed:", 0.64),
                              mod_parms_ui("parms_sel4",
                                           "Selection probability among noncases unexposed:", 0.25)
                          ),
                          conditionalPanel(
                              condition = 'input.parms_controller == 1',
                              ns = ns,
                              sliderInput("bias_factor",
                                          "Selection-bias factor:",
                                          value = 0.43,
                                          min = 0,
                                          max = 1)
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "misclass"',
                          ns = ns,
                          material_radio_button(
                              input_id = "misclass_type",
                              label = "Misclassification of:",
                              choices = c("exposure", "outcome"),
                              selected = "exposure",
                              color = "#ff1744"),
                          mod_parms_ui("parms_mis1",
                                       "Sensitivity of exposure (or outcome) classification among those with the outcome (or exposure):", 0.78),
                          mod_parms_ui("parms_mis2",
                                       "Sensitivity of exposure (or outcome) classification among those without the outcome (or exposure):", 0.78),
                          mod_parms_ui("parms_mis3",
                                       "Specificity of exposure (or outcome) classification among those with the outcome (or exposure):", 0.99),
                          mod_parms_ui("parms_mis4",
                                       "Specificity of exposure (or outcome) classification among those without the outcome (or exposure):", 0.99)
                      ),
                      ## Alpha level
                      material_slider(
                          "alpha",
                          HTML("&alpha;-level:"),
                          min_value = 0.01,
                          max_value = 0.2,
                          step_size = 0.01,
                          initial_value = 0.05,
                          color = "#ff1744"
                      ),
                      material_button(
                          input_id = "reset_input",
                          label = "Parameters back to example",
                          icon = "restore",
                          color = "red accent-3"
                      )
                  )
              )
          ),
          material_column(
              width = 8,
              "Text!"
#              uiOutput(ns("summary"))
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

    DF = reactive({
                      if(input$type == "selection") {
                          data.frame(Exposed = c(136, 297), Unexposed = c(107, 165),
                                     row.names = c("Cases", "Noncases"))
                      } else if(input$type == "misclass") {
                          data.frame(Exposed = c(215, 668), Unexposed = c(1449, 4296),
                                     row.names = c("Cases", "Noncases"))
                      }# else if(input$type == "probsens") {
                       #   data.frame(Exposed = c(45, 257), Unexposed = c(94, 945),
                       #              row.names = c("Cases", "Noncases"))
                      #}
                  })

    output$two_by_two = renderRHandsontable({
                                                input$reset_input # trigger rendering on reset
                                                rhandsontable(DF(),
                                                              rowHeaderWidth = 200,
                                                              width = 500,
                                                              stretchH = "all")
                                            })

    episensrout = reactive({
                               mat <- as.matrix(hot_to_r(req({input$two_by_two})))
                               if (input$type == "selection") {
                                   mod <- selection(mat,
                                                    bias_parms = if (input$parms_controller == FALSE) {
                                                                     c(callModule(mod_parms_server, "parms_sel1"),
                                                                       callModule(mod_parms_server, "parms_sel2"),
                                                                       callModule(mod_parms_server, "parms_sel3"),
                                                                       callModule(mod_parms_server, "parms_sel4"))
                                                                 } else if (input$parms_controller == TRUE) {
                                                                     input$bias_factor
                                                                 },
                                                    alpha = input$alpha)
                               } else if (input$type == "misclass") {
                                   mod <- misclassification(mat,
                                                            type = input$misclass_type,
                                                            bias_parms = c(callModule(mod_parms_server, "parms_mis1"),
                                                                           callModule(mod_parms_server, "parms_mis2"),
                                                                           callModule(mod_parms_server, "parms_mis3"),
                                                                           callModule(mod_parms_server, "parms_mis4")),
                                                            alpha = input$alpha)
                               }# else if (input$type == "probsens") {
#                                   mod <- probsens(mat,
#                                                   type = input$probsens_type,
#                                                   reps = 20000,
#                                                   seca.parms = list("trapezoidal", c(.75, .85, .95, 1)),
#                                                   spca.parms = list("trapezoidal", c(.75, .85, .95, 1)))
#                               }
                           })

    ## Output
#    output$summary = renderUI({episensrout()})

}
    
## To be copied in the UI
# mod_analysis_ui("tab_analysis")
    
## To be copied in the server
# callModule(mod_analysis_server, "tab_analysis")
