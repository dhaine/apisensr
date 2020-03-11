# Probabilistic Analysis Module UI
  
#' @title   UI Module for Probabilistic analysis tab
#' @description  A shiny Module to render the probabilistic analysis tab.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_prob
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_prob_ui <- function(id, label = "tab_prob"){
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 4,
              material_card(
                  depth = 5,
                  material_dropdown(
                      ns("prob_type"),
                      label = "Choose bias analysis:",
                      choices = c(
                          "Selection bias" = "probsens"
                      ),
                      color = "#ff1744"),
                  "Observed data",
                  rHandsontableOutput(ns('two_by_two')),
                  br(),
                  div(
                      id = "side-panel2",
                      material_radio_button(
                          input_id = "misclassProb_type",
                          label = "Misclassification of:",
                          choices = c("exposure", "outcome"),
                          selected = "exposure",
                          color = "#ff1744"),
                      material_switch(
                          input_id = "prob_corr",
                          label = "Correlations between case and non-case?",
                          off_label = "none",
                          on_label = "yes",
                          initial_value = FALSE,
                          color = "#ff1744"),
                      material_slider(
                          input_id = "reps",
                          label = "Number of replications to run:",
                          min_value = 10000,
                          max_value = 100000,
                          step_size = 5000,
                          initial_value = 25000,
                          color = "#ff1744")

                  )
              )
          ),
          material_column(
              width = 4,
              material_card(
                  material_dropdown(
                      input_id = ns("seca_parms"),
                      label = "Distribution, sensitivity of exposure classification among those with the outcome:",
                      choices = c(
                          "Constant" = "constant",
                          "Uniform" = "uniform",
                          "Triangular" = "triangular",
                          "Trapezoidal" = "trapezoidal",
                          "Logit-logistic" = "logit-logistic",
                          "Logit-normal" = "logit-normal"),
                      selected = "trapezoidal",
                      color = "#ff1744"),
                  material_dropdown(
                      input_id = ns("seexp_parms"),
                      label = "Distribution, sensitivity of exposure classification among those without the outcome:",
                      choices = c(
                          "Constant" = "constant",
                          "Uniform" = "uniform",
                          "Triangular" = "triangular",
                          "Trapezoidal" = "trapezoidal",
                          "Logit-logistic" = "logit-logistic",
                          "Logit-normal" = "logit-normal"),
                      selected = "trapezoidal",
                      color = "#ff1744")
              )
          ),
          material_column(
              width = 4,
              material_card(
                  material_dropdown(
                      input_id = ns("spca_parms"),
                      label = "Distribution, specificity of exposure classification among those with the outcome:",
                      choices = c(
                          "Constant" = "constant",
                          "Uniform" = "uniform",
                          "Triangular" = "triangular",
                          "Trapezoidal" = "trapezoidal",
                          "Logit-logistic" = "logit-logistic",
                          "Logit-normal" = "logit-normal"),
                      selected = "trapezoidal",
                      color = "teal accent-2"),
                  material_dropdown(
                      input_id = ns("spexp_parms"),
                      label = "Distribution, specificity of exposure classification among those without the outcome:",
                      choices = c(
                          "Constant" = "constant",
                          "Uniform" = "uniform",
                          "Triangular" = "triangular",
                          "Trapezoidal" = "trapezoidal",
                          "Logit-logistic" = "logit-logistic",
                          "Logit-normal" = "logit-normal"),
                      selected = "trapezoidal",
                      color = "teal accent-2")
              )
          )
      ),
      material_row(
          material_column(
              width = 12,
              "Text!"
          )
      )
  )
}

# Module Server
    
#' @rdname mod_prob
#' @export
#' @keywords internal
    
mod_prob_server <- function(input, output, session){
    ns <- session$ns

    DF = reactive({
                      if(input$prob_type == "probsens") {
                          data.frame(Exposed = c(45, 257), Unexposed = c(94, 945),
                                     row.names = c("Cases", "Noncases"))
                      }
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
                               if (input$prob_type == "probsens") {
                                   mod <- probsens(mat,
                                                   type = input$probsens_type,
                                                   reps = 20000,
                                                   seca.parms = list("trapezoidal", c(.75, .85, .95, 1)),
                                                   spca.parms = list("trapezoidal", c(.75, .85, .95, 1)))
                               }
                           })

    ## Output
#    output$summary = renderUI({episensrout()})

}
    
## To be copied in the UI
# mod_prob_ui("tab_prob")
    
## To be copied in the server
# callModule(mod_prob_server, "tab_prob")
