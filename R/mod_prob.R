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
                  material_dropdown(
                      ns("prob_type"),
                      label = "Choose bias analysis:",
                      choices = c(
                          "Misclassification bias" = "probsens",
                          "Unmeasured confounder" = "probsens_conf"
                      ),
                      color = "#ff1744"
                  ),
                  "Observed data",
                  div(id = "obs-table-prob",
                      rHandsontableOutput(ns('two_by_two_prob')),
                      material_button(
                          input_id = ns("reset_table"),
                          label = "Table back to example",
                          icon = "restore",
                          color = "red accent-3"
                      )
                      ),
                  br(),
                  div(
                      id = "side-panel-prob",
                      conditionalPanel(
                          condition = 'input.prob_type == "probsens"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("misclassProb_type"),
                              label = "Misclassification of:",
                              choices = c("exposure", "outcome"),
                              selected = "exposure",
                              color = "#ff1744"),
                          material_slider(
                              input_id = ns("reps"),
                              label = "Number of replications to run:",
                              min_value = 10000,
                              max_value = 100000,
                              step_size = 5000,
                              initial_value = 25000,
                              color = "#ff1744"),
                          material_switch(
                              input_id = ns("diff"),
                              label = "",
                              off_label = "Non-differential misclassification",
                              on_label = "Differential misclassification",
                              initial_value = FALSE,
                              color = "#ff1744"),
                          br(),
                          material_checkbox(
                              input_id = ns("discard"),
                              label = "Discard draws of negative adjusted counts.",
                              initial_value = FALSE,
                              color = "#ff1744"),
                          conditionalPanel(
                              condition = 'input.diff == 1',
                              ns = ns,
                              material_slider(
                                  input_id = ns("corr_se"),
                                  label = "Correlation between case and non-case sensitivities:",
                                  min_value = 0,
                                  max_value = 1,
                                  step_size = 0.1,
                                  initial_value = 0.8,
                                  color = "#ff1744"
                              ),
                              material_slider(
                                  input_id = ns("corr_sp"),
                                  label = "Correlation between case and non-case specificities:",
                                  min_value = 0,
                                  max_value = 1,
                                  step_size = 0.1,
                                  initial_value = 0.8,
                                  color = "teal accent-2"
                              )                              
                          ),
                          material_button(
                              input_id = "help_probsens",
                              label = "Help",
                              icon = "help",
                              color = "orange")
                      ),
                      ## Alpha level
                      material_slider(
                          ns("alpha"),
                          HTML("&alpha;-level:"),
                          min_value = 0.01,
                          max_value = 0.2,
                          step_size = 0.01,
                          initial_value = 0.05,
                          color = "#ff1744"),
                      material_button(
                          input_id = "reset_input3",
                          label = "Parameters back to example",
                          icon = "restore",
                          color = "red accent-3"),
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
                          "Beta" = "beta"),
                      selected = "trapezoidal",
                      color = "#ff1744"),
                  conditionalPanel(
                      condition = 'input.seca_parms == "constant"',
                      ns = ns,
                      mod_parms_ui(ns("parms_seca_C"), "Constant value:", 0.8)
                  ),
                  conditionalPanel(
                      condition = 'input.seca_parms == "uniform"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_seca_U"), "Minimum and maximum:", 0.7, 0.9)
                  ),
                  conditionalPanel(
                      condition = 'input.seca_parms == "triangular"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_seca_Tr1"),
                                      "Lower and upper limit:", 0.7, 0.9),
                      mod_parms_ui(ns("parms_seca_Tr2"), "Mode:", 0.8)
                  ),
                  conditionalPanel(
                      condition = 'input.seca_parms == "trapezoidal"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_seca_Tz1"),
                                      "Minimum and maximum:", 0.75, 1),
                      mod_parmsrge_ui(ns("parms_seca_Tz2"),
                                      "Lower and upper mode:", 0.85, 0.95)
                  ),
                  conditionalPanel(
                      condition = 'input.seca_parms == "beta"',
                      ns = ns,
                      material_number_box(ns("parms_seca_B1"),
                                          "alpha:", min_value = 0, max_value = 10^6,
                                          initial_value = 908, color = "#ff1744"),
                      material_number_box(ns("parms_seca_B2"),
                                          "beta:", min_value = 0, max_value = 10^6,
                                          initial_value = 56, color = "#ff1744")
                  ),
                  conditionalPanel(
                      condition = 'input.diff == 1',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("seexp_parms"),
                          label = "Distribution, sensitivity of exposure classification among those without the outcome:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Beta" = "beta"),
                          selected = "trapezoidal",
                          color = "#ff1744")
                  )
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
                          "Beta" = "beta"),
                      selected = "trapezoidal",
                      color = "teal accent-2"),
                  conditionalPanel(
                      condition = 'input.spca_parms == "constant"',
                      ns = ns,
                      mod_parms_ui(ns("parms_spca_C"), "Constant value:", 0.8)
                  ),
                  conditionalPanel(
                      condition = 'input.spca_parms == "uniform"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_spca_U"), "Minimum and maximum:", 0.7, 0.9)
                  ),
                  conditionalPanel(
                      condition = 'input.spca_parms == "triangular"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_spca_Tr1"),
                                      "Lower and upper limit:", 0.7, 0.9),
                      mod_parms_ui(ns("parms_spca_Tr2"), "Mode:", 0.8)
                  ),
                  conditionalPanel(
                      condition = 'input.spca_parms == "trapezoidal"',
                      ns = ns,
                      mod_parmsrge_ui(ns("parms_spca_Tz1"),
                                      "Minimum and maximum:", 0.75, 1),
                      mod_parmsrge_ui(ns("parms_spca_Tz2"),
                                      "Lower and upper mode:", 0.85, 0.95)
                  ),
                  conditionalPanel(
                      condition = 'input.spca_parms == "beta"',
                      ns = ns,
                      material_number_box(ns("parms_spca_B1"),
                                          "alpha:", min_value = 0, max_value = 10^6,
                                          initial_value = 153, color = "teal accent-2"),
                      material_number_box(ns("parms_spca_B2"),
                                          "beta:", min_value = 0, max_value = 10^6,
                                          initial_value = 6, color = "teal accent-2")
                  )#,
#                  material_dropdown(
#                      input_id = ns("spexp_parms"),
#                      label = "Distribution, specificity of exposure classification among those without the outcome:",
#                      choices = c(
#                          "Constant" = "constant",
#                          "Uniform" = "uniform",
#                          "Triangular" = "triangular",
#                          "Trapezoidal" = "trapezoidal",
#                          "Beta" = "beta"),
#                      selected = "trapezoidal",
#                      color = "teal accent-2")
#              )
          )
      ),
      material_column(
          width = 8,
          material_card(
              verbatimTextOutput(ns("summary_prob"))
          )
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

    output$two_by_two_prob = renderRHandsontable({
                                                     input$reset_table # trigger rendering on reset
                                                     rhandsontable(DF(),
                                                                   rowHeaderWidth = 200,
                                                                   width = 500,
                                                                   stretchH = "all")
                                                 })

    episensrout = reactive({
                               mat <- as.matrix(hot_to_r(req({input$two_by_two_prob})))
                               if (input$seca_parms == "trapezoidal") {
                                   dist_seca <- c(callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz1")[2])
                               } else if (input$seca_parms == "triangular") {
                                   dist_seca <- c(callModule(mod_parmsrge_server,
                                                             "parms_seca_Tr1"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tr2"))
                               } else if (input$seca_parms == "uniform") {
                                   dist_seca <- callModule(mod_parmsrge_server,
                                                           "parms_seca_U")
                               } else if (input$seca_parms == "constant") {
                                   dist_seca <- callModule(mod_parms_server, "parms_seca_C")
                               } else if (input$seca_parms == "beta") {
                                   dist_seca <- c(input$parms_seca_B1, input$parms_seca_B2)
                               }
#                               dist_seexp <- if (input$seexp_parms == "trapezoidal") {
#                                                dist_seexp <- c(input$parms_seexp1[1],
#                                                               input$parms_seexp2[1],
#                                                               input$parms_seexp2[2],
#                                                               input$parms_seexp1[2])
#                                            } else if (input$seexp_parms == "triangular") {
#                                                dist_seexp <- c(input$parms_seexp1[1],
#                                                               input$parms_seexp1[2],
#                                                               input$parms_seexp2)
#                                            } else if (input$seexp_parms == "uniform") {
#                                                dist_seexp <- c(input$parms_seexp1[1],
#                                                               input$parms_seexp1[2])
#                                            } else if (input$seexp_parms == "constant") {
#                                                dist_seexp <- input$parms_seexp1
#                                            } else if (input$seexp_parms == "beta") {
#                                                dist_seexp <- c(input$parms_seexp1,
#                                                               input$parms_seexp2)
#                                            }
                               if (input$spca_parms == "trapezoidal") {
                                   dist_spca <- c(callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz1")[2])
                               } else if (input$spca_parms == "triangular") {
                                   dist_spca <- c(callModule(mod_parmsrge_server,
                                                             "parms_spca_Tr1"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tr2"))
                               } else if (input$spca_parms == "uniform") {
                                   dist_spca <- callModule(mod_parmsrge_server,
                                                           "parms_spca_U")
                               } else if (input$spca_parms == "constant") {
                                   dist_spca <- callModule(mod_parms_server, "parms_spca_C")
                               } else if (input$spca_parms == "beta") {
                                   dist_spca <- c(input$parms_spca_B1, input$parms_spca_B2)
                                            }
#                               dist_spexp <- if (input$spexp_parms == "trapezoidal") {
#                                                dist_spexp <- c(input$parms_spexp1[1],
#                                                               input$parms_spexp2[1],
#                                                               input$parms_spexp2[2],
#                                                               input$parms_spexp1[2])
#                                            } else if (input$spexp_parms == "triangular") {
#                                                dist_spexp <- c(input$parms_spexp1[1],
#                                                               input$parms_spexp1[2],
#                                                               input$parms_spexp2)
#                                            } else if (input$spexp_parms == "uniform") {
#                                                dist_spexp <- c(input$parms_spexp1[1],
#                                                               input$parms_spexp1[2])
#                                            } else if (input$spexp_parms == "constant") {
#                                                dist_spexp <- input$parms_spexp1
#                                            } else if (input$spexp_parms == "beta") {
#                                                dist_spexp <- c(input$parms_spexp1,
#                                                               input$parms_spexp2)
#                                            }

                               dist_seca
                               dist_spca
                               if (input$prob_type == "probsens" & input$diff == 0) {
                                   probsens(mat,
                                            type = input$probsens_type,
                                            reps = input$reps,
                                            seca.parms = list(input$seca_parms, dist_seca),
                                            spca.parms = list(input$spca_parms, dist_spca))
                               } else if (input$prob_type == "probsens" & input$diff == 1) {
                                   probsens(mat,
                                            type = input$probsens_type,
                                            reps = input$reps,
                                            seca.parms = list(input$seca_parms, dist_seca),
                                            seexp.parms = list(input$seca_parms, dist_seca),
                                            spca.parms = list(input$spca_parms, dist_spca),
                                            spexp.parms = list(input$spexp_parms,
                                                               dist_spexp),
                                            corr.se = 0.8,
                                            corr.sp = 0.8)
                               }
                           })

    ## Output
    output$summary_prob = renderPrint({
                                          episensrout()
                                      })
    
    runjs("document.getElementById('help_probsens').onclick = function() { 
           window.open('https://dhaine.github.io/episensr/reference/probsens.html', '_blank');
         };"
    )
}
    
## To be copied in the UI
# mod_prob_ui("tab_prob")
    
## To be copied in the server
# callModule(mod_prob_server, "tab_prob")
