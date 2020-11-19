# Analysis Module UI

#' @title   UI Module for the Simple Analysis tab
#' @description  A shiny Module to render the Simple Analysis tab, i.e. non-probabilistic
#' analyses not requiring a 2-by-2 table as input (selection bias analysis `selection`,
#' bias analysis for unmeasured confounder `confounders`, bias analysis for unmeasured
#' 3-level confounder `confounders.poly`, bias analysis for unmeasured confounder with
#' effect modification `confounders.emm`, and misclassification bias analysis
#' `misclassification`).
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
                          "Unmeasured confounder" = "confounder",
                          "Unmeasured 3-level confounder" = "confounder_3",
                          "Unmeasured confounder with effect modification" = "confounder_emm",
                          "Misclassification bias" = "misclass"
                      ),
                      color = "#d50000"
                  ),
                  "Observed data",
                  div(id = "obs-table",
                      rHandsontableOutput(ns('two_by_two')),
                      material_button(
                          input_id = ns("reset_table"),
                          label = "Table back to example",
                          icon = "restore",
                          color = "red accent-4"
                      )
                      ),
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
                              color = "#9b0000"
                          ),
                          conditionalPanel(
                              condition = 'input.parms_controller == 0',
                              ns = ns,
                              mod_parms_ui(ns("parms_sel1"),
                                           "Selection probability among cases exposed:", 0.94),
                              mod_parms_ui(ns("parms_sel2"),
                                           "Selection probability among cases unexposed:", 0.85),
                              mod_parms_ui(ns("parms_sel3"),
                                           "Selection probability among noncases exposed:", 0.64),
                              mod_parms_ui(ns("parms_sel4"),
                                           "Selection probability among noncases unexposed:", 0.25)
                          ),
                          conditionalPanel(
                              condition = 'input.parms_controller == 1',
                              ns = ns,
                              mod_parms_ui(ns("bias_factor"),
                                           "Selection-bias factor:",
                                           value = 0.43)
                          ),
                          material_button(
                              input_id = "help_selection",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("confounder_type"),
                              label = "Type of implementation",
                              choices = c("Relative Risk" = "RR",
                                          "Odds Ratio" = "OR",
                                          "Risk Difference" = "RD"),
                              selected = "RR",
                              color = "#ff5131"
                          ),
                          div(
                              id = "side-panel_RR_RD",
                              conditionalPanel(
                                  condition = 'input.confounder_type != "RD"',
                                  ns = ns,
                                  mod_parms_ui(ns("parms_confounder1a"),
                                               "Association between the confounder and the outcome among those who were not exposed:", 0.63),
                                  ),
                              conditionalPanel(
                                  condition = 'input.confounder_type == "RD"',
                                  ns = ns,
                                  mod_parms3_ui(ns("parms_confounder1b"),
                                                "Association between the confounder and the outcome among those who were not exposed:", -0.37)
                              )
                          ),
                          mod_parms_ui(ns("parms_confounder2"),
                                       "Prevalence of the confounder among the exposed:", 0.8),
                          mod_parms_ui(ns("parms_confounder3"),
                                       "Prevalence of the confounder among the unexposed:", 0.05),
                          material_button(
                              input_id = "help_confounder",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder_3"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("confounder3_type"),
                              label = "Type of implementation",
                              choices = c("Relative Risk" = "RR",
                                          "Odds Ratio" = "OR",
                                          "Risk Difference" = "RD"),
                              selected = "RR",
                              color = "#ff1744"
                          ),
                          conditionalPanel(
                              condition = 'input.confounder3_type != "RD"',
                              ns = ns,
                              mod_parms_ui(ns("parms_confounder_3_1a"),
                                           "Association between the highest level confounder and the outcome:", 0.4),
                              mod_parms_ui(ns("parms_confounder_3_2a"),
                                           "Association between the mid-level confounder and the outcome:", 0.8)
                          ),
                          conditionalPanel(
                              condition = 'input.confounder3_type == "RD"',
                              ns = ns,
                              mod_parms3_ui(ns("parms_confounder_3_1b"),
                                            "Association between the highest level confounder and the outcome:", -0.4),
                              mod_parms3_ui(ns("parms_confounder_3_2b"),
                                            "Association between the mid-level confounder and the outcome:", -0.2)
                          ),
                          mod_parms_ui(ns("parms_confounder_3_3"),
                                       "Prevalence of the highest level confounder among the exposed:", 0.6),
                          mod_parms_ui(ns("parms_confounder_3_4"),
                                       "Prevalence of the highest level confounder among the unexposed:", 0.05),
                          mod_parms_ui(ns("parms_confounder_3_5"),
                                       "Prevalence of the mid-level confounder among the exposed:", 0.2),
                          mod_parms_ui(ns("parms_confounder_3_6"),
                                       "Prevalence of the mid-level confounder among the unexposed:", 0.2),
                          material_button(
                              input_id = "help_confounder3",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder_emm"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("confounderemm_type"),
                              label = "Type of implementation",
                              choices = c("Relative Risk" = "RR",
                                          "Odds Ratio" = "OR",
                                          "Risk Difference" = "RD"),
                              selected = "RR",
                              color = "#ff1744"
                          ),
                          conditionalPanel(
                              condition = 'input.confounderemm_type != "RD"',
                              ns = ns,
                              mod_parms_ui(ns("parms_confounder_emm_1a"),
                                           "Association between the confounder and the outcome among those who were exposed:", 0.4),
                              mod_parms_ui(ns("parms_confounder_emm_2a"),
                                           "Association between the confounder and the outcome among those who were not exposed:", 0.7)
                          ),
                          conditionalPanel(
                              condition = 'input.confounderemm_type == "RD"',
                              ns = ns,
                              mod_parms3_ui(ns("parms_confounder_emm_1b"),
                                            "Association between the confounder and the outcome among those who were exposed:", -0.6),
                              mod_parms3_ui(ns("parms_confounder_emm_2b"),
                                            "Association between the confounder and the outcome among those who were not exposed:", -0.3)
                          ),
                          mod_parms_ui(ns("parms_confounder_emm_3"),
                                       "Prevalence of the confounder among the exposed:", 0.8),
                          mod_parms_ui(ns("parms_confounder_emm_4"),
                                       "Prevalence of the confounder among the unexposed:", 0.05),
                          material_button(
                              input_id = "help_confounderemm",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "misclass"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("misclass_type"),
                              label = "Misclassification of:",
                              choices = c("exposure", "outcome"),
                              selected = "exposure",
                              color = "#ff1744"),
                          mod_parms_ui(ns("parms_mis1"),
                                       "Sensitivity of exposure (or outcome) classification among those with the outcome (or exposure):", 0.78),
                          mod_parms_ui(ns("parms_mis2"),
                                       "Sensitivity of exposure (or outcome) classification among those without the outcome (or exposure):", 0.78),
                          mod_parms_ui(ns("parms_mis3"),
                                       "Specificity of exposure (or outcome) classification among those with the outcome (or exposure):", 0.99),
                          mod_parms_ui(ns("parms_mis4"),
                                       "Specificity of exposure (or outcome) classification among those without the outcome (or exposure):", 0.99),
                          material_button(
                              input_id = "help_misclass",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      ## Alpha level
                      material_slider(
                          ns("alpha"),
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
                          color = "red accent-4"
                      )
                  )
              )
          ),
          material_column(
              width = 8,
              material_card(
                  verbatimTextOutput(ns("summary"))
              )
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
                      } else if (input$type == "confounder" |
                                 input$type == "confounder_3" |
                                 input$type == "confounder_emm") {
                          data.frame(Exposed = c(105, 527), Unexposed = c(85, 93),
                                     row.names = c("Cases", "Noncases"))
                      } else if (input$type == "misclass") {
                          data.frame(Exposed = c(215, 668), Unexposed = c(1449, 4296),
                                     row.names = c("Cases", "Noncases"))
                      }
                  })

    output$two_by_two = renderRHandsontable({
                                                input$reset_table # trigger rendering on reset
                                                rhandsontable(DF(),
                                                              rowHeaderWidth = 200,
                                                              width = 500,
                                                              stretchH = "all")
                                            })

    episensrout = reactive({
                               mat <- as.matrix(hot_to_r(req({input$two_by_two})))
                               if (input$type == "selection") {
                                  episensr::selection(mat,
                                                      bias_parms = if (input$parms_controller == 0) {
                                                                       c(callModule(mod_parms_server, "parms_sel1"),
                                                                         callModule(mod_parms_server, "parms_sel2"),
                                                                         callModule(mod_parms_server, "parms_sel3"),
                                                                         callModule(mod_parms_server, "parms_sel4"))
                                                                   } else if (input$parms_controller == 1) {
                                                                       input$bias_factor
                                                                   },
                                                      alpha = input$alpha)
                               } else if (input$type == "confounder") {
                                   episensr::confounders(mat,
                                                         type = input$confounder_type,
                                                         bias_parms = c(if (input$confounder_type != "RD")
                                                                        {callModule(mod_parms_server, "parms_confounder1a")} else callModule(mod_parms3_server, "parms_confounder1b"),
                                                                        callModule(mod_parms_server, "parms_confounder2"),
                                                                        callModule(mod_parms_server, "parms_confounder3")),
                                                         alpha = input$alpha)
                               } else if (input$type == "confounder_3") {
                                   episensr::confounders.poly(mat,
                                                              type = input$confounder3_type,
                                                              bias_parms = c(if (input$confounder3_type != "RD")
                                                                             {callModule(mod_parms_server, "parms_confounder_3_1a")}
                                                                             else callModule(mod_parms3_server, "parms_confounder_3_1b"),
                                                                             if (input$confounder3_type != "RD")
                                                                             {callModule(mod_parms_server, "parms_confounder_3_2a")}
                                                                             else callModule(mod_parms3_server, "parms_confounder_3_2b"),
                                                                             callModule(mod_parms_server, "parms_confounder_3_3"),
                                                                             callModule(mod_parms_server, "parms_confounder_3_4"),
                                                                             callModule(mod_parms_server, "parms_confounder_3_5"),
                                                                             callModule(mod_parms_server, "parms_confounder_3_6")),
                                                              alpha = input$alpha)
                               } else if (input$type == "confounder_emm") {
                                   episensr::confounders.emm(mat,
                                                             type = input$confounderemm_type,
                                                             bias_parms = c(if (input$confounderemm_type != "RD")
                                                                            {callModule(mod_parms_server, "parms_confounder_emm_1a")}
                                                                            else callModule(mod_parms3_server, "parms_confounder_emm_1b"),
                                                                            if (input$confounderemm_type != "RD")
                                                                            {callModule(mod_parms_server, "parms_confounder_emm_2a")}
                                                                            else callModule(mod_parms3_server, "parms_confounder_emm_2b"),
                                                                            callModule(mod_parms_server, "parms_confounder_emm_3"),
                                                                            callModule(mod_parms_server, "parms_confounder_emm_4")),
                                                             alpha = input$alpha
                                                             )
                               } else if (input$type == "misclass") {
                                   episensr::misclassification(mat,
                                                               type = input$misclass_type,
                                                               bias_parms = c(callModule(mod_parms_server, "parms_mis1"),
                                                                              callModule(mod_parms_server, "parms_mis2"),
                                                                              callModule(mod_parms_server, "parms_mis3"),
                                                                              callModule(mod_parms_server, "parms_mis4")),
                                                               alpha = input$alpha)
                               }
                           })

    ## Output
    output$summary = renderPrint({
                                     episensrout()
                                 })

    runjs("document.getElementById('help_selection').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/selection.html', '_blank');
         };"
         )

    runjs("document.getElementById('help_confounder').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.html', '_blank');
         };"
         )

    runjs("document.getElementById('help_confounder3').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.poly.html', '_blank');
         };"
         )

    runjs("document.getElementById('help_confounderemm').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.emm.html', '_blank');
         };"
         )

    runjs("document.getElementById('help_misclass').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/misclassification.html', '_blank');
         };"
  )
}

## To be copied in the UI
# mod_analysis_ui("tab_analysis")

## To be copied in the server
# callModule(mod_analysis_server, "tab_analysis")
