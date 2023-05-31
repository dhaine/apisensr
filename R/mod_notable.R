# Analysis Module UI with no observed data table

#' @title   UI Module for Simple Analysis tab with no 2x2 table.
#' @description  A shiny Module to render the Simple Analysis tab when no 2-by-2 table
#' is provided (M-bias analysis `mbias` and analysis by bounding the bias limits of
#' unmeasured confounding `confounders.limit`).
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_notable
#'
#' @keywords internal
#' @noRd
#' @import episensr
#' @importFrom shiny NS tagList
#' @importFrom shinyjs runjs
mod_notable_ui <- function(id, label = "tab_notable") {
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
                          "M-bias" = "mbias",
                          "Bounding the bias limits of unmeasured confounding" = "confounder_limit",
                          "Unmeasured confounders based on external adjustment" = "confounder_ext",
                          "Bias due to unmeasured confounders based on confounding imbalance among exposed and unexposed" = "confounder_array"
                      ),
                      color = "#d50000"
                  ),
                  br(),
                  div(
                      id = "side-panel-notab",
                      conditionalPanel(
                          condition = 'input.type == "mbias"',
                          ns = ns,
                          mod_parms3b_ui(ns("parms_mbias1"),
                                        "Odds ratio between A and the exposure E:", 2),
                          mod_parms3b_ui(ns("parms_mbias2"),
                                        "Odds ratio between A and the collider C:", 5.4),
                          mod_parms3b_ui(ns("parms_mbias3"),
                                        "Odds ratio between B and the collider C:", 2.5),
                          mod_parms3b_ui(ns("parms_mbias4"),
                                        "Odds ratio between B and the outcome D:", 1.5),
                          mod_parms3b_ui(ns("parms_mbias5"),
                                        "Odds ratio observed between the exposure E and the outcome D", 1),
                          material_button(
                              input_id = "help_mbias",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder_limit"',
                          ns = ns,
                          mod_parms3a_ui(ns("parms_conflimit1"),
                                       "Proportion with the confounder among the unexposed group:", NA),
                          mod_parms3b_ui(ns("parms_conflimit2"),
                                       "Relative risk between the confounder and the outcome:", NA),
                          mod_parms3b_ui(ns("parms_conflimit3"),
                                        "Odds ratio between the confounder and the outcome:", 1.65),
                          mod_parms3b_ui(ns("parms_conflimit4"),
                                         "Crude relative risk between the exposure and the outcome", 1.5),
                          material_button(
                              input_id = "help_conflimit",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder_ext"',
                          ns = ns,
                          mod_parms3c_ui(ns("parms_confext0"),
                                         "\"True\" or fully adjusted exposure relative risk:",
                                         1),
                          mod_parms3c_ui(ns("parms_confext1"),
                                         "Association between the confounder and the outcome (RR):",
                                         0.1),
                          mod_parms3c_ui(ns("parms_confext2"),
                                       "Association between the exposure category and the confounder (OR):", 0.9),
                          mod_parms3a_ui(ns("parms_confext3"),
                                        "Prevalence of the confounder:", 0.1),
                          mod_parms3a_ui(ns("parms_confext4"),
                                         "Prevalence of the exposure", 0.4),
                          material_button(
                              input_id = "help_confext",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      conditionalPanel(
                          condition = 'input.type == "confounder_array"',
                          ns = ns,
                          mod_parms3_ui(ns("parms_confarray0"), "Crude risk:", 1.5),
                          material_radio_button(
                              input_id = ns("parms_confarray1"),
                              label = "Type of implementation:",
                              choices = c("Binary covariates" = "binary",
                                          "Continuous covariates" = "continuous",
                                          "Risk difference" = "RD"),
                              selected = "binary",
                              color = "#ff5131"),
                          mod_parms3_ui(ns("parms_confarray2"),
                                         "Association between the confounder and the outcome (RR):",
                                         5.5),
                          mod_parms3_ui(ns("parms_confarray3"),
                                        "Prevalence of the confounder among the exposed, or mean value of the confounder among the exposed:", 0.5),
                          mod_parms3a_ui(ns("parms_confarray4"),
                                         "Prevalence of the confounder among the unexposed, or mean value of the confounder among the unexposed", 0.1),
                          material_button(
                              input_id = "help_confarray",
                              label = "Help",
                              icon = "help",
                              color = "orange"
                          )
                      ),
                      material_button(
                          input_id = "reset_input2",
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
              ),
              conditionalPanel(
                  condition = 'input.type == "mbias"',
                  ns = ns,
                  material_column(
                      width = 6,
                      material_card(
                          title = "DAG before conditioning on M",
                          plotOutput(ns("plot_mbias_before"), width = "400px")
                      )
                  ),
                  material_column(
                      width = 6,
                      material_card(
                          title = "DAG after conditioning on M",
                          plotOutput(ns("plot_mbias_after"), width = "400px")
                      )
                  )
              )
          )
      )
  )
}

# Module Server

#' @rdname mod_notable
#' @noRd
#' @keywords internal

mod_notable_server <- function(input, output, session) {
    ns <- session$ns

    episensrout = reactive({
                               if (input$type == "mbias") {
                                   episensr::mbias(or = c(callModule(mod_parms3b_server, "parms_mbias1"),
                                                          callModule(mod_parms3b_server, "parms_mbias2"),
                                                          callModule(mod_parms3b_server, "parms_mbias3"),
                                                          callModule(mod_parms3b_server, "parms_mbias4"),
                                                          callModule(mod_parms3b_server, "parms_mbias5")),
                                                   var = c("Outcome", "Exposure", "A", "B", "Collider"))
                               } else if (input$type == "confounder_limit") {
                                   episensr::confounders.limit(p = callModule(mod_parms3a_server, "parms_conflimit1"),
                                                               RR = callModule(mod_parms3b_server, "parms_conflimit2"),
                                                               OR = callModule(mod_parms3b_server, "parms_conflimit3"),
                                                               crude.RR = callModule(mod_parms3b_server, "parms_conflimit4")
                                                               )
                               } else if (input$type == "confounder_ext") {
                                   episensr::confounders.ext(RR = callModule(mod_parms3b_server,
                                                                            "parms_confext0"),
                                                            bias_parms = c(callModule(mod_parms3b_server, "parms_confext1"), callModule(mod_parms3b_server, "parms_confext2"), callModule(mod_parms3c_server, "parms_confext3"), callModule(mod_parms3c_server, "parms_confext4")))
                               } else if (input$type == "confounder_array") {
                                   episensr::confounders.array(
                                                 crude.risk = callModule(mod_parms3_server,
                                                                         "parms_confarray0"),
                                                 type = input$parms_confarray1,
                                                 bias_parms = c(callModule(mod_parms3_server, "parms_confarray2"), callModule(mod_parms3_server, "parms_confarray3"), callModule(mod_parms3_server, "parms_confarray4"))
                                             )
                               }
                           })

    ## Output
    output$summary <- renderPrint({
                                      episensrout()
                                  })

    output$plot_mbias_before <- renderPlot({
                                               draw_mdag_before(episensrout())
                                      })

    output$plot_mbias_after <- renderPlot({
                                              draw_mdag_after(episensrout())
                                          })

    shinyjs::runjs("document.getElementById('help_confarray').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.array.html', '_blank');
         };"
         )

    shinyjs::runjs("document.getElementById('help_confext').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.ext.html', '_blank');
         };"
         )

    shinyjs::runjs("document.getElementById('help_conflimit').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/confounders.limit.html', '_blank');
         };"
         )

    shinyjs::runjs("document.getElementById('help_mbias').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/mbias.html', '_blank');
         };"
  )
}

## To be copied in the UI
# mod_notable_ui("tab_notable")

## To be copied in the server
# callModule(mod_notable_server, "tab_notable")
