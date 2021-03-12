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
mod_notable_ui <- function(id, label = "tab_notable"){
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
                          "Bounding the bias limits of unmeasured confounding" = "confounder_limit"
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

mod_notable_server <- function(input, output, session){
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
