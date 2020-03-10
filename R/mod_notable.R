# Analysis Module UI with no observed data table
  
#' @title   UI Module for Analysis tab with no 2x2 table.
#' @description  A shiny Module to render the analysis tab when no 2-by-2 table
#' is provided.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_notable
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
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
                          "M-bias" = "mbias"
                      ),
                      color = "#ff1744"
                  ),
                  br(),
                  div(
                      id = "side-panel-notab",
                      conditionalPanel(
                          condition = 'input.type == "mbias"',
                          ns = ns,
                          mod_parms2_ui(ns("parms_mbias1"),
                                        "Odds ratio between A and the exposure E:", 2),
                          mod_parms2_ui(ns("parms_mbias2"),
                                        "Odds ratio between A and the collider C:", 5.4),
                          mod_parms2_ui(ns("parms_mbias3"),
                                        "Odds ratio between B and the collider C:", 2.5),
                          mod_parms2_ui(ns("parms_mbias4"),
                                        "Odds ratio between B and the outcome D:", 1.5),
                          mod_parms2_ui(ns("parms_mbias5"),
                                        "Odds ratio observed between the exposure E and the outcome D", 1)
                      ),
                      material_button(
                          input_id = "reset_input2",
                          label = "Parameters back to example",
                          icon = "restore",
                          color = "red accent-3"
                      )
                  )
              )
          ),
          material_column(
              width = 8,
              material_card(
                  verbatimTextOutput(ns("summary"))
              ),
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
}

# Module Server
    
#' @rdname mod_notable
#' @export
#' @keywords internal
    
mod_notable_server <- function(input, output, session){
    ns <- session$ns

    episensrout = reactive({
                               if (input$type == "mbias") {
                                   mbias(or = c(callModule(mod_parms2_server, "parms_mbias1"),
                                                callModule(mod_parms2_server, "parms_mbias2"),
                                                callModule(mod_parms2_server, "parms_mbias3"),
                                                callModule(mod_parms2_server, "parms_mbias4"),
                                                callModule(mod_parms2_server, "parms_mbias5")),
                                         var = c("Outcome", "Exposure", "A", "B", "Collider"))
                               } else if (input$type != "mbias") {
                                   "Text"
                               }
                           })

    ## Output
    output$summary <- renderPrint({
                                      episensrout()
                                  })

    output$plot_mbias_before <- renderPlot({
                                               plot_out <- plot(episensrout(),
                                                                type = "before")
                                               plot_out
                                           })

    output$plot_mbias_after <- renderPlot({
                                              plot_out <- plot(episensrout(),
                                                               type = "after")
                                              plot_out
                                           })
}
    
## To be copied in the UI
# mod_notable_ui("tab_notable")
    
## To be copied in the server
# callModule(mod_notable_server, "tab_notable")