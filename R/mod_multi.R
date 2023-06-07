# Analysis Module UI for Multiple Tables

#' @title   UI Module for the Simple Analysis Multiple Tables tab
#' @description  A shiny Module to render the Simple Analysis Multiple Tables tab,
#' i.e. analysis to correct for a misclassified covariate (`misclassification.cov`).
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_multi
#'
#' @keywords internal
#' @noRd
#' @import episensr
#' @importFrom shiny NS tagList
#' @importFrom shinyjs runjs
#' @importFrom rhandsontable hot_to_r rHandsontableOutput renderRHandsontable rhandsontable
mod_multi_ui <- function(id, label = "tab_multi") {
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 4,
              material_card(
                  "Sensitivity Analysis for Covariate Misclassification",
                  br(),
                  "First level of stratification",
                  div(id = "obs-table1",
                      rhandsontable::rHandsontableOutput(ns('two_by_twoA'))
                      ),
                  br(),
                  "Second level of stratification",
                  div(id = "obs-table2",
                      rhandsontable::rHandsontableOutput(ns('two_by_twoB'))
                      ),
                  br(),
                  div(
                      id = "side-panel-multi",
                      mod_parms_ui(ns("parms_miscov1"),
                                   "Sensitivity of confounder classification among those with the outcome:", 0.6),
                      mod_parms_ui(ns("parms_miscov2"),
                                   "Sensitivity of confounder classification among those without the outcome:", 0.6),
                      mod_parms_ui(ns("parms_miscov3"),
                                   "Specificity of confounder classification among those with the outcome:", 0.95),
                      mod_parms_ui(ns("parms_miscov4"),
                                   "Specificity of confounder classification among those without the outcome:", 0.95),
                      material_button(
                          input_id = "help_misclass_cov",
                          label = "Help",
                          icon = "help",
                          color = "orange"),
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
                          color = "red accent-4")
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

#' @rdname mod_multi
#' @noRd
#' @keywords internal

mod_multi_server <- function(input, output, session) {
    ns <- session$ns

    DF = reactive({
                      tab2 <- data.frame(Exposed = c(565, 3583), Unexposed = c(781, 21958),
                                        row.names = c("Cases", "Noncases"))
                      tab3 <- data.frame(Exposed = c(754, 34471), Unexposed = c(4860, 383588),
                                        row.names = c("Cases", "Noncases"))
                      list(#df1 = tab1,
                           df2 = tab2, df3 = tab3)
                          })

    output$two_by_twoA = rhandsontable::renderRHandsontable({
                                                               rhandsontable::rhandsontable(DF()[['df2']], rowHeaderWidth = 200, width = 500, stretchH = "all")
                                            })
    output$two_by_twoB = rhandsontable::renderRHandsontable({
                                                               rhandsontable::rhandsontable(DF()[['df3']], rowHeaderWidth = 200, width = 500, stretchH = "all")
                                            })

    episensrout = reactive({
                               mat1 <- as.matrix(rhandsontable::hot_to_r(req({input$two_by_twoA})))
                               mat2 <- as.matrix(rhandsontable::hot_to_r(req({input$two_by_twoB})))
                               mat <- mat1 + mat2
                               episensr::misclassification.cov(array(c(mat, mat1, mat2),
                                                                     dimnames = list(c("Cases",
                                                                                       "Noncases"),
                                                                                     c("Exposed",
                                                                                       "Unexposed"),
                                                                                     c("Total",
                                                                                       "1st strata level",
                                                                                       "2nd strata level")),
                                                                     dim = c(2, 2, 3)),
                                                               bias_parms =
                                                                   c(callModule(mod_parms_server,
                                                                                "parms_miscov1"),
                                                                     callModule(mod_parms_server,
                                                                                "parms_miscov2"),
                                                                     callModule(mod_parms_server,
                                                                                "parms_miscov3"),
                                                                     callModule(mod_parms_server,
                                                                                "parms_miscov4")),
                                                               alpha = input$alpha
                                                           )
                           })

    ## Output
    output$summary = renderPrint({
                                     episensrout()
                                 })

    shinyjs::runjs("document.getElementById('help_misclass_cov').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/misclassification.cov.html', '_blank');
         };"
         )
}

## To be copied in the UI
# mod_multi_ui("tab_multi")

## To be copied in the server
# callModule(mod_multi_server, "tab_multi")
