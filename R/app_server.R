#' @import shiny
source("inst/app/www/pop_up.R")
app_server <- function(input, output,session) {
    # List the first level callModules here
    observe({
                pop_up('apisensr & episensr', 'inst/app/www/start.html', "Let's go!")
            })

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
                                                              width = 400,
                                                              stretchH = "all")
                                            })

    ## Observed events
    observeEvent(input$reset_input, {
                     shinyjs::reset("parms")
                 })

    observeEvent(input$type_load, {
                     shinyjs::show("parms")
                 }
    )

    ## Automatically stop Shiny app when closing browser tab
    session$onSessionEnded(stopApp)

}
