#' @import shiny
source("inst/app/www/pop_up.R")

app_server <- function(input, output, session) {
    # List the first level callModules here
    observe({
                pop_up('apisensr & episensr', 'inst/app/www/start.html', "Let's go!")
            })

    callModule(mod_analysis_server, "tab_analysis")
    callModule(mod_about_server, "tab_about")

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
