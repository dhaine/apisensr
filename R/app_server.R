#' @importFrom shinyjs reset

app_server <- function(input, output, session) {
    # List the first level callModules here
    callModule(mod_analysis_server, "tab_analysis")
    callModule(mod_multi_server, "tab_multi")
    callModule(mod_notable_server, "tab_notable")
    callModule(mod_prob_server, "tab_prob")
    callModule(mod_about_server, "tab_about")

    ## Observed events
    observeEvent(input$reset_input, {
                     shinyjs::reset("side-panel")
                     shinyjs::reset("side-panel_RR_RD")
                 })
    observeEvent(input$reset_input2, {
                     shinyjs::reset("side-panel-notab")
                 })
    observeEvent(input$reset_input3, {
                     shinyjs::reset("side-panel-multi")
                 })

    ## Automatically stop Shiny app when closing browser tab
    session$onSessionEnded(stopApp)
}
