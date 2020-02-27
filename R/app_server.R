#' @import shiny
source("inst/app/www/pop_up.R")
app_server <- function(input, output,session) {
    # List the first level callModules here
    # get started button clicked
#    observeEvent(input$show_help,{
#        pop_up('Welcome to apisensr', 'inst/app/www/get_started.html', 'Get started')
#        })
    ## observe show intro event which is triggered on start from custom.js
    observe({
                pop_up('apisensr & episensr', 'inst/app/www/start.html', "Let's go!")
    })
}
