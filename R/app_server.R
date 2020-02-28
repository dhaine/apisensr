#' @import shiny
source("inst/app/www/pop_up.R")
app_server <- function(input, output,session) {
    # List the first level callModules here
    observe({
                pop_up('apisensr & episensr', 'inst/app/www/start.html', "Let's go!")
            })

    ## Automatically stop Shiny app when closing browser tab
    session$onSessionEnded(stopApp)

}
