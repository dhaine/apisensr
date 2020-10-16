#' @import shiny
#' @import shinymaterial
#' @import shinyjs
#' @import rmarkdown
#' @import rhandsontable
#' @import episensr
#' @import ggplot2
#' @import dagitty
#' @import ggdag

app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    material_page(
        ## title bar
        nav_bar_fixed = TRUE,
        nav_bar_color = "red accent-4",
        title = "apisensr: Happy API for episensr!",
        
        useShinyjs(),
   
        ## tabs
        material_tabs(
            tabs = c(
                "Simple analysis" = "tab_analysis",
                "Simple analysis, no observed data" = "tab_notable",
                "Probabilistic analysis" = "tab_prob",
                "About" = "tab_about"
            ),
            color = "#ff8a80"
        ),

        mod_analysis_ui("tab_analysis"),

        mod_notable_ui("tab_notable"),

        mod_prob_ui("tab_prob"),

        mod_about_ui("tab_about")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'apisensr')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(ext = 'png'),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
#    shinyalert::useShinyalert()
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    )

#    tags$footer(
#             actionLink("show_help", "Help"),
#             align = "center", style = "
#              bottom:0;
#              width:100%;
#              color: black;
#              padding: 10px;
#              background-color: #F5F5F5;
#              z-index: 1000;"
#         )
}
