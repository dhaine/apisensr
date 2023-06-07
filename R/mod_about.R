# About Module UI

#' @title   UI Module for the About Analysis tab
#' @description  A shiny Module to render the About analysis tab, including the table
#' for available functions and comparison with `episensr`, as well as `episensr` version.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom utils packageVersion citation
mod_about_ui <- function(id, label = "tab_about") {
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 9,
              material_card(
                  p("Quantitative bias analysis allows to estimate nonrandom errors in epidemiologic studies, assessing the magnitude and direction of biases, and quantifying their uncertainties. Every study has some random error due to its limited sample size, and is susceptible to systematic errors as well, from selection bias to the presence of (un)known confounders or information bias (measurement error, including misclassification). Bias analysis methods were compiled by Lash et al. in their book", a("Applying Quantitative Bias Analysis to Epidemiologic Data.", href="https://www.springer.com/us/book/9780387879604", target="_blank"), "This Shiny app implements bias analyses from the book, as well as others (e.g. by S. Greenland), as computed by the R package", a("episensr", href="https://dhaine.github.io/episensr/index.html", target="_blank"), ". More can be found in the", code("episensr"), "package available for download on", a("R CRAN", href="https://CRAN.R-project.org/package=episensr", target = "_blank"), ". The four tabs allow to perform (1) a simple analysis (for bias analysis requiring a 2-by-2 table as data input), (2) an analysis for covariate misclassification (requiring two 2-by-2 tables as data input), (3) a simple analysis with no observed data (for bias analysis that does not have as input an observed 2-by-2 table), and (4) a probabilistic bias analysis."), br(), br(), br(), includeMarkdown(system.file("app/www", "functions.md", package = "apisensr"))
              )
          ),
          material_column(
              width = 3,
              wellPanel(verbatimTextOutput(ns("cite_api"), placeholder = TRUE), br(), br(),"Please report bugs at", a("https://github.com/dhaine/apisensr/issues", href="https://github.com/dhaine/apisensr/issues", target="_blank"), br(), br(), "Shiny app by", a("Denis Haine", href="https://www.denishaine.ca/", target="_blank"), br(), br(), "episensr version:", verbatimTextOutput(ns("versioning_epi"), placeholder = TRUE), "apisensr version:", verbatimTextOutput(ns("versioning_api"), placeholder = TRUE))
          )
      )
  )
}

# Module Server

#' @rdname mod_about
#' @noRd
#' @keywords internal

mod_about_server <- function(input, output, session) {
    ns <- session$ns

    output$cite_api <- renderPrint(citation("apisensr"))
    output$versioning_epi <- renderPrint(packageVersion("episensr"))
    output$versioning_api <- renderPrint(packageVersion("apisensr"))

}

## To be copied in the UI
# mod_about_ui("tab_about")

## To be copied in the server
# callModule(mod_about_server, "tab_about")
