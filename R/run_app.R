#' Run the Shiny Application
#'
#' @description Runs the 'apisensr' Shiny application. This function does not
#' return; interrupt R to stop the application (usually by pressing Ctrl+C or Esc).
#'
#' @param ... Unused arguments.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
