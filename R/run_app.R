#' Launch the PCR simulator Shiny App
#'
#' This function launches the interactive user interface for PCR
#' for use in validating designed primers bbefore synthesis.
#'
#' @export
run_app <- function() {
  addResourcePath("www", system.file("www", package = "inSilicoPCR"))
  shiny::shinyApp(ui = ui, server = server)
}
