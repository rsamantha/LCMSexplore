#' shiny based xcms demo
#'
#' Depending on the argument, run a web based interactive app to demonstrate the use of xcms
#'
#' @param example the name of the app
#'
#' @return a standard shiny app
#' @export
#'
#' @examples
#' ## show he list of all the available demos
#' shinyExample()
#'
shinyExample <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "LCMSexplore"))

  if(missing(example)){
    validExamplesMsg <-
      paste0(
        "Valid examples are: '",
        paste(validExamples, collapse = "', '"),
        "'")
    return(validExamplesMsg)
  }

  # if an invalid example is given, throw an error
  if (!nzchar(example) || !example %in% validExamples) {
    stop(
      'Please run `shinyExample()` passing a valid example app as argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "LCMSexplore")
  shiny::runApp(appDir, display.mode = "normal")
}
