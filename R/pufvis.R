# References
# https://github.com/gpilgrim2670/SwimMap/blob/master/app.R
# Exporting the results of a table
# https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table
# https://rshiny.blog/2023/04/05/load-data-to-shiny/
# https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app

library(shiny)

#' Open the shiny application
#'
#' @param crps Array or dataframe containing the crps
#' @param algorithms List of algorithms used to create the CPRs from the raw responses
#' @param maxRequestSize Maximum size in bytes of files to upload to the server
#'
#' @export
pufvis <- function(crps = NULL, algorithms = algorithms, maxRequestSize = NULL, ...) {
  if (is.data.frame(crps)) {
    crp_table <<- reactiveVal(crps_to_arr(crps))
  } else if (is.array(crps)) {
    crp_table <<- reactiveVal(crps)
  } else {
    crp_table <<- reactiveVal()
  }

  if (!is.null(maxRequestSize)) {
    options(shiny.maxRequestSize = maxRequestSize)
  }

  pkg_app_dir <- system.file("application", package = "pufvis")
  file.copy(pkg_app_dir, tempdir(), recursive = TRUE)
  app_dir <- paste0(tempdir(), "/application")
  on.exit(unlink(app_dir, recursive = TRUE))

  try(runApp(appDir = app_dir, ...))
}
