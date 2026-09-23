if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Run install_packages.R before starting the tool.", call. = FALSE)
}
shiny::runApp(".", launch.browser = TRUE)
