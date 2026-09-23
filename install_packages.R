packages <- c("shiny", "readxl", "ggplot2", "bslib", "DT", "leaflet", "sf")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Setup complete. Run: Rscript run_tool.R")
