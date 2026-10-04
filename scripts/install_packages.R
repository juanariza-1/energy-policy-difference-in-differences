# Install dependencies explicitly before running an analysis.
required_packages <- c(
  "data.table", "dplyr", "tidyr", "ggplot2", "fixest", "broom",
  "stringr", "scales", "readxl", "readr", "haven", "modelsummary"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All project dependencies are already installed.")
}

