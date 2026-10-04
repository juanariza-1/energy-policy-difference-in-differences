# Fast input validation using base R; does not fit models or install packages.
project_root <- normalizePath(Sys.getenv("ENERGY_DID_ROOT", unset = "."), winslash = "/", mustWork = TRUE)
analysis_file <- file.path(project_root, "data", "processed", "analysis_dataset_v2.csv")
stopifnot(file.exists(analysis_file))
df <- read.csv(analysis_file, colClasses = c(NEWID = "character", CUID = "character", yq = "character"), check.names = FALSE)
stopifnot(
  nrow(df) == 88006L,
  ncol(df) == 38L,
  length(unique(df$STATE)) == 41L,
  length(unique(df$yq)) == 21L,
  all(df$treat_tercile %in% c("Low", "Medium", "High")),
  all(is.finite(df$ln_energy)),
  all(is.finite(df$ln_income)),
  all(is.finite(df$FINLWT21)),
  all(df$FINLWT21 > 0),
  all(df$energy_exp > 0),
  all(df$post %in% c(0, 1)),
  isTRUE(all.equal(df$post_x_treat, df$post * df$treat_continuous)),
  isTRUE(all.equal(df$post_x_high, df$post * df$high_renew))
)
cat("Input checks passed:", nrow(df), "observations,", ncol(df), "columns, 41 states, 21 quarters.\n")
cat("Tercile counts:\n")
print(table(df$treat_tercile))
for (script in c("FINAL_CODE.R", "did_unified_analysis.R", "master_script.R")) {
  parse(file.path(project_root, "scripts", script))
  cat("Syntax check passed:", script, "\n")
}
