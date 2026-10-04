# ==============================================================================
# MASTER SCRIPT — Renewable Energy and the 2022 Energy Price Shock
#
# "Renewable Energy Capacity and the 2022 Energy Price Shock:
#  Heterogeneous Effects on U.S. Household Energy Expenditure"
#
# Authors: Juan Pablo Ariza Gallo & Juan Esteban Londoño
# Aix-Marseille University
#
# Data:    U.S. Consumer Expenditure Survey (CES), 2019Q1–2024Q1
# Method:  Difference-in-Differences with continuous treatment intensity
# ==============================================================================

rm(list = ls())
gc()

# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

# Run from the repository root, or set ENERGY_DID_ROOT.
project_root <- normalizePath(Sys.getenv("ENERGY_DID_ROOT", unset = "."), winslash = "/", mustWork = TRUE)
BASE_DIR <- project_root

# Derived paths
FMLI_DIR   <- file.path(BASE_DIR, "data", "raw", "FMLI")
RENEW_FILE <- file.path(BASE_DIR, "data", "raw",
                         "Renewable energy production across U.S. states.xlsx")
OUTPUT_DIR <- file.path(BASE_DIR, "generated", "master_script", "figures")

# Create output directory if it doesn't exist
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. PACKAGES
# ==============================================================================

required_packages <- c(
  "dplyr", "tidyr", "readxl", "readr", "haven",
  "fixest", "ggplot2", "modelsummary"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       ". Run Rscript scripts/install_packages.R first.", call. = FALSE)
}

library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(haven)
library(fixest)
library(ggplot2)
library(modelsummary)

cat("============================================================\n")
cat("  MASTER SCRIPT — DiD: Renewable Energy & 2022 Price Shock  \n")
cat("============================================================\n\n")

# ==============================================================================
# 2. DATA LOADING — FMLI (2019Q1 – 2024Q1)
# ==============================================================================

cat("SECTION 1: DATA LOADING\n")
cat("------------------------------------------------------------\n\n")

# Variables to retain across all files
keep_cols <- c(
  "NEWID", "CUID", "QINTRVYR", "QINTRVMO", "FINLWT21",
  "FINCBTAX", "FSALARYX",
  "FAM_SIZE", "AGE_REF", "SEX_REF", "REF_RACE", "HISP_REF",
  "CUTENURE", "ROOMSQ", "BEDROOMQ",
  "REGION", "STATE",
  "ELCTRCPQ", "NTLGASPQ", "FULOILPQ", "UTILPQ",
  "TOTEXPPQ", "HOUSPQ"
)

# Numeric fields that may arrive as character/labelled from SAS
num_force <- c(
  "QINTRVYR", "QINTRVMO", "STATE", "FINCBTAX", "FINLWT21", "FSALARYX",
  "FAM_SIZE", "AGE_REF", "SEX_REF", "REF_RACE", "HISP_REF",
  "CUTENURE", "ROOMSQ", "BEDROOMQ", "REGION",
  "ELCTRCPQ", "NTLGASPQ", "FULOILPQ", "UTILPQ",
  "TOTEXPPQ", "HOUSPQ", "URBAN"
)

# --- Helper: standardise one FMLI file ---
read_fmli <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df  <- if (ext == "sas7bdat") read_sas(path) else read.csv(path, colClasses = c(NEWID = "character"))

  # Harmonise URBAN / BLS_URBN
  if ("BLS_URBN" %in% names(df) & !"URBAN" %in% names(df)) df$URBAN <- df$BLS_URBN

  cols <- intersect(c(keep_cols, "URBAN"), names(df))
  df   <- df[, unique(cols)]

  # Survey identifiers must have a common type across SAS and CSV files.
  for (col in intersect(c("NEWID", "CUID"), names(df))) {
    df[[col]] <- as.character(df[[col]])
  }

  # Force numeric on key variables
  for (col in intersect(num_force, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  }
  df
}

# --- 2a. SAS files: 2019Q1 – 2021Q1 ---
sas_files <- list.files(
  FMLI_DIR,
  pattern = "^fmli(19|20|21)[0-9x]*\\.sas7bdat$",
  full.names = TRUE
)
if (!length(sas_files)) {
  stop("Raw FMLI files are not present. Follow data/raw/README.md before running this optional script.", call. = FALSE)
}
cat("SAS FMLI files found:", length(sas_files), "\n")

fmli_sas <- bind_rows(lapply(sas_files, function(f) {
  cat("  Reading:", basename(f), "\n")
  read_fmli(f)
}))

# Keep only 2019+ (drop 2017-2018 files that match the pattern)
fmli_sas <- fmli_sas %>% filter(QINTRVYR >= 2019)
cat("SAS observations (2019+):", nrow(fmli_sas), "\n\n")

# --- 2b. CSV files: 2021Q2 – 2024Q1 ---
csv_files <- list.files(
  FMLI_DIR,
  pattern = "^fmli2[1-4][0-9]\\.csv$",
  full.names = TRUE
)
cat("CSV FMLI files found:", length(csv_files), "\n")

fmli_csv <- bind_rows(lapply(csv_files, function(f) {
  cat("  Reading:", basename(f), "\n")
  read_fmli(f)
}))
cat("CSV observations:", nrow(fmli_csv), "\n\n")

# --- 2c. Combine into one panel ---
fmli <- bind_rows(fmli_sas, fmli_csv)
rm(fmli_sas, fmli_csv)

# Construct time identifiers
fmli <- fmli %>%
  mutate(
    year    = as.integer(QINTRVYR),
    month   = as.integer(QINTRVMO),
    quarter = ceiling(month / 3),
    yq      = paste0(year, "Q", quarter)
  )

# Drop missing STATE
fmli <- fmli %>% filter(!is.na(STATE))
fmli$STATE <- as.integer(fmli$STATE)

cat("Total FMLI panel:", nrow(fmli), "observations across",
    n_distinct(fmli$STATE), "state codes\n")
cat("\nTime coverage (year × quarter):\n")
print(table(fmli$year, fmli$quarter))

# ==============================================================================
# 3. OUTCOME VARIABLE CONSTRUCTION
# ==============================================================================

cat("\n\nSECTION 2: OUTCOME VARIABLE\n")
cat("------------------------------------------------------------\n\n")

# Fill missing expenditures with 0 (not interviewed for that component)
fmli <- fmli %>%
  mutate(
    ELCTRCPQ = replace_na(ELCTRCPQ, 0),
    NTLGASPQ = replace_na(NTLGASPQ, 0),
    FULOILPQ = replace_na(FULOILPQ, 0),
    UTILPQ   = replace_na(UTILPQ, 0)
  )

# Primary outcome: total energy expenditure (electricity + gas + fuel oil)
fmli$energy_exp <- fmli$ELCTRCPQ + fmli$NTLGASPQ + fmli$FULOILPQ

cat("Raw energy expenditure:\n")
print(summary(fmli$energy_exp))

# Drop zero / negative (included-in-rent or data errors)
fmli <- fmli %>% filter(energy_exp > 0)
cat("\nAfter dropping zero/negative:", nrow(fmli), "observations\n")

# Winsorize at 1st and 99th percentiles
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

fmli <- fmli %>%
  mutate(
    energy_exp = winsorize(energy_exp),
    ELCTRCPQ   = winsorize(ELCTRCPQ),
    NTLGASPQ   = winsorize(NTLGASPQ),
    FULOILPQ   = winsorize(FULOILPQ),
    UTILPQ     = winsorize(UTILPQ)
  )

# Log transformations
fmli <- fmli %>%
  mutate(
    ln_energy = log(energy_exp),
    ln_elec   = log(pmax(ELCTRCPQ, 0.01)),
    ln_gas    = log(pmax(NTLGASPQ, 0.01)),
    ln_util   = log(pmax(UTILPQ, 0.01))
  )

cat("\nLog energy expenditure:\n")
print(summary(fmli$ln_energy))

# ==============================================================================
# 4. TREATMENT VARIABLE — STATE-LEVEL RENEWABLE ENERGY INTENSITY
# ==============================================================================

cat("\n\nSECTION 3: TREATMENT VARIABLE\n")
cat("------------------------------------------------------------\n\n")

# Load state-level renewable energy data
renew_raw <- read_excel(RENEW_FILE)
# Support the original workbook, whose mean column is named x.
if (!"Mean" %in% names(renew_raw) && "x" %in% names(renew_raw)) {
  names(renew_raw)[names(renew_raw) == "x"] <- "Mean"
}

renew <- renew_raw %>%
  rename(
    STATE      = `State Code`,
    population = Population,
    mean_val   = Mean
  ) %>%
  mutate(
    STATE          = as.integer(STATE),
    mean_per_capita = mean_val / population
  ) %>%
  select(STATE, population, mean_val, mean_per_capita)

cat("Renewable energy data:", nrow(renew), "states\n")

# Merge into FMLI
fmli <- fmli %>%
  left_join(renew, by = "STATE") %>%
  filter(!is.na(mean_per_capita))

cat("After merge (dropping unmatched states):", nrow(fmli), "obs across",
    n_distinct(fmli$STATE), "states\n")

# (A) Continuous treatment: standardized log(renewable spending per capita)
fmli$ln_renew_pc     <- log(fmli$mean_per_capita)
fmli$treat_continuous <- as.numeric(scale(fmli$ln_renew_pc))

# (B) Binary treatment: above/below median
med_renew         <- median(unique(fmli$mean_per_capita))
fmli$high_renew   <- as.integer(fmli$mean_per_capita > med_renew)

# Post indicator: >= 2022
fmli$post <- as.integer(fmli$year >= 2022)

# DiD interactions
fmli$post_x_treat <- fmli$post * fmli$treat_continuous
fmli$post_x_high  <- fmli$post * fmli$high_renew

cat("\nTreatment summary:\n")
cat("  Continuous (std.):", round(range(fmli$treat_continuous), 2), "\n")
cat("  Binary split — High:", sum(fmli$high_renew == 1),
    "| Low:", sum(fmli$high_renew == 0), "\n")
cat("  Pre-period:", sum(fmli$post == 0), "| Post-period:", sum(fmli$post == 1), "\n")

# ==============================================================================
# 5. CONTROL VARIABLES
# ==============================================================================

cat("\n\nSECTION 4: CONTROL VARIABLES\n")
cat("------------------------------------------------------------\n\n")

fmli <- fmli %>%
  mutate(
    ln_income  = log(pmax(replace_na(FINCBTAX, 0), 1)),
    fam_size   = replace_na(FAM_SIZE, median(FAM_SIZE, na.rm = TRUE)),
    rooms      = replace_na(ROOMSQ, median(ROOMSQ, na.rm = TRUE)),
    age        = replace_na(AGE_REF, median(AGE_REF, na.rm = TRUE)),
    age_sq     = age^2,
    owner      = as.integer(CUTENURE %in% c(1, 2)),
    urban      = as.integer(URBAN == 1),
    female     = as.integer(SEX_REF == 2),
    hispanic   = as.integer(HISP_REF == 1),
    race_black = as.integer(REF_RACE == 2),
    race_other = as.integer(REF_RACE %in% c(3, 4, 5, 6))
  )

# Fixed-effect factors
fmli$state_f  <- factor(fmli$STATE)
fmli$yq_f     <- factor(fmli$yq)
fmli$region_f <- factor(as.integer(fmli$REGION))

# Relative time variables (for event studies)
fmli$rel_year    <- fmli$year - 2022
fmli$rel_quarter <- (fmli$year - 2022) * 4 + fmli$quarter

# Income quartile (for heterogeneity analysis)
fmli$inc_quartile <- ntile(fmli$ln_income, 4)
fmli$inc_q4       <- as.integer(fmli$inc_quartile == 4)

# Energy burden: energy > 10% of income
fmli$energy_share  <- fmli$energy_exp / pmax(exp(fmli$ln_income), 100)
fmli$energy_share  <- pmin(fmli$energy_share,
                            quantile(fmli$energy_share, 0.99, na.rm = TRUE))
fmli$energy_burden <- as.integer(fmli$energy_share > 0.10)

cat("Final analysis sample:", nrow(fmli), "observations\n")
cat("Years:", sort(unique(fmli$year)), "\n")
cat("Year-quarters:", length(unique(fmli$yq)), "\n")
cat("Income Q4:", sum(fmli$inc_q4), "obs\n")
cat("Energy burden rate:", round(mean(fmli$energy_burden, na.rm = TRUE) * 100, 1), "%\n")

# ==============================================================================
# 6. DESCRIPTIVE STATISTICS
# ==============================================================================

cat("\n\nSECTION 5: DESCRIPTIVE STATISTICS\n")
cat("------------------------------------------------------------\n\n")

desc_vars <- fmli %>%
  summarise(
    across(
      c(energy_exp, ln_energy, ELCTRCPQ, NTLGASPQ, FULOILPQ,
        treat_continuous, post,
        ln_income, fam_size, rooms, age, owner, urban, female,
        hispanic, race_black),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd   = ~sd(., na.rm = TRUE),
        min  = ~min(., na.rm = TRUE),
        med  = ~median(., na.rm = TRUE),
        max  = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  )

# Print key statistics
cat("Observations:", nrow(fmli), "\n")
cat("States:", n_distinct(fmli$STATE), "\n\n")

cat(sprintf("Energy expenditure: mean = $%.0f, sd = $%.0f, med = $%.0f\n",
            desc_vars$energy_exp__mean, desc_vars$energy_exp__sd,
            desc_vars$energy_exp__med))
cat(sprintf("  Electricity:      mean = $%.0f\n", desc_vars$ELCTRCPQ__mean))
cat(sprintf("  Natural gas:      mean = $%.0f\n", desc_vars$NTLGASPQ__mean))
cat(sprintf("  Fuel oil:         mean = $%.0f\n", desc_vars$FULOILPQ__mean))
cat(sprintf("Log income:         mean = %.2f\n", desc_vars$ln_income__mean))
cat(sprintf("Family size:        mean = %.2f\n", desc_vars$fam_size__mean))
cat(sprintf("Rooms:              mean = %.2f\n", desc_vars$rooms__mean))
cat(sprintf("Age:                mean = %.1f\n", desc_vars$age__mean))
cat(sprintf("Homeowner:          %.1f%%\n", desc_vars$owner__mean * 100))
cat(sprintf("Urban:              %.1f%%\n", desc_vars$urban__mean * 100))

# Pre vs. post comparison
cat("\nPre vs. Post mean energy expenditure:\n")
fmli %>%
  group_by(post) %>%
  summarise(
    n          = n(),
    mean_exp   = mean(energy_exp, na.rm = TRUE),
    mean_elec  = mean(ELCTRCPQ, na.rm = TRUE),
    mean_gas   = mean(NTLGASPQ, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  print()

# By treatment group
cat("\nBy treatment group (binary):\n")
fmli %>%
  group_by(high_renew, post) %>%
  summarise(
    n        = n(),
    mean_exp = mean(energy_exp, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  print()

# ==============================================================================
# 7. FIGURES — DESCRIPTIVE
# ==============================================================================

cat("\n\nSECTION 6: DESCRIPTIVE FIGURES\n")
cat("------------------------------------------------------------\n\n")

theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank()))

yq_levels <- fmli %>%
  distinct(year, quarter, yq) %>%
  arrange(year, quarter) %>%
  pull(yq)

fmli$yq_ordered <- factor(fmli$yq, levels = yq_levels, ordered = TRUE)

# --- FIG 1: Parallel Trends (binary groups) ---
trends <- fmli %>%
  group_by(yq_ordered, high_renew) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    se_energy   = sd(energy_exp, na.rm = TRUE) / sqrt(n()),
    .groups     = "drop"
  ) %>%
  mutate(group = factor(high_renew, labels = c("Low Renewable", "High Renewable")))

g1 <- ggplot(trends, aes(x = yq_ordered, y = mean_energy,
                          color = group, group = group)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_energy - 1.96 * se_energy,
                  ymax = mean_energy + 1.96 * se_energy, fill = group),
              alpha = 0.12, color = NA) +
  geom_vline(xintercept = which(yq_levels == "2022Q1"), linetype = "dashed") +
  scale_color_manual(values = c("#2196F3", "#F44336")) +
  scale_fill_manual(values = c("#2196F3", "#F44336")) +
  labs(title = "Parallel Trends: Energy Expenditure (2019-2024)",
       x = "Year-Quarter", y = "Mean Energy Expenditure ($)",
       color = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig1_parallel_trends.png"),
       g1, width = 12, height = 6, dpi = 300)
cat("Saved: fig1_parallel_trends.png\n")

# --- FIG 2: Trends by Treatment Tercile ---
fmli$treat_tercile <- cut(
  fmli$mean_per_capita,
  breaks = quantile(fmli$mean_per_capita, c(0, 1/3, 2/3, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

trends_terc <- fmli %>%
  group_by(yq_ordered, treat_tercile) %>%
  summarise(mean_energy = mean(energy_exp, na.rm = TRUE), .groups = "drop")

g2 <- ggplot(trends_terc, aes(x = yq_ordered, y = mean_energy,
                               color = treat_tercile, group = treat_tercile)) +
  geom_point(size = 1.5) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = which(yq_levels == "2022Q1"), linetype = "dashed") +
  scale_color_manual(values = c("#4CAF50", "#FF9800", "#F44336")) +
  labs(title = "Energy Expenditure by Renewable Intensity Tercile (2019-2024)",
       x = "Year-Quarter", y = "Mean Expenditure ($)",
       color = "Renewable\nIntensity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig2_trends_tercile.png"),
       g2, width = 12, height = 6, dpi = 300)
cat("Saved: fig2_trends_tercile.png\n")

# --- FIG 3: Distribution of Log Energy Expenditure ---
g3 <- ggplot(fmli, aes(x = ln_energy,
                         fill = factor(post, labels = c("Pre-2022", "Post-2022")))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~factor(high_renew, labels = c("Low Renewable", "High Renewable"))) +
  scale_fill_manual(values = c("#2196F3", "#F44336")) +
  labs(title = "Distribution of Log Energy Expenditure",
       x = "Log Energy Expenditure", y = "Density", fill = NULL) +
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig3_distributions.png"),
       g3, width = 12, height = 5, dpi = 300)
cat("Saved: fig3_distributions.png\n")

# --- FIG 4: Component Trends ---
comp_long <- fmli %>%
  select(yq_ordered, high_renew, ELCTRCPQ, NTLGASPQ, UTILPQ) %>%
  pivot_longer(cols = c(ELCTRCPQ, NTLGASPQ, UTILPQ),
               names_to = "component", values_to = "expenditure") %>%
  mutate(component = recode(component,
                             ELCTRCPQ = "Electricity",
                             NTLGASPQ = "Natural Gas",
                             UTILPQ   = "Utilities")) %>%
  group_by(yq_ordered, high_renew, component) %>%
  summarise(mean_exp = mean(expenditure, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = factor(high_renew, labels = c("Low Renew", "High Renew")))

g4 <- ggplot(comp_long, aes(x = yq_ordered, y = mean_exp,
                              color = group, group = group)) +
  geom_point(size = 1.2) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = which(yq_levels == "2022Q1"), linetype = "dashed") +
  facet_wrap(~component, scales = "free_y") +
  scale_color_manual(values = c("#2196F3", "#F44336")) +
  labs(title = "Component Trends (2019-2024)",
       x = "Year-Quarter", y = "Mean Expenditure ($)", color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig4_component_trends.png"),
       g4, width = 16, height = 5, dpi = 300)
cat("Saved: fig4_component_trends.png\n")

# ==============================================================================
# 8. MAIN DiD REGRESSIONS
# ==============================================================================

cat("\n\nSECTION 7: MAIN DiD REGRESSIONS\n")
cat("------------------------------------------------------------\n\n")

controls_formula <- "ln_income + fam_size + rooms + age + age_sq +
  owner + urban + female + hispanic + race_black + race_other"

# (1) Binary DiD
m1 <- feols(
  as.formula(paste0("ln_energy ~ post_x_high + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# (2) Continuous DiD — BASELINE
m2 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# (3) Weighted continuous DiD
m3 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f, weights = ~FINLWT21
)

# (4) Region FE (identifies treatment level)
m4 <- feols(
  as.formula(paste0("ln_energy ~ treat_continuous + post_x_treat + ",
                     controls_formula, " | region_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# (5) Electricity only
m5 <- feols(
  as.formula(paste0("ln_elec ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# (6) Dollar levels
m6 <- feols(
  as.formula(paste0("energy_exp ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

cat("TABLE 1: Main DiD Estimates\n\n")
etable(m1, m2, m3, m4, m5, m6,
       title = "Table 1: DiD Estimates - Log(Energy Expenditure), 2019-2024",
       headers = c("Binary", "Continuous", "Weighted", "Region FE",
                    "Electricity", "Levels ($)"),
       keep = c("post_x_high", "post_x_treat", "treat_continuous"),
       se.below = TRUE,
       fitstat = c("n", "r2"),
       notes = "SE clustered at state level. All models include controls.")

# ==============================================================================
# 9. HETEROGENEITY BY INCOME QUARTILE
# ==============================================================================

cat("\n\nSECTION 8: HETEROGENEITY BY INCOME QUARTILE\n")
cat("------------------------------------------------------------\n\n")

m_q1 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + fam_size + rooms + age + age_sq +
    owner + urban + female + hispanic + race_black + race_other | state_f + yq_f")),
  data = fmli %>% filter(inc_quartile == 1), cluster = ~state_f
)

m_q2 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_quartile == 2), cluster = ~state_f
)

m_q3 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_quartile == 3), cluster = ~state_f
)

m_q4 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_quartile == 4), cluster = ~state_f
)

cat("TABLE 2: Heterogeneity by Income Quartile\n\n")
etable(m_q1, m_q2, m_q3, m_q4,
       title = "Table 2: Heterogeneous Treatment Effects by Income Quartile",
       headers = c("Q1 (Low)", "Q2", "Q3", "Q4 (High)"),
       keep = "post_x_treat",
       se.below = TRUE,
       fitstat = c("n", "r2"))

# Full sample on high-income only — PREFERRED SPECIFICATION 1
m_high_inc <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_q4 == 1), cluster = ~state_f
)

cat("\nPreferred Specification 1 (High Income Q4):\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_high_inc)["post_x_treat"],
            se(m_high_inc)["post_x_treat"],
            pvalue(m_high_inc)["post_x_treat"]))

# ==============================================================================
# 10. ENERGY BURDEN — PREFERRED SPECIFICATION 2
# ==============================================================================

cat("\n\nSECTION 9: ENERGY BURDEN MODEL\n")
cat("------------------------------------------------------------\n\n")

m_burden <- feols(
  as.formula(paste0("energy_burden ~ post_x_treat + ", controls_formula,
                     " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

cat("TABLE 3: Energy Burden DiD\n\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_burden)["post_x_treat"],
            se(m_burden)["post_x_treat"],
            pvalue(m_burden)["post_x_treat"]))

# ==============================================================================
# 11. SUB-COMPONENT ANALYSIS
# ==============================================================================

cat("\n\nSECTION 10: SUB-COMPONENT ANALYSIS\n")
cat("------------------------------------------------------------\n\n")

m_elec <- feols(
  as.formula(paste0("ln_elec ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

m_gas <- feols(
  as.formula(paste0("ln_gas ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(NTLGASPQ > 0), cluster = ~state_f
)

m_util <- feols(
  as.formula(paste0("ln_util ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

cat("TABLE 4: DiD by Energy Component\n\n")
etable(m2, m_elec, m_gas, m_util,
       title = "Table 4: DiD by Energy Component (Continuous Treatment)",
       headers = c("Total Energy", "Electricity", "Nat. Gas", "Utilities"),
       keep = "post_x_treat",
       se.below = TRUE,
       fitstat = c("n", "r2"))

# ==============================================================================
# 12. EVENT STUDIES
# ==============================================================================

cat("\n\nSECTION 11: EVENT STUDIES\n")
cat("------------------------------------------------------------\n\n")

# --- 12a. Annual Event Study — Full Sample (ref = 2021) ---
m_event <- feols(
  as.formula(paste0("ln_energy ~ i(rel_year, treat_continuous, ref = -1) + ",
                     controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

cat("Annual Event Study — Full Sample:\n")
summary(m_event)

# --- 12b. Annual Event Study — High Income Q4 ---
m_ev_hi <- feols(
  as.formula(paste0("ln_energy ~ i(rel_year, treat_continuous, ref = -1) + ",
                     controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_q4 == 1), cluster = ~state_f
)

cat("\nAnnual Event Study — High Income (Q4):\n")
summary(m_ev_hi)

# --- 12c. Annual Event Study — Energy Burden ---
m_ev_burd <- feols(
  as.formula(paste0("energy_burden ~ i(rel_year, treat_continuous, ref = -1) + ",
                     controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

cat("\nAnnual Event Study — Energy Burden:\n")
summary(m_ev_burd)

# --- 12d. Quarterly Event Study — Full Sample (ref = 2021Q4) ---
m_event_q <- feols(
  as.formula(paste0("ln_energy ~ i(rel_quarter, treat_continuous, ref = -1) + ",
                     controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# ==============================================================================
# 13. PRE-TREND TESTS
# ==============================================================================

cat("\n\nSECTION 12: PRE-TREND TESTS\n")
cat("------------------------------------------------------------\n\n")

# Full sample — annual
pre_annual <- grep("rel_year::-[2-9]", names(coef(m_event)), value = TRUE)
if (length(pre_annual) > 0) {
  cat("Full sample annual pre-trend test:\n")
  print(wald(m_event, pre_annual))
}

# High income — annual
pre_hi <- grep("rel_year::-[2-9]", names(coef(m_ev_hi)), value = TRUE)
if (length(pre_hi) > 0) {
  cat("\nHigh income Q4 annual pre-trend test:\n")
  print(wald(m_ev_hi, pre_hi))
}

# Energy burden — annual
pre_burd <- grep("rel_year::-[2-9]", names(coef(m_ev_burd)), value = TRUE)
if (length(pre_burd) > 0) {
  cat("\nEnergy burden annual pre-trend test:\n")
  print(wald(m_ev_burd, pre_burd))
}

# Full sample — quarterly
pre_q <- grep("rel_quarter::-", names(coef(m_event_q)), value = TRUE)
if (length(pre_q) > 0) {
  cat("\nQuarterly pre-trend test (full sample):\n")
  print(wald(m_event_q, pre_q))
}

# ==============================================================================
# 14. ROBUSTNESS CHECKS
# ==============================================================================

cat("\n\nSECTION 13: ROBUSTNESS CHECKS\n")
cat("------------------------------------------------------------\n\n")

# (A) Alternative post cutoff: 2022Q2
fmli$post_q2          <- as.integer(fmli$year > 2022 | (fmli$year == 2022 & fmli$quarter >= 2))
fmli$post_q2_x_treat  <- fmli$post_q2 * fmli$treat_continuous

m_rob1 <- feols(
  as.formula(paste0("ln_energy ~ post_q2_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli, cluster = ~state_f
)

# (B) Exclude 2020 (COVID)
m_rob2 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(year != 2020), cluster = ~state_f
)

# (C) Trim extreme renewable states (10th–90th percentile)
p10 <- quantile(fmli$mean_per_capita, 0.10)
p90 <- quantile(fmli$mean_per_capita, 0.90)

m_rob3 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(mean_per_capita > p10 & mean_per_capita < p90),
  cluster = ~state_f
)

# (D) Homeowners only
m_rob4 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ln_income + fam_size + rooms +
    age + age_sq + urban + female + hispanic + race_black + race_other | state_f + yq_f")),
  data = fmli %>% filter(owner == 1), cluster = ~state_f
)

# (E) Renters only
m_rob5 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ln_income + fam_size + rooms +
    age + age_sq + urban + female + hispanic + race_black + race_other | state_f + yq_f")),
  data = fmli %>% filter(owner == 0), cluster = ~state_f
)

# (F) Urban only
m_rob6 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ln_income + fam_size + rooms +
    age + age_sq + owner + female + hispanic + race_black + race_other | state_f + yq_f")),
  data = fmli %>% filter(urban == 1), cluster = ~state_f
)

# (G) High income Q4 excluding 2020
m_rob7 <- feols(
  as.formula(paste0("ln_energy ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(inc_q4 == 1, year != 2020), cluster = ~state_f
)

# (H) Energy burden excluding 2020
m_rob8 <- feols(
  as.formula(paste0("energy_burden ~ post_x_treat + ", controls_formula, " | state_f + yq_f")),
  data = fmli %>% filter(year != 2020), cluster = ~state_f
)

cat("TABLE 5: Robustness Checks — Full Sample\n\n")
etable(m2, m_rob1, m_rob2, m_rob3, m_rob4, m_rob5, m_rob6,
       title = "Table 5: Robustness Checks",
       headers = c("Baseline", "Post=Q2", "No 2020", "Trim States",
                    "Owners", "Renters", "Urban"),
       keep = c("post_x_treat", "post_q2_x_treat"),
       se.below = TRUE,
       fitstat = c("n", "r2"))

cat("\nTABLE 6: Robustness — Preferred Specifications excl. COVID\n\n")
etable(m_high_inc, m_rob7, m_burden, m_rob8,
       title = "Table 6: Robustness - Excluding 2020",
       headers = c("High Inc", "High Inc no2020", "Burden", "Burden no2020"),
       keep = "post_x_treat",
       se.below = TRUE,
       fitstat = c("n", "r2"))

# ==============================================================================
# 15. FINAL FIGURES — EVENT STUDIES AND HETEROGENEITY
# ==============================================================================

cat("\n\nSECTION 14: FINAL FIGURES\n")
cat("------------------------------------------------------------\n\n")

# --- FIG 5: Annual Event Study — Full Sample ---
png(file.path(OUTPUT_DIR, "fig5_event_study_annual.png"),
    width = 10, height = 6, units = "in", res = 300)
iplot(m_event,
      main = "Annual Event Study: Continuous Treatment (ref = 2021)",
      xlab = "Years Relative to 2022",
      ylab = "DiD Coefficient",
      col = "#1565C0", ci_col = "#90CAF9")
abline(h = 0, lty = 2)
abline(v = -0.5, lty = 2, col = "red")
dev.off()
cat("Saved: fig5_event_study_annual.png\n")

# --- FIG 6: Quarterly Event Study ---
png(file.path(OUTPUT_DIR, "fig6_event_study_quarterly.png"),
    width = 14, height = 6, units = "in", res = 300)
iplot(m_event_q,
      main = "Quarterly Event Study: Continuous Treatment (ref = 2021Q4)",
      xlab = "Quarters Relative to 2022Q1",
      ylab = "DiD Coefficient",
      col = "#1565C0", ci_col = "#90CAF9")
abline(h = 0, lty = 2)
abline(v = -0.5, lty = 2, col = "red")
dev.off()
cat("Saved: fig6_event_study_quarterly.png\n")

# --- FIG 7: Event Study — High Income Q4 ---
png(file.path(OUTPUT_DIR, "fig7_event_study_high_income.png"),
    width = 10, height = 6, units = "in", res = 300)
iplot(m_ev_hi,
      main = "Event Study: High-Income Households (Q4) - ref = 2021",
      xlab = "Years Relative to 2022",
      ylab = "DiD Coefficient",
      col = "#D32F2F", ci_col = "#FFCDD2")
abline(h = 0, lty = 2)
abline(v = -0.5, lty = 2, col = "black")
dev.off()
cat("Saved: fig7_event_study_high_income.png\n")

# --- FIG 8: Event Study — Energy Burden ---
png(file.path(OUTPUT_DIR, "fig8_event_study_burden.png"),
    width = 10, height = 6, units = "in", res = 300)
iplot(m_ev_burd,
      main = "Event Study: P(Energy Burden > 10% of Income) - ref = 2021",
      xlab = "Years Relative to 2022",
      ylab = "Change in P(Energy Burden)",
      col = "#1565C0", ci_col = "#BBDEFB")
abline(h = 0, lty = 2)
abline(v = -0.5, lty = 2, col = "black")
dev.off()
cat("Saved: fig8_event_study_burden.png\n")

# --- FIG 9: Heterogeneity by Income Quartile ---
het_df <- data.frame(
  quartile = c("Q1\n(Low)", "Q2", "Q3", "Q4\n(High)"),
  coef = c(coef(m_q1)["post_x_treat"], coef(m_q2)["post_x_treat"],
           coef(m_q3)["post_x_treat"], coef(m_q4)["post_x_treat"]),
  se   = c(se(m_q1)["post_x_treat"], se(m_q2)["post_x_treat"],
           se(m_q3)["post_x_treat"], se(m_q4)["post_x_treat"]),
  pval = c(pvalue(m_q1)["post_x_treat"], pvalue(m_q2)["post_x_treat"],
           pvalue(m_q3)["post_x_treat"], pvalue(m_q4)["post_x_treat"])
)
het_df$quartile <- factor(het_df$quartile, levels = het_df$quartile)
het_df$sig <- ifelse(het_df$pval < 0.01, "p<0.01",
              ifelse(het_df$pval < 0.05, "p<0.05",
              ifelse(het_df$pval < 0.10, "p<0.10", "n.s.")))

g_het <- ggplot(het_df, aes(x = quartile, y = coef, fill = sig)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(aes(label = sprintf("p=%.3f", pval),
                y = coef - 1.96 * se - 0.005), size = 3.5) +
  scale_fill_manual(values = c("p<0.01" = "#D32F2F", "p<0.05" = "#FF5722",
                                "p<0.10" = "#FF9800", "n.s." = "#B0BEC5")) +
  labs(title = "Heterogeneous Treatment Effects by Income Quartile",
       subtitle = "DiD Coefficient: Post x Renewable Intensity (Continuous)",
       y = "Coefficient", x = "Income Quartile", fill = "Significance") +
  theme(legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig9_income_heterogeneity.png"),
       g_het, width = 9, height = 6, dpi = 300)
cat("Saved: fig9_income_heterogeneity.png\n")

# --- FIG 10: Parallel Trends — High-Income Households ---
fmli_hi <- fmli %>% filter(inc_q4 == 1)
fmli_hi$treat_group <- ifelse(fmli_hi$treat_continuous > 0,
                                "High Renewable", "Low Renewable")

trends_hi <- fmli_hi %>%
  group_by(yq_ordered, treat_group) %>%
  summarise(
    mean_e = mean(energy_exp, na.rm = TRUE),
    se_e   = sd(energy_exp, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

g_trends_hi <- ggplot(trends_hi, aes(x = yq_ordered, y = mean_e,
                                      color = treat_group, group = treat_group)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_e - 1.96 * se_e, ymax = mean_e + 1.96 * se_e,
                  fill = treat_group), alpha = 0.12, color = NA) +
  geom_vline(xintercept = which(yq_levels == "2022Q1"), linetype = "dashed") +
  scale_color_manual(values = c("#2196F3", "#F44336")) +
  scale_fill_manual(values = c("#2196F3", "#F44336")) +
  labs(title = "High-Income Households (Q4): Parallel Trends",
       x = "Year-Quarter", y = "Mean Energy Expenditure ($)",
       color = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig10_high_income_trends.png"),
       g_trends_hi, width = 12, height = 6, dpi = 300)
cat("Saved: fig10_high_income_trends.png\n")

# --- FIG 11: Energy Burden Rate Trends ---
fmli$treat_group <- ifelse(fmli$treat_continuous > 0,
                            "High Renewable", "Low Renewable")

burd_trends <- fmli %>%
  group_by(yq_ordered, treat_group) %>%
  summarise(burden_rate = mean(energy_burden, na.rm = TRUE) * 100,
            .groups = "drop")

g_burd <- ggplot(burd_trends, aes(x = yq_ordered, y = burden_rate,
                                    color = treat_group, group = treat_group)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = which(yq_levels == "2022Q1"), linetype = "dashed") +
  scale_color_manual(values = c("#2196F3", "#F44336")) +
  labs(title = "Energy Burden Rate: % of Households with Energy > 10% of Income",
       x = "Year-Quarter", y = "Energy Burden Rate (%)", color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "bottom")

ggsave(file.path(OUTPUT_DIR, "fig11_burden_trends.png"),
       g_burd, width = 12, height = 6, dpi = 300)
cat("Saved: fig11_burden_trends.png\n")

# ==============================================================================
# 16. SUMMARY OF ALL KEY RESULTS
# ==============================================================================

cat("\n\n============================================================\n")
cat("  SUMMARY OF KEY RESULTS\n")
cat("============================================================\n\n")

cat("--- Full Sample Baseline (Continuous DiD) ---\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m2)["post_x_treat"], se(m2)["post_x_treat"],
            pvalue(m2)["post_x_treat"]))

cat("\n--- Preferred Specification 1: High-Income Q4 ---\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_high_inc)["post_x_treat"], se(m_high_inc)["post_x_treat"],
            pvalue(m_high_inc)["post_x_treat"]))

cat("\n--- Preferred Specification 2: Energy Burden ---\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_burden)["post_x_treat"], se(m_burden)["post_x_treat"],
            pvalue(m_burden)["post_x_treat"]))

cat("\n--- Sub-Components ---\n")
cat(sprintf("  Electricity: beta = %.4f, p = %.4f\n",
            coef(m_elec)["post_x_treat"], pvalue(m_elec)["post_x_treat"]))
cat(sprintf("  Natural Gas: beta = %.4f, p = %.4f\n",
            coef(m_gas)["post_x_treat"], pvalue(m_gas)["post_x_treat"]))
cat(sprintf("  Utilities:   beta = %.4f, p = %.4f\n",
            coef(m_util)["post_x_treat"], pvalue(m_util)["post_x_treat"]))

cat("\n--- Robustness: High Income excl. 2020 ---\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_rob7)["post_x_treat"], se(m_rob7)["post_x_treat"],
            pvalue(m_rob7)["post_x_treat"]))

cat("\n--- Robustness: Energy Burden excl. 2020 ---\n")
cat(sprintf("  beta = %.4f, SE = %.4f, p = %.4f\n",
            coef(m_rob8)["post_x_treat"], se(m_rob8)["post_x_treat"],
            pvalue(m_rob8)["post_x_treat"]))

# ==============================================================================
# 17. SAVE WORKSPACE
# ==============================================================================

save.image(file.path(BASE_DIR, "generated", "master_script", "master_workspace.RData"))

cat("\n============================================================\n")
cat("  MASTER SCRIPT COMPLETE\n")
cat("  Workspace saved to generated/master_script/master_workspace.RData\n")
cat("  Figures saved to generated/master_script/figures/\n")
cat("============================================================\n")
