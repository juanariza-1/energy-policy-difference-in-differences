

# ======================================================================
# LOAD ANALYSIS DATASET V2 AND PREPARE ANALYSIS VARIABLES
# ======================================================================

cat("\n=== LOADING analysis_dataset_v2.csv ===\n")

project_root <- normalizePath(Sys.getenv("ENERGY_DID_ROOT", unset = "."), winslash = "/", mustWork = TRUE)
analysis_file <- file.path(project_root, "data", "processed", "analysis_dataset_v2.csv")
output_dir <- file.path(project_root, "generated", "did_unified_analysis", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(analysis_file)) {
  stop("File not found: ", analysis_file)
}

required_packages <- c(
  "data.table", "dplyr", "tidyr", "ggplot2", "fixest",
  "broom", "stringr", "scales"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       ". Run Rscript scripts/install_packages.R first.", call. = FALSE)
}
for (pkg in required_packages) library(pkg, character.only = TRUE)

df <- data.table::fread(analysis_file, data.table = FALSE)

cat("Dataset loaded successfully.\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# ----------------------------------------------------------------------
# Basic type cleaning
# ----------------------------------------------------------------------
char_vars <- c("NEWID", "CUID", "yq")
num_vars <- c(
  "ln_energy", "ln_elec", "ln_gas", "ln_util", "post", "high_renew",
  "treat_continuous", "post_x_high", "post_x_treat", "STATE", "year",
  "quarter", "rel_year", "rel_quarter", "FINLWT21", "REGION",
  "energy_exp", "ELCTRCPQ", "NTLGASPQ", "FULOILPQ", "UTILPQ",
  "mean_per_capita", "ln_renew_pc", "ln_income", "fam_size", "rooms",
  "age", "age_sq", "owner", "urban", "female", "hispanic",
  "race_black", "race_other", "treat_tercile"
)

# The supplied CSV stores terciles as labels; preserve their ordered meaning.
if ("treat_tercile" %in% names(df)) {
  tercile_labels <- as.character(df$treat_tercile)
  label_codes <- c(Low = 1, Medium = 2, High = 3)
  labeled <- !is.na(tercile_labels) & tercile_labels %in% names(label_codes)
  tercile_labels[labeled] <- as.character(label_codes[tercile_labels[labeled]])
  df$treat_tercile <- suppressWarnings(as.numeric(tercile_labels))
}

for (v in intersect(char_vars, names(df))) {
  df[[v]] <- as.character(df[[v]])
}

for (v in intersect(num_vars, names(df))) {
  df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
}

# ----------------------------------------------------------------------
# Derived variables needed by the old script
# ----------------------------------------------------------------------

# Keep aliases expected by later code
df <- df %>%
  mutate(
    state = STATE,
    year_quarter = yq,
    state_f = factor(STATE),
    yq_f = factor(yq),
    region_f = factor(REGION)
  )

# Binary treatment alias
if (!"treat_binary" %in% names(df)) {
  if ("high_renew" %in% names(df)) {
    df$treat_binary <- df$high_renew
  } else {
    med_treat <- median(df$treat_continuous, na.rm = TRUE)
    df$treat_binary <- ifelse(df$treat_continuous > med_treat, 1, 0)
  }
}

# Income quartiles from ln_income
if (!"inc_quartile" %in% names(df)) {
  qcuts <- quantile(df$ln_income, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  df$inc_quartile <- dplyr::case_when(
    is.na(df$ln_income) ~ NA_real_,
    df$ln_income <= qcuts[1] ~ 1,
    df$ln_income <= qcuts[2] ~ 2,
    df$ln_income <= qcuts[3] ~ 3,
    df$ln_income >  qcuts[3] ~ 4
  )
}

# Energy burden = energy expenditure / income level
if (!"energy_burden" %in% names(df)) {
  income_level <- exp(df$ln_income)
  income_level[!is.finite(income_level) | income_level <= 0] <- NA
  df$energy_burden <- df$energy_exp / income_level
}

# Renter indicator from owner
if (!"renter" %in% names(df)) {
  df$renter <- ifelse(is.na(df$owner), NA, ifelse(df$owner == 1, 0, 1))
}

# Alternative 75/25 treatment split based on state renewable intensity
if (!"treat_75_25" %in% names(df)) {
  p75 <- quantile(df$mean_per_capita, 0.75, na.rm = TRUE)
  df$treat_75_25 <- ifelse(df$mean_per_capita >= p75, 1, 0)
}

# Alternative post definition: 2022Q2 onward
if (!"post_2022q2" %in% names(df)) {
  df$post_2022q2 <- ifelse(df$year > 2022 | (df$year == 2022 & df$quarter >= 2), 1, 0)
}

# Alternative interaction term
if (!"post_x_75_25" %in% names(df)) {
  df$post_x_75_25 <- df$post_2022q2 * df$treat_75_25
}

# Safety: age_sq
if (!"age_sq" %in% names(df)) {
  df$age_sq <- df$age^2
}

# Safety checks
cat("\n=== PREPARATION CHECKS ===\n")
print(names(df))
cat("\nMissingness in key variables:\n")
print(colSums(is.na(df[, intersect(c(
  "ln_energy", "post", "treat_binary", "treat_continuous", "STATE",
  "year", "quarter", "yq", "ln_income", "energy_exp"
), names(df)), drop = FALSE])))

# ======================================================================
# SECTION 7: SECTION A - MAIN RESULTS TABLE
# ======================================================================

cat("\n=== SECTION A: MAIN RESULTS ===\n")

# Model 1: Binary DiD
m1 <- feols(ln_energy ~ post_x_high + post + treat_binary +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              state_f + yq_f,
            data = df, cluster = ~state_f)

# Model 2: Continuous DiD (MAIN)
m2 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              state_f + yq_f,
            data = df, cluster = ~state_f)

# Model 3: Weighted continuous DiD
m3 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              state_f + yq_f,
            data = df, weights = ~FINLWT21, cluster = ~state_f)

# Model 4: Region FE instead of state FE
m4 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              region_f + yq_f,
            data = df, cluster = ~state_f)

# Model 5: Electricity only
m5 <- feols(ln_elec ~ post_x_treat + post + treat_continuous +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              state_f + yq_f,
            data = df, cluster = ~state_f)

# Model 6: Levels (dollars)
m6 <- feols(energy_exp ~ post_x_treat + post + treat_continuous +
              ln_income + fam_size + rooms + age + age_sq +
              owner + urban + female + hispanic + race_black + race_other |
              state_f + yq_f,
            data = df, cluster = ~state_f)

cat("Main results models estimated.\n")
cat("\n=== TABLE: MAIN RESULTS ===\n")
print(etable(m1, m2, m3, m4, m5, m6))

# ======================================================================
# SECTION 8: SECTION B - HETEROGENEITY BY INCOME QUARTILE
# ======================================================================

cat("\n=== SECTION B: HETEROGENEITY BY INCOME QUARTILE ===\n")

m_q1 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                ln_income + fam_size + rooms + age + age_sq +
                owner + urban + female + hispanic + race_black + race_other |
                state_f + yq_f,
              data = df %>% filter(inc_quartile == 1), cluster = ~state_f)

m_q2 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                ln_income + fam_size + rooms + age + age_sq +
                owner + urban + female + hispanic + race_black + race_other |
                state_f + yq_f,
              data = df %>% filter(inc_quartile == 2), cluster = ~state_f)

m_q3 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                ln_income + fam_size + rooms + age + age_sq +
                owner + urban + female + hispanic + race_black + race_other |
                state_f + yq_f,
              data = df %>% filter(inc_quartile == 3), cluster = ~state_f)

m_q4 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                ln_income + fam_size + rooms + age + age_sq +
                owner + urban + female + hispanic + race_black + race_other |
                state_f + yq_f,
              data = df %>% filter(inc_quartile == 4), cluster = ~state_f)

cat("\n=== TABLE: INCOME QUARTILE HETEROGENEITY ===\n")
print(etable(m_q1, m_q2, m_q3, m_q4))

# ======================================================================
# SECTION 9: SECTION C - ENERGY BURDEN ANALYSIS
# ======================================================================

cat("\n=== SECTION C: ENERGY BURDEN ===\n")

m_burden <- feols(energy_burden ~ post_x_treat + post + treat_continuous +
                    ln_income + fam_size + rooms + age + age_sq +
                    owner + urban + female + hispanic + race_black + race_other |
                    state_f + yq_f,
                  data = df, cluster = ~state_f)

m_burd_no20 <- feols(energy_burden ~ post_x_treat + post + treat_continuous +
                       ln_income + fam_size + rooms + age + age_sq +
                       owner + urban + female + hispanic + race_black + race_other |
                       state_f + yq_f,
                     data = df %>% filter(year != 2020), cluster = ~state_f)

cat("\n=== TABLE: ENERGY BURDEN ===\n")
print(etable(m_burden, m_burd_no20))

# ======================================================================
# SECTION 10: SECTION D - EVENT STUDIES
# ======================================================================

cat("\n=== SECTION D: EVENT STUDIES ===\n")

es_annual <- feols(ln_energy ~ i(rel_year, treat_continuous, ref = -1) +
                     post + treat_continuous +
                     ln_income + fam_size + rooms + age + age_sq +
                     owner + urban + female + hispanic + race_black + race_other |
                     state_f + yq_f,
                   data = df, cluster = ~state_f)

cat("\nAnnual Event Study (Full Sample) Summary:\n")
print(summary(es_annual))

es_quarterly <- feols(ln_energy ~ i(rel_quarter, treat_continuous, ref = -1) +
                        post + treat_continuous +
                        ln_income + fam_size + rooms + age + age_sq +
                        owner + urban + female + hispanic + race_black + race_other |
                        state_f + yq_f,
                      data = df, cluster = ~state_f)

cat("\nQuarterly Event Study (Full Sample) Summary:\n")
print(summary(es_quarterly))

es_q4_annual <- feols(ln_energy ~ i(rel_year, treat_continuous, ref = -1) +
                        post + treat_continuous +
                        ln_income + fam_size + rooms + age + age_sq +
                        owner + urban + female + hispanic + race_black + race_other |
                        state_f + yq_f,
                      data = df %>% filter(inc_quartile == 4), cluster = ~state_f)

cat("\nAnnual Event Study (High Income Q4) Summary:\n")
print(summary(es_q4_annual))

es_burden <- feols(energy_burden ~ i(rel_year, treat_continuous, ref = -1) +
                     post + treat_continuous +
                     ln_income + fam_size + rooms + age + age_sq +
                     owner + urban + female + hispanic + race_black + race_other |
                     state_f + yq_f,
                   data = df, cluster = ~state_f)

cat("\nAnnual Event Study (Energy Burden) Summary:\n")
print(summary(es_burden))

# ======================================================================
# SECTION 11: SECTION E - PRE-TREND WALD TESTS
# ======================================================================

cat("\n=== SECTION E: PRE-TREND TESTS (WALD) ===\n")

pre_coefs_annual <- coef(es_annual)[grepl("rel_year::\\-", names(coef(es_annual)))]
cat("\nAnnual Event Study - Pre-period Wald test:\n")
if (length(pre_coefs_annual) > 0) {
  cat(sprintf("  Pre-trend coefficients: %s\n",
              paste(round(pre_coefs_annual, 4), collapse = ", ")))
}

pre_coefs_quarterly <- coef(es_quarterly)[grepl("rel_quarter::\\-", names(coef(es_quarterly)))]
cat("\nQuarterly Event Study - Pre-period Wald test:\n")
if (length(pre_coefs_quarterly) > 1) {
  cat(sprintf("  Number of pre-period quarters tested: %d\n", length(pre_coefs_quarterly)))
}

# ======================================================================
# SECTION 12: SECTION F - ROBUSTNESS CHECKS
# ======================================================================

cat("\n=== SECTION F: ROBUSTNESS CHECKS ===\n")

m_rob_2022q2 <- feols(ln_energy ~ post_x_75_25 + post_2022q2 + treat_75_25 +
                        ln_income + fam_size + rooms + age + age_sq +
                        owner + urban + female + hispanic + race_black + race_other |
                        state_f + yq_f,
                      data = df, cluster = ~state_f)

m_rob_no2020 <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                        ln_income + fam_size + rooms + age + age_sq +
                        owner + urban + female + hispanic + race_black + race_other |
                        state_f + yq_f,
                      data = df %>% filter(year != 2020), cluster = ~state_f)

treat_p10 <- quantile(df$mean_per_capita, 0.10, na.rm = TRUE)
treat_p90 <- quantile(df$mean_per_capita, 0.90, na.rm = TRUE)

m_rob_trim <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                      ln_income + fam_size + rooms + age + age_sq +
                      owner + urban + female + hispanic + race_black + race_other |
                      state_f + yq_f,
                    data = df %>% filter(mean_per_capita >= treat_p10,
                                         mean_per_capita <= treat_p90),
                    cluster = ~state_f)

m_rob_owners <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                        ln_income + fam_size + rooms + age + age_sq +
                        owner + urban + female + hispanic + race_black + race_other |
                        state_f + yq_f,
                      data = df %>% filter(owner == 1), cluster = ~state_f)

m_rob_renters <- feols(ln_energy ~ post_x_treat + post + treat_continuous +
                         ln_income + fam_size + rooms + age + age_sq +
                         owner + urban + female + hispanic + race_black + race_other |
                         state_f + yq_f,
                       data = df %>% filter(renter == 1), cluster = ~state_f)

cat("\n=== TABLE: ROBUSTNESS CHECKS ===\n")
print(etable(m_rob_2022q2, m_rob_no2020, m_rob_trim, m_rob_owners, m_rob_renters))

# ======================================================================
# SECTION 13: SECTION G - SUB-COMPONENT ANALYSIS
# ======================================================================

cat("\n=== SECTION G: SUB-COMPONENTS ===\n")

m_comp_elec <- feols(ln_elec ~ post_x_treat + post + treat_continuous +
                       ln_income + fam_size + rooms + age + age_sq +
                       owner + urban + female + hispanic + race_black + race_other |
                       state_f + yq_f,
                     data = df, cluster = ~state_f)

m_comp_gas <- feols(ln_gas ~ post_x_treat + post + treat_continuous +
                      ln_income + fam_size + rooms + age + age_sq +
                      owner + urban + female + hispanic + race_black + race_other |
                      state_f + yq_f,
                    data = df %>% filter(NTLGASPQ > 0), cluster = ~state_f)

m_comp_util <- feols(ln_util ~ post_x_treat + post + treat_continuous +
                       ln_income + fam_size + rooms + age + age_sq +
                       owner + urban + female + hispanic + race_black + race_other |
                       state_f + yq_f,
                     data = df %>% filter(UTILPQ > 0), cluster = ~state_f)

cat("\n=== TABLE: SUB-COMPONENTS ===\n")
print(etable(m_comp_elec, m_comp_gas, m_comp_util))

# ======================================================================
# SECTION 14: SECTION H - 75/25 TREATMENT COMPARISON
# ======================================================================

cat("\n=== SECTION H: 75/25 TREATMENT SPLIT ===\n")

m_7525_did <- feols(ln_energy ~ post_x_75_25 + post + treat_75_25 +
                      ln_income + fam_size + rooms + age + age_sq +
                      owner + urban + female + hispanic + race_black + race_other |
                      state_f + yq_f,
                    data = df, cluster = ~state_f)

es_7525_annual <- feols(ln_energy ~ i(rel_year, treat_75_25, ref = -1) +
                          post + treat_75_25 +
                          ln_income + fam_size + rooms + age + age_sq +
                          owner + urban + female + hispanic + race_black + race_other |
                          state_f + yq_f,
                        data = df, cluster = ~state_f)

cat("\n=== TABLE: 75/25 TREATMENT SPLIT ===\n")
print(etable(m_7525_did))

cat("\n75/25 Event Study Summary:\n")
print(summary(es_7525_annual))

# ======================================================================
# SECTION 15: PREPARE DATA FOR VISUALIZATIONS
# ======================================================================

cat("\n=== PREPARING DATA FOR VISUALIZATIONS ===\n")

df_trends_binary <- df %>%
  mutate(treat_group = ifelse(treat_binary == 1, "High Renewable", "Low Renewable")) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    sd_energy = sd(energy_exp, na.rm = TRUE),
    n = n(),
    se_energy = sd_energy / sqrt(n),
    ci_lower = mean_energy - 1.96 * se_energy,
    ci_upper = mean_energy + 1.96 * se_energy,
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

df_trends_tercile <- df %>%
  mutate(treat_group = case_when(
    treat_tercile == 1 ~ "Low (T1)",
    treat_tercile == 2 ~ "Medium (T2)",
    treat_tercile == 3 ~ "High (T3)"
  )) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    sd_energy = sd(energy_exp, na.rm = TRUE),
    n = n(),
    se_energy = sd_energy / sqrt(n),
    ci_lower = mean_energy - 1.96 * se_energy,
    ci_upper = mean_energy + 1.96 * se_energy,
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

df_es_annual <- df %>%
  group_by(rel_year, treat_binary) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    sd_energy = sd(energy_exp, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

df_hi_income <- df %>% filter(inc_quartile == 4)

df_trends_hiinc <- df_hi_income %>%
  mutate(treat_group = ifelse(treat_binary == 1, "High Renewable", "Low Renewable")) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    sd_energy = sd(energy_exp, na.rm = TRUE),
    n = n(),
    se_energy = sd_energy / sqrt(n),
    ci_lower = mean_energy - 1.96 * se_energy,
    ci_upper = mean_energy + 1.96 * se_energy,
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

df_trends_burden <- df %>%
  mutate(treat_group = ifelse(treat_binary == 1, "High Renewable", "Low Renewable")) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    burden_rate = mean(energy_burden, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

df_components <- df %>%
  mutate(treat_group = ifelse(treat_binary == 1, "High Renewable", "Low Renewable")) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    electricity = mean(ELCTRCPQ, na.rm = TRUE),
    gas = mean(NTLGASPQ, na.rm = TRUE),
    utilities = mean(UTILPQ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(electricity, gas, utilities),
               names_to = "component", values_to = "mean_exp") %>%
  arrange(year, quarter)

df_trends_7525 <- df %>%
  mutate(treat_group = ifelse(treat_75_25 == 1, "Top 25%", "Bottom 75%")) %>%
  group_by(year_quarter, year, quarter, treat_group) %>%
  summarise(
    mean_energy = mean(energy_exp, na.rm = TRUE),
    sd_energy = sd(energy_exp, na.rm = TRUE),
    n = n(),
    se_energy = sd_energy / sqrt(n),
    ci_lower = mean_energy - 1.96 * se_energy,
    ci_upper = mean_energy + 1.96 * se_energy,
    .groups = "drop"
  ) %>%
  arrange(year, quarter)

state_treat <- df %>%
  group_by(state) %>%
  summarise(
    mean_per_capita_renew = mean(mean_per_capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_per_capita_renew)

cat("Data aggregated for visualizations.\n")

# ======================================================================
# SECTION 16: FIGURE 1 - PARALLEL TRENDS (BINARY)
# ======================================================================

cat("\nCreating Figure 1: Parallel trends (binary treatment)...\n")

fig1 <- ggplot(df_trends_binary, aes(x = interaction(year, quarter, sep = "Q"),
                                     y = mean_energy, color = treat_group, group = treat_group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treat_group),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = which(df_trends_binary$year_quarter == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  scale_fill_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  labs(
    title = "Parallel Trends: Mean Quarterly Energy Expenditure",
    subtitle = "by Renewable Energy Adoption Level",
    x = "Year-Quarter",
    y = "Mean Energy Expenditure ($)",
    color = "Treatment Group",
    fill = "Treatment Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave(file.path(output_dir, "fig1_parallel_trends.png"), fig1, width = 12, height = 6, dpi = 300)

# ======================================================================
# SECTION 17: FIGURE 2 - ANNUAL EVENT STUDY (CONTINUOUS)
# ======================================================================

cat("Creating Figure 2: Annual event study (continuous treatment)...\n")

es_annual_df <- broom::tidy(es_annual) %>%
  filter(grepl("rel_year", term)) %>%
  mutate(
    rel_year = as.integer(stringr::str_extract(term, "-?\\d+")),
    coef = estimate,
    se = std.error,
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  arrange(rel_year) %>%
  filter(rel_year >= -3 & rel_year <= 2)

fig2 <- ggplot(es_annual_df, aes(x = rel_year, y = coef)) +
  geom_point(size = 3, color = "#2196F3") +
  geom_line(color = "#2196F3", alpha = 0.7, linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "#2196F3", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    title = "Annual Event Study: Effect on Log Energy Expenditure",
    subtitle = "Continuous Treatment",
    x = "Years Relative to 2022",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig2_event_study_annual.png"), fig2, width = 10, height = 6, dpi = 300)

# ======================================================================
# SECTION 18: FIGURE 2B - QUARTERLY EVENT STUDY
# ======================================================================

cat("Creating Figure 2B: Quarterly event study...\n")

es_quarterly_df <- broom::tidy(es_quarterly) %>%
  filter(grepl("rel_quarter", term)) %>%
  mutate(
    rel_quarter = as.integer(stringr::str_extract(term, "-?\\d+")),
    coef = estimate,
    se = std.error,
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  arrange(rel_quarter) %>%
  filter(rel_quarter >= -8 & rel_quarter <= 8)

fig2b <- ggplot(es_quarterly_df, aes(x = rel_quarter, y = coef)) +
  geom_point(size = 2, color = "#2196F3") +
  geom_line(color = "#2196F3", alpha = 0.7, linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, color = "#2196F3", alpha = 0.6) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    title = "Quarterly Event Study: Effect on Log Energy Expenditure",
    subtitle = "Continuous Treatment",
    x = "Quarters Relative to 2022Q1",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig2b_event_study_quarterly.png"), fig2b, width = 11, height = 6, dpi = 300)

# ======================================================================
# SECTION 19: FIGURE 3 - TERCILE TRENDS
# ======================================================================

cat("Creating Figure 3: Tercile trends...\n")

fig3 <- ggplot(df_trends_tercile, aes(x = interaction(year, quarter, sep = "Q"),
                                      y = mean_energy, color = treat_group, group = treat_group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_vline(xintercept = which(df_trends_tercile$year_quarter == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("Low (T1)" = "#2196F3", "Medium (T2)" = "#4CAF50", "High (T3)" = "#F44336")) +
  labs(
    title = "Renewable Energy Adoption Terciles: Mean Energy Expenditure",
    x = "Year-Quarter",
    y = "Mean Energy Expenditure ($)",
    color = "Tercile"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave(file.path(output_dir, "fig3_trends_tercile.png"), fig3, width = 12, height = 6, dpi = 300)

# ======================================================================
# SECTION 20: FIGURE 4 - DENSITY DISTRIBUTIONS
# ======================================================================

cat("Creating Figure 4: Density distributions...\n")

df_plot <- df %>%
  mutate(
    treat_group = ifelse(treat_binary == 1, "High Renewable", "Low Renewable"),
    period = ifelse(year >= 2022, "Post-2022", "Pre-2022")
  )

fig4 <- ggplot(df_plot, aes(x = ln_energy, fill = treat_group, color = treat_group)) +
  geom_density(alpha = 0.5, linewidth = 0.7) +
  facet_wrap(~period, nrow = 1) +
  scale_fill_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  scale_color_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  labs(
    title = "Distribution of Log Energy Expenditure by Treatment and Period",
    x = "Log Energy Expenditure",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig4_distributions.png"), fig4, width = 11, height = 6, dpi = 300)

# ======================================================================
# SECTION 21: FIGURE 5 - COMPONENT TRENDS
# ======================================================================

cat("Creating Figure 5: Component trends...\n")

fig5 <- ggplot(df_components, aes(x = interaction(year, quarter, sep = "Q"),
                                  y = mean_exp, color = component, group = component)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  facet_wrap(~treat_group, nrow = 2) +
  geom_vline(xintercept = which(unique(paste0(df_components$year, "Q", df_components$quarter)) == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("electricity" = "#FFC107", "gas" = "#795548", "utilities" = "#9C27B0")) +
  labs(
    title = "Energy Components: Mean Expenditure Trends",
    x = "Year-Quarter",
    y = "Mean Expenditure ($)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave(file.path(output_dir, "fig5_component_trends.png"), fig5, width = 12, height = 8, dpi = 300)

# ======================================================================
# SECTION 22: FIGURE 6 - INCOME QUARTILE HETEROGENEITY
# ======================================================================

cat("Creating Figure 6: Income quartile heterogeneity...\n")

coef_q <- data.frame(
  quartile = c("Q1\n(Lowest)", "Q2", "Q3", "Q4\n(Highest)"),
  coef = c(coef(m_q1)["post_x_treat"], coef(m_q2)["post_x_treat"],
           coef(m_q3)["post_x_treat"], coef(m_q4)["post_x_treat"]),
  se = c(se(m_q1)["post_x_treat"], se(m_q2)["post_x_treat"],
         se(m_q3)["post_x_treat"], se(m_q4)["post_x_treat"])
) %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  )

fig6 <- ggplot(coef_q, aes(x = quartile, y = coef, fill = quartile)) +
  geom_col(alpha = 0.7, color = "black", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, color = "black", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  scale_fill_manual(values = c("#E8F5E9", "#A5D6A7", "#66BB6A", "#2E7D32")) +
  labs(
    title = "Heterogeneous Effects by Income Quartile",
    subtitle = "DiD Coefficient on Post × Renewable Treatment Interaction",
    x = "Income Quartile",
    y = "Coefficient (95% CI)"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig6_income_heterogeneity.png"), fig6, width = 9, height = 6, dpi = 300)

# ======================================================================
# SECTION 23: FIGURE 7 - HIGH-INCOME EVENT STUDY
# ======================================================================

cat("Creating Figure 7: High-income (Q4) event study...\n")

es_q4_df <- broom::tidy(es_q4_annual) %>%
  filter(grepl("rel_year", term)) %>%
  mutate(
    rel_year = as.integer(stringr::str_extract(term, "-?\\d+")),
    coef = estimate,
    se = std.error,
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  arrange(rel_year) %>%
  filter(rel_year >= -3 & rel_year <= 2)

fig7 <- ggplot(es_q4_df, aes(x = rel_year, y = coef)) +
  geom_point(size = 3, color = "#F44336") +
  geom_line(color = "#F44336", alpha = 0.7, linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "#F44336", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    title = "Event Study: High-Income Households (Income Quartile 4)",
    x = "Years Relative to 2022",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig7_event_study_high_income.png"), fig7, width = 10, height = 6, dpi = 300)

# ======================================================================
# SECTION 24: FIGURE 8 - ENERGY BURDEN EVENT STUDY
# ======================================================================

cat("Creating Figure 8: Energy burden event study...\n")

es_burden_df <- broom::tidy(es_burden) %>%
  filter(grepl("rel_year", term)) %>%
  mutate(
    rel_year = as.integer(stringr::str_extract(term, "-?\\d+")),
    coef = estimate,
    se = std.error,
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  arrange(rel_year) %>%
  filter(rel_year >= -3 & rel_year <= 2)

fig8 <- ggplot(es_burden_df, aes(x = rel_year, y = coef)) +
  geom_point(size = 3, color = "#FF6F00") +
  geom_line(color = "#FF6F00", alpha = 0.7, linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "#FF6F00", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    title = "Event Study: Energy Burden",
    x = "Years Relative to 2022",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig8_event_study_burden.png"), fig8, width = 10, height = 6, dpi = 300)

# ======================================================================
# SECTION 25: FIGURE 9 - HIGH-INCOME PARALLEL TRENDS
# ======================================================================

cat("Creating Figure 9: High-income parallel trends...\n")

fig9 <- ggplot(df_trends_hiinc, aes(x = interaction(year, quarter, sep = "Q"),
                                    y = mean_energy, color = treat_group, group = treat_group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treat_group),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = which(df_trends_hiinc$year_quarter == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  scale_fill_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  labs(
    title = "Parallel Trends: High-Income Households (Quartile 4)",
    x = "Year-Quarter",
    y = "Mean Energy Expenditure ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(file.path(output_dir, "fig9_high_income_trends.png"), fig9, width = 12, height = 6, dpi = 300)

# ======================================================================
# SECTION 26: FIGURE 10 - ENERGY BURDEN TRENDS
# ======================================================================

cat("Creating Figure 10: Energy burden trends...\n")

fig10 <- ggplot(df_trends_burden, aes(x = interaction(year, quarter, sep = "Q"),
                                      y = burden_rate, color = treat_group, group = treat_group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_vline(xintercept = which(df_trends_burden$year_quarter == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("High Renewable" = "#F44336", "Low Renewable" = "#2196F3")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Energy Burden Trends",
    x = "Year-Quarter",
    y = "Proportion with Energy Burden"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(file.path(output_dir, "fig10_burden_trends.png"), fig10, width = 12, height = 6, dpi = 300)

# ======================================================================
# SECTION 27: FIGURE 11 - 75/25 PARALLEL TRENDS
# ======================================================================

cat("Creating Figure 11: 75/25 parallel trends...\n")

fig11 <- ggplot(df_trends_7525, aes(x = interaction(year, quarter, sep = "Q"),
                                    y = mean_energy, color = treat_group, group = treat_group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treat_group),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = which(df_trends_7525$year_quarter == "2022Q1")[1],
             linetype = "dashed", color = "gray40", linewidth = 0.7) +
  scale_color_manual(values = c("Top 25%" = "#F44336", "Bottom 75%" = "#2196F3")) +
  scale_fill_manual(values = c("Top 25%" = "#F44336", "Bottom 75%" = "#2196F3")) +
  labs(
    title = "Parallel Trends: 75/25 Treatment Split",
    x = "Year-Quarter",
    y = "Mean Energy Expenditure ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave(file.path(output_dir, "fig11_75_25_parallel_trends.png"), fig11, width = 12, height = 6, dpi = 300)

# ======================================================================
# SECTION 28: FIGURE 12 - 75/25 EVENT STUDY
# ======================================================================

cat("Creating Figure 12: 75/25 event study...\n")

es_7525_df <- broom::tidy(es_7525_annual) %>%
  filter(grepl("rel_year", term)) %>%
  mutate(
    rel_year = as.integer(stringr::str_extract(term, "-?\\d+")),
    coef = estimate,
    se = std.error,
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  ) %>%
  arrange(rel_year) %>%
  filter(rel_year >= -3 & rel_year <= 2)

fig12 <- ggplot(es_7525_df, aes(x = rel_year, y = coef)) +
  geom_point(size = 3, color = "#9C27B0") +
  geom_line(color = "#9C27B0", alpha = 0.7, linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, color = "#9C27B0", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    title = "Event Study: 75/25 Treatment Split",
    x = "Years Relative to 2022",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig12_75_25_event_study.png"), fig12, width = 10, height = 6, dpi = 300)

# ======================================================================
# SECTION 29: FIGURE 13 - TREATMENT COMPARISON
# ======================================================================

cat("Creating Figure 13: Treatment specification comparison...\n")

coef_comparison <- data.frame(
  specification = c("Binary\n(50/50)", "75/25", "Continuous"),
  coef = c(coef(m1)["post_x_high"], coef(m_7525_did)["post_x_75_25"], coef(m2)["post_x_treat"]),
  se = c(se(m1)["post_x_high"], se(m_7525_did)["post_x_75_25"], se(m2)["post_x_treat"])
) %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se,
    color_group = specification
  )

fig13 <- ggplot(coef_comparison, aes(x = specification, y = coef, fill = color_group)) +
  geom_col(alpha = 0.7, color = "black", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.3, color = "black", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.7) +
  scale_fill_manual(values = c("#2196F3", "#FF9800", "#4CAF50")) +
  labs(
    title = "Robustness: Alternative Treatment Specifications",
    x = "Treatment Specification",
    y = "Coefficient (95% CI)"
  ) +
  guides(fill = "none") +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "fig13_treatment_comparison.png"), fig13, width = 9, height = 6, dpi = 300)

# ======================================================================
# SECTION 30: FIGURE 14 - STATE-LEVEL TREATMENT DISTRIBUTION
# ======================================================================

cat("Creating Figure 14: State-level treatment distribution...\n")

fig14 <- ggplot(state_treat, aes(x = reorder(as.factor(state), mean_per_capita_renew),
                                 y = mean_per_capita_renew)) +
  geom_col(fill = "#2196F3", alpha = 0.7, color = "black", linewidth = 0.4) +
  coord_flip() +
  labs(
    title = "Distribution of Renewable Energy per Capita Across States",
    x = "State (FIPS Code)",
    y = "Mean Renewable Energy per Capita"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(output_dir, "fig14_state_distribution.png"), fig14, width = 10, height = 12, dpi = 300)

# ======================================================================
# SECTION 31: SUMMARY OF KEY RESULTS
# ======================================================================

cat("\n")
cat(strrep("=", 80))
cat("\n=== SUMMARY OF KEY RESULTS ===\n")
cat(strrep("=", 80))
cat("\n")

coef_main <- coef(m2)["post_x_treat"]
se_main <- se(m2)["post_x_treat"]
ci_lower_main <- coef_main - 1.96 * se_main
ci_upper_main <- coef_main + 1.96 * se_main
pval_main <- 2 * (1 - pnorm(abs(coef_main / se_main)))

cat("MAIN FINDING (Model 2 - Continuous DiD):\n")
cat(sprintf("  Coefficient: %.4f\n", coef_main))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n", ci_lower_main, ci_upper_main))
cat(sprintf("  t-stat: %.3f, p-value: %.4f\n", coef_main / se_main, pval_main))
cat(sprintf("  Interpretation: a one-unit increase in treatment intensity is associated with a %.2f%% change in household energy expenditure after 2022.\n\n",
            coef_main * 100))

coef_burden <- coef(m_burden)["post_x_treat"]
se_burden <- se(m_burden)["post_x_treat"]

cat("ENERGY BURDEN:\n")
cat(sprintf("  Coefficient: %.4f\n", coef_burden))
cat(sprintf("  SE: %.4f\n\n", se_burden))

cat("INCOME HETEROGENEITY:\n")
cat(sprintf("  Q1: %.4f\n", coef(m_q1)["post_x_treat"]))
cat(sprintf("  Q2: %.4f\n", coef(m_q2)["post_x_treat"]))
cat(sprintf("  Q3: %.4f\n", coef(m_q3)["post_x_treat"]))
cat(sprintf("  Q4: %.4f\n\n", coef(m_q4)["post_x_treat"]))

cat("SAMPLE CHARACTERISTICS:\n")
cat(sprintf("  Total observations: %d\n", nrow(df)))
cat(sprintf("  Number of states: %d\n", dplyr::n_distinct(df$state)))
cat(sprintf("  Time period: %s to %s\n", min(df$year_quarter, na.rm = TRUE), max(df$year_quarter, na.rm = TRUE)))
cat(sprintf("  Pre-treatment quarters: %d\n", dplyr::n_distinct(df$year_quarter[df$post == 0])))
cat(sprintf("  Post-treatment quarters: %d\n", dplyr::n_distinct(df$year_quarter[df$post == 1])))
cat(sprintf("  Households with energy burden: %.1f%%\n\n", mean(df$energy_burden, na.rm = TRUE) * 100))

cat("SUB-COMPONENTS:\n")
cat(sprintf("  Electricity: %.4f\n", coef(m_comp_elec)["post_x_treat"]))
cat(sprintf("  Natural gas: %.4f\n", coef(m_comp_gas)["post_x_treat"]))
cat(sprintf("  Utilities: %.4f\n\n", coef(m_comp_util)["post_x_treat"]))

cat("ROBUSTNESS CHECKS:\n")
cat(sprintf("  Alternative post (2022Q2): %.4f\n", coef(m_rob_2022q2)["post_x_75_25"]))
cat(sprintf("  Excluding 2020: %.4f\n", coef(m_rob_no2020)["post_x_treat"]))
cat(sprintf("  Trim extreme states: %.4f\n", coef(m_rob_trim)["post_x_treat"]))
cat(sprintf("  Homeowners only: %.4f\n", coef(m_rob_owners)["post_x_treat"]))
cat(sprintf("  Renters only: %.4f\n\n", coef(m_rob_renters)["post_x_treat"]))

cat("FIGURES SAVED IN:", output_dir, "\n")

cat(strrep("=", 80))
cat("\nANALYSIS COMPLETE\n")
cat(strrep("=", 80))
cat("\n")

gc()
cat("\nScript execution completed successfully.\n")