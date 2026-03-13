# ============================================================
# Calibration Curves for Fisheries Stock Assessment
# Companion R Script
#
# Blog: "Calibration curves for simulation-based inference:
#        bringing ML diagnostics to fisheries stock assessment"
# Author: MSK | 2026-03-12
#
# Structure:
#   - GLOBAL SETUP  : shared parameters, data generation, theme
#   - EXAMPLE 1     : coverage calibration (table + curve for R0)
#   - EXAMPLE 2     : continuous calibration via PIT (to be added)
#   - EXAMPLE 3     : forecast/predictive interval calibration (to be added)
# ============================================================


# ============================================================
# LIBRARIES
# ============================================================
library(ggplot2)
library(cowplot)
library(gridExtra)
library(scales)


# ============================================================
# GLOBAL SETUP
# ============================================================

set.seed(42)

# ---- Simulation dimensions ----
n_sim   <- 100   # simulation replicates
n_hist  <- 30    # historical years
n_fut   <- 20    # future/projection years
n_years <- n_hist + n_fut   # 50 total years

# ---- True population parameter: R0 (virgin recruitment) ----
true_R0  <- 1000   # true value, fixed across all replicates
sigma_R0 <- 200    # true SD of estimation error for R0

# For each replicate, the model produces an estimate and a reported SE.
#
# WELL-CALIBRATED: the reported SE equals the true estimation SD,
#   so confidence intervals achieve their nominal coverage rates.
#
# POORLY-CALIBRATED: the reported SE is underestimated (overconfident),
#   so confidence intervals are too narrow and miss the truth far more
#   often than the nominal level implies.

est_R0_well <- rnorm(n_sim, mean = true_R0, sd = sigma_R0)
se_R0_well  <- rep(sigma_R0, n_sim)               # correct SE

est_R0_poor <- rnorm(n_sim, mean = true_R0, sd = sigma_R0)
se_R0_poor  <- rep(sigma_R0 * 0.35, n_sim)        # SE underestimated ~65%

# ---- True biomass trajectory ----
# Logistic recovery from an initially depleted state.
# All replicates share this same true trajectory; only the
# estimates (and their reported uncertainty) differ.

K           <- 5000    # carrying capacity
r           <- 0.10    # intrinsic growth rate
depl_frac   <- 0.25    # initial biomass as fraction of K
years       <- seq_len(n_years)
true_B      <- K / (1 + ((1 / depl_frac - 1) * exp(-r * (years - 1))))
sigma_B_rel <- 0.15    # relative (CV-style) estimation error for biomass
sigma_B     <- true_B * sigma_B_rel   # absolute SD by year

# Simulate biomass estimates (row = replicate, column = year).
# Matrix fills column-by-column, so rep(..., each = n_sim) correctly
# assigns year-specific mean and SD to each column.
B_est_well <- matrix(
  rnorm(n_sim * n_years,
        mean = rep(true_B,   each = n_sim),
        sd   = rep(sigma_B,  each = n_sim)),
  nrow = n_sim, ncol = n_years
)
B_se_well <- matrix(
  rep(sigma_B, each = n_sim),
  nrow = n_sim, ncol = n_years
)

B_est_poor <- matrix(
  rnorm(n_sim * n_years,
        mean = rep(true_B,   each = n_sim),
        sd   = rep(sigma_B,  each = n_sim)),
  nrow = n_sim, ncol = n_years
)
B_se_poor <- matrix(
  rep(sigma_B * 0.35, each = n_sim),
  nrow = n_sim, ncol = n_years
)

# ---- Shared plot theme and color palette ----
pal <- c(
  "Well-calibrated"   = "#2C7BB6",
  "Poorly-calibrated" = "#D7191C"
)

theme_cal <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    axis.line         = element_line(color = "grey30", linewidth = 0.5),
    axis.ticks        = element_line(color = "grey30", linewidth = 0.4),
    legend.position   = "bottom",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 12),
    legend.key.width  = unit(1.5, "cm"),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(color = "grey40", size = 10),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA)
  )


# ============================================================
# EXAMPLE 1: Coverage Calibration
# ============================================================
#
# We examine the R0 estimates from the global setup. For each
# nominal CI level (0% to 100% in steps of 5%), we ask:
# "What fraction of our 100 replicates actually contain the truth
#  within that stated CI?" -- this is empirical coverage.
#
# A perfectly calibrated set produces a straight 1:1 line.
# An overconfident (too-narrow SE) set curves below the 1:1 line.

# -- Helper: empirical coverage at each nominal level --
compute_coverage <- function(estimates, ses, true_val, nom_levels) {
  sapply(nom_levels, function(alpha) {
    z  <- qnorm((1 + alpha) / 2)
    lo <- estimates - z * ses
    hi <- estimates + z * ses
    mean(true_val >= lo & true_val <= hi)
  })
}

nominal_levels <- seq(0, 1, by = 0.05)

cov_well <- compute_coverage(est_R0_well, se_R0_well, true_R0, nominal_levels)
cov_poor <- compute_coverage(est_R0_poor, se_R0_poor, true_R0, nominal_levels)

# Long-form data frame for calibration curve
cov_df <- data.frame(
  nominal   = rep(nominal_levels, 2),
  empirical = c(cov_well, cov_poor),
  type      = factor(
    rep(c("Well-calibrated", "Poorly-calibrated"), each = length(nominal_levels)),
    levels = c("Well-calibrated", "Poorly-calibrated")
  )
)

# -- Truncated table: 50%, 80%, 95% --
table_levels <- c(0.50, 0.80, 0.95)

fmt_pct <- function(x) paste0(round(x * 100, 1), "%")

table_data <- data.frame(
  "Nominal CI"         = paste0(table_levels * 100, "%"),
  "Well-calibrated"    = fmt_pct(compute_coverage(est_R0_well, se_R0_well, true_R0, table_levels)),
  "Poorly-calibrated"  = fmt_pct(compute_coverage(est_R0_poor, se_R0_poor, true_R0, table_levels)),
  check.names = FALSE
)

# Style the table grob: alternating row fill, bold header
tbl_header_style <- list(
  fg_params = list(fontface = "bold", fontsize = 12, col = "grey20"),
  bg_params = list(fill = "grey93", col = NA)
)
tbl_core_style <- list(
  fg_params = list(fontsize = 12, col = "grey20"),
  bg_params = list(
    fill = matrix(rep(c("white", "grey97", "white"), each = 3), nrow = 3),
    col  = NA
  )
)

tbl_grob <- tableGrob(
  table_data,
  rows  = NULL,
  theme = ttheme_minimal(
    colhead = tbl_header_style,
    core    = tbl_core_style
  )
)

# -- Calibration curve panel --
p_curve <- ggplot(cov_df, aes(x = nominal, y = empirical, color = type)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "grey55", linewidth = 0.9
  ) +
#   annotate(
#     "text", x = 0.72, y = 0.68, label = "Perfect calibration",
#     color = "grey45", size = 3.5, angle = 40, fontface = "italic"
#   ) +
  geom_line(linewidth = 1.3, alpha = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    name   = "Nominal coverage",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.20)
  ) +
  scale_y_continuous(
    name   = "Empirical coverage",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.20)
  ) +
  labs(
    title    = "Coverage calibration curve — R0",
    subtitle = paste0(
      "n = ", n_sim, " replicates  |  ",
      "well-calibrated: SE = \u03c3 mean |  ",
      "poorly-calibrated: SE = 0.35 \u00d7 mean"
    )
  ) +
  theme_cal

# -- Compose and save --
# cowplot::plot_grid handles both grobs and ggplots in one layout
fig1 <- plot_grid(
  tbl_grob,
  p_curve,
  nrow       = 1,
  rel_widths = c(1, 2.2),
  labels     = c("A", "B"),
  label_size = 13
)

ggsave(
  file.path(
    "posts/2026-03-12-calibration-fisheries",
    "example1_coverage_calibration.png"
  ),
  fig1,
  width = 12, height = 5.5, dpi = 150, bg = "white"
)

message("Example 1 saved.")


# ============================================================
# EXAMPLE 2: Continuous Calibration via Probability Integral Transform
# ============================================================
#
# The PIT asks: for each replicate s, what probability does the
# *reported* predictive distribution assign to values <= the truth?
#
#   u_s = F_s(theta_true)  =  pnorm(true_R0, est_R0[s], se_R0[s])
#
# If the model is correctly specified (right SE), the true value
# is equally likely to land anywhere in the predictive distribution,
# so u_s ~ Uniform(0,1) → flat PIT histogram.
#
# Overconfident model (SE too small): the truth often lies far in the
# tails of the narrow predictive distribution → u_s pile up near 0
# and 1 → U-shaped PIT histogram.
#
# We reuse the R0 estimates and SEs from the global setup.

# -- Compute PIT values --
pit_well <- pnorm(true_R0, mean = est_R0_well, sd = se_R0_well)
pit_poor <- pnorm(true_R0, mean = est_R0_poor, sd = se_R0_poor)

pit_df <- data.frame(
  u    = c(pit_well, pit_poor),
  type = factor(
    rep(c("Well-calibrated", "Poorly-calibrated"), each = n_sim),
    levels = c("Well-calibrated", "Poorly-calibrated")
  )
)

# Expected uniform density line (count scale): n_sim / n_bins
n_bins       <- 10
uniform_count <- n_sim / n_bins

# -- Build each panel separately so we can color fill independently --
# n_bins is a global constant; uniform reference is computed per call
# so the function works regardless of how many PIT values are passed.
make_pit_panel <- function(u_vals, label, fill_col) {
  n_obs <- length(u_vals)
  df    <- data.frame(u = u_vals)
  ggplot(df, aes(x = u)) +
    geom_hline(
      yintercept = n_obs / n_bins,
      linetype   = "dashed", color = "grey50", linewidth = 0.9
    ) +
    geom_histogram(
      aes(y = after_stat(count)),
      breaks   = seq(0, 1, length.out = n_bins + 1),
      fill     = fill_col,
      color    = "white",
      alpha    = 0.85
    ) +
    scale_x_continuous(
      name   = expression(italic(u)[s] == F[s](theta[true])),
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", "0.25", "0.5", "0.75", "1")
    ) +
    scale_y_continuous(
      name   = "Count",
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(title = label, subtitle = "Dashed line = uniform reference (ideal)") +
    theme_cal +
    theme(legend.position = "none")
}

p_pit_well <- make_pit_panel(
  pit_well,
  label    = "Well-calibrated",
  fill_col = pal["Well-calibrated"]
)

p_pit_poor <- make_pit_panel(
  pit_poor,
  label    = "Poorly-calibrated (overconfident)",
  fill_col = pal["Poorly-calibrated"]
)

fig2 <- plot_grid(
  p_pit_well,
  p_pit_poor,
  nrow       = 1,
  labels     = c("A", "B"),
  label_size = 13
)

ggsave(
  file.path(
    "posts/2026-03-12-calibration-fisheries",
    "example2_PIT_histogram.png"
  ),
  fig2,
  width = 10, height = 4.5, dpi = 150, bg = "white"
)

message("Example 2 saved.")


# ============================================================
# EXAMPLE 3: Forecast / Predictive Interval Calibration
# ============================================================
#
# The same coverage question from Example 1 is now applied to the
# FORECAST portion of the biomass time series (years n_hist+1 … n_years).
#
# For each nominal CI level α and each forecast year y, we ask:
# "What fraction of the 100 replicates contain true_B[y] inside the
#  reported CI for that year?"  → coverage(α, y)
#
# This yields two complementary views:
#   Panel A — Calibration curve: average coverage over all forecast years
#              vs. nominal level (directly comparable to Example 1).
#   Panel B — Coverage by forecast year for three spotlight CI levels
#              (50%, 80%, 95%).  Color = nominal level; linetype =
#              calibration type; dashed horizontals = nominal targets.
#              This reveals whether calibration holds across the horizon.

# -- Indices for the forecast block --
fut_idx   <- (n_hist + 1):n_years          # columns 31–50 in B matrices
fut_years <- seq_along(fut_idx)            # 1…20 relative forecast years
true_B_fut <- true_B[fut_idx]

# -- Per-year coverage as a function of nominal level --
#    Returns a matrix: rows = nominal levels, cols = forecast years
coverage_by_year <- function(B_est_mat, B_se_mat, true_vec, nom_levels) {
  fut_cols <- (n_hist + 1):n_years
  t(sapply(nom_levels, function(alpha) {
    z <- qnorm((1 + alpha) / 2)
    sapply(seq_along(fut_cols), function(j) {
      y   <- fut_cols[j]
      lo  <- B_est_mat[, y] - z * B_se_mat[, y]
      hi  <- B_est_mat[, y] + z * B_se_mat[, y]
      mean(true_vec[j] >= lo & true_vec[j] <= hi)
    })
  }))
}

cov_yr_well <- coverage_by_year(B_est_well, B_se_well, true_B_fut, nominal_levels)
cov_yr_poor <- coverage_by_year(B_est_poor, B_se_poor, true_B_fut, nominal_levels)

# -- Panel A: Calibration curve (mean over forecast years) --
cov_avg_well <- rowMeans(cov_yr_well)
cov_avg_poor <- rowMeans(cov_yr_poor)

cov_fcast_df <- data.frame(
  nominal   = rep(nominal_levels, 2),
  empirical = c(cov_avg_well, cov_avg_poor),
  type      = factor(
    rep(c("Well-calibrated", "Poorly-calibrated"), each = length(nominal_levels)),
    levels = c("Well-calibrated", "Poorly-calibrated")
  )
)

p3a <- ggplot(cov_fcast_df, aes(x = nominal, y = empirical, color = type)) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "grey55", linewidth = 0.9
  ) +
#   annotate(
#     "text", x = 0.72, y = 0.68, label = "Perfect calibration",
#     color = "grey45", size = 3.5, angle = 40, fontface = "italic"
#   ) +
  geom_line(linewidth = 1.3, alpha = 0.9) +
  geom_point(size = 2.8) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    name   = "Nominal coverage",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1), breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    name   = "Empirical coverage\n(mean over forecast years)",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1), breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title    = "Calibration curve — forecast biomass",
    subtitle = "Average across all 20 projection years"
  ) +
  theme_cal

# -- Panel B: Coverage by forecast year for spotlight CI levels --
spotlight <- c(0.50, 0.80, 0.95)
spot_labels <- paste0(spotlight * 100, "% CI")

# Long-form data frame: one row per (type, nominal, forecast year)
make_yr_df <- function(cov_mat, type_label) {
  rows <- which(nominal_levels %in% spotlight)
  do.call(rbind, lapply(seq_along(rows), function(k) {
    data.frame(
      year     = fut_years,
      coverage = cov_mat[rows[k], ],
      nominal  = spotlight[k],
      nom_lab  = factor(spot_labels[k], levels = spot_labels),
      type     = type_label
    )
  }))
}

yr_df <- rbind(
  make_yr_df(cov_yr_well, "Well-calibrated"),
  make_yr_df(cov_yr_poor, "Poorly-calibrated")
)
yr_df$type <- factor(yr_df$type, levels = c("Well-calibrated", "Poorly-calibrated"))

# Horizontal reference lines (one per nominal level)
ref_df <- data.frame(
  nominal = spotlight,
  nom_lab = factor(spot_labels, levels = spot_labels)
)

# Color spotlight levels with a neutral sequential palette
spot_pal <- c("50% CI" = "#4DAF4A", "80% CI" = "#FF7F00", "95% CI" = "#984EA3")

p3b <- ggplot(yr_df, aes(x = year, y = coverage,
                          color = nom_lab, linetype = type)) +
  geom_hline(
    data     = ref_df,
    aes(yintercept = nominal, color = nom_lab),
    linetype = "dashed", linewidth = 0.65, alpha = 0.6
  ) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 1.8, alpha = 0.8) +
  scale_color_manual(values = spot_pal, name = "Nominal CI") +
  scale_linetype_manual(
    values = c("Well-calibrated" = "solid", "Poorly-calibrated" = "longdash"),
    name   = NULL
  ) +
  scale_x_continuous(
    name   = "Forecast year",
    breaks = seq(2, 20, by = 2)
  ) +
  scale_y_continuous(
    name   = "Empirical coverage",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1), breaks = seq(0, 1, 0.2)
  ) +
  labs(
    title    = "Coverage by forecast year",
    subtitle = "Dashed lines = nominal targets; solid/dashed traces = calibration type"
  ) +
  guides(
    color    = guide_legend(order = 1, override.aes = list(linewidth = 1.3)),
    linetype = guide_legend(order = 2, override.aes = list(linewidth = 1.0))
  ) +
  theme_cal

fig3 <- plot_grid(
  p3a, p3b,
  nrow       = 1,
  rel_widths = c(1, 1.4),
  labels     = c("A", "B"),
  label_size = 13
)

ggsave(
  file.path(
    "posts/2026-03-12-calibration-fisheries",
    "example3_forecast_calibration.png"
  ),
  fig3,
  width = 13, height = 5.5, dpi = 150, bg = "white"
)

message("Example 3 saved.")


# ============================================================
# EXAMPLE 4: Putting It Together — Mixed Calibration
# ============================================================
#
# A key insight: calibration is QUANTITY-SPECIFIC. The same underlying
# model can simultaneously exhibit:
#   - Well-calibrated parameter estimates  (R0)       → flat PIT
#   - Overconfident state estimates        (hist. B)  → U-shaped PIT
#   - Underconfident forecast intervals   (future B)  → hump-shaped PIT
#
# We hold point estimates (B_est_well) and true estimation noise constant,
# but manipulate only the *reported* SE for each output type:
#   R0 report SE  : sigma_R0        (correct)
#   Historical B  : 0.35 * sigma_B  (too small → overconfident)
#   Forecast B    : 2.50 * sigma_B  (too large → underconfident)
#
# PIT values are pooled across years within each block so the
# histogram is well-populated even with modest replicate counts:
#   R0     : n_sim           = 100   PIT values
#   Hist B : n_sim * n_hist  = 3000  PIT values
#   Fut B  : n_sim * n_fut   = 2000  PIT values

# -- Reported SE matrices for the mixed scenario --
B_se_hist_over <- matrix(
  rep(sigma_B[1:n_hist] * 0.35, each = n_sim),
  nrow = n_sim, ncol = n_hist
)
B_se_fut_under <- matrix(
  rep(sigma_B[(n_hist + 1):n_years] * 2.5, each = n_sim),
  nrow = n_sim, ncol = n_fut
)

# -- R0 PIT (well-calibrated, reuse global) --
pit_mix_r0 <- pnorm(true_R0, mean = est_R0_well, sd = se_R0_well)

# -- Historical biomass PIT (overconfident SE) --
# Build a matrix with true_B[y] repeated n_sim times per column;
# matrix() fills column-by-column so each column j = true_B[j].
true_B_hist_mat <- matrix(
  rep(true_B[1:n_hist], each = n_sim),
  nrow = n_sim, ncol = n_hist
)
pit_mix_hist <- as.vector(
  pnorm(true_B_hist_mat,
        mean = B_est_well[, 1:n_hist],
        sd   = B_se_hist_over)
)

# -- Forecast biomass PIT (underconfident SE) --
true_B_fut_mat <- matrix(
  rep(true_B[(n_hist + 1):n_years], each = n_sim),
  nrow = n_sim, ncol = n_fut
)
pit_mix_fut <- as.vector(
  pnorm(true_B_fut_mat,
        mean = B_est_well[, (n_hist + 1):n_years],
        sd   = B_se_fut_under)
)

# -- Three-color palette for the three quantities --
mix_pal <- c(
  "R0 parameter"       = "#3182BD",   # blue
  "Historical biomass" = "#E6550D",   # orange-red
  "Forecast biomass"   = "#31A354"    # green
)

p4a <- make_pit_panel(
  pit_mix_r0,
  label    = expression(R[0] * " parameter  (well-calibrated)"),
  fill_col = mix_pal["R0 parameter"]
)
p4b <- make_pit_panel(
  pit_mix_hist,
  label    = "Historical biomass  (overconfident SE)",
  fill_col = mix_pal["Historical biomass"]
)
p4c <- make_pit_panel(
  pit_mix_fut,
  label    = "Forecast biomass  (underconfident SE)",
  fill_col = mix_pal["Forecast biomass"]
)

fig4 <- plot_grid(
  p4a, p4b, p4c,
  nrow       = 1,
  labels     = c("A", "B", "C"),
  label_size = 13
)

ggsave(
  file.path(
    "posts/2026-03-12-calibration-fisheries",
    "example4_mixed_calibration.png"
  ),
  fig4,
  width = 15, height = 5, dpi = 150, bg = "white"
)

message("Example 4 saved.")
