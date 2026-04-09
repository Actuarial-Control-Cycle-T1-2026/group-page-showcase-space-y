# -----------------------------------------------------------------------------
# 0. Install and load required packages
# -----------------------------------------------------------------------------
pkgs <- c(
  "readxl",          # Read xlsx
  "dplyr",           # Data manipulation
  "ggplot2",         # Visualisation
  "MASS",            # glm.nb
  "fitdistrplus",    # Distribution fitting & comparison
  "actuar",          # Actuarial distributions (Burr, LogLogistic, etc.)
  "car",             # VIF
  "lmtest",          # Likelihood ratio test
  "patchwork",       # Plot composition
  "scales",          # Axis formatting
  "glmnet",          # LASSO / Ridge
  "zoo",             # rollmean for Hill plot smoothing
  "ismev",           # gpd.fit / gpd.diag
  "evd",             # pgpd / qgpd / rgpd
  "tidyr"
)

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

# Prevent MASS from masking dplyr verbs
select <- dplyr::select
filter <- dplyr::filter


# -----------------------------------------------------------------------------
# 1. Import dataset
# -----------------------------------------------------------------------------

sev_raw <- readRDS("D:/Dropbox/Dropbox/Private document/SOA case study/Dataset/bi_sev_cleaned.rds")

sev <- sev_raw

x <- sev$claim_amount
n <- length(x)


# =============================================================================
# 1.  DESCRIPTIVE STATISTICS & SHAPE DIAGNOSTICS
# =============================================================================

cat("n =", n, "\n")
cat("Any non-positive values?", any(x <= 0), "\n")

cat(sprintf("mean        = %12.2f\n",   mean(x)))
cat(sprintf("median      = %12.2f\n",   median(x)))
cat(sprintf("sd          = %12.2f\n",   sd(x)))
cat(sprintf("CV          = %12.4f\n",   sd(x) / mean(x)))
cat(sprintf("skewness    = %12.4f\n",   mean(((x - mean(x)) / sd(x))^3)))
cat(sprintf("ex.kurtosis = %12.4f\n",   mean(((x - mean(x)) / sd(x))^4) - 3))
cat(sprintf("log_mu      = %12.4f\n",   mean(log(x))))
cat(sprintf("log_sigma   = %12.4f\n",   sd(log(x))))

summary(sev$claim_amount)
quantile(sev$claim_amount, c(.50, .75, .90, .95, .99, .995, .999))

# Tail concentration check
sev %>%
  summarise(
    total = sum(claim_amount),
    top1  = sum(claim_amount[claim_amount >= quantile(claim_amount, .99)]) / total,
    top5  = sum(claim_amount[claim_amount >= quantile(claim_amount, .95)]) / total
  )

# Variation across solar systems
sev %>%
  group_by(solar_system) %>%
  summarise(
    n    = n(),
    mean = mean(claim_amount),
    med  = median(claim_amount),
    p95  = quantile(claim_amount, .95),
    p99  = quantile(claim_amount, .99)
  ) %>%
  arrange(desc(p99))

# Histogram of log(claim_amount)
hist(log(sev$claim_amount), breaks = 60,
     main = "log(claim_amount) — BI", xlab = "")

# QQ-plot: log(claim_amount) vs Normal
p_qq_ln <- ggplot(sev, aes(sample = log(claim_amount))) +
  stat_qq(alpha = 0.4, size = 0.9, colour = "#636363") +
  stat_qq_line(colour = "#d6604d", linewidth = 1) +
  labs(
    title    = "Q-Q Plot: log(Claim Amount) vs Normal",
    subtitle = "Good fit across body and tail → LogNormal well-supported",
    x = "Theoretical Normal quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 12)

print(p_qq_ln)

# Log-log survival plot
xs   <- sort(x)
surv <- 1 - ecdf(xs)(xs)

plot(log(xs), log(surv),
     xlab = "log(claim)",
     ylab = "log(P(X > t))",
     main = "Log-Log Survival Plot — BI Claims")
# Concave shape (not straight line) → consistent with LogNormal, not pure Pareto


# =============================================================================
# 2.  TAIL DIAGNOSTICS
# =============================================================================
# We run these diagnostics for completeness and to justify the final model choice.
# Conclusion from all four plots:
#   - MEP rises but Tail Q-Q shows LogNormal tracks the diagonal best
#   - GPD sigma* unstable across thresholds → GPD splice not warranted
#   - Hill plot never plateaus → no clean Pareto structure
#   - LogNormal is the final chosen model

# -----------------------------------------------------------------------------
# 2.1  Mean Excess Plot (MEP)
# -----------------------------------------------------------------------------

u_grid <- quantile(xs, seq(0.10, 0.97, by = 0.005))

mep_df <- data.frame(
  u           = as.numeric(u_grid),
  mean_excess = sapply(u_grid, function(u) mean(xs[xs > u] - u)),
  n_above     = sapply(u_grid, function(u) sum(xs > u))
) %>%
  mutate(
    sd_excess = sapply(u, function(u) {
      exc <- xs[xs > u] - u
      if (length(exc) < 2) return(NA)
      sd(exc)
    }),
    se    = sd_excess / sqrt(n_above),
    ci_lo = mean_excess - 1.96 * se,
    ci_hi = mean_excess + 1.96 * se
  )

p_mep <- ggplot(mep_df, aes(x = u, y = mean_excess)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.15, fill = "#7b2d8b") +
  geom_line(colour = "#7b2d8b", linewidth = 1) +
  geom_vline(xintercept = quantile(x, c(0.75, 0.85, 0.90)),
             linetype = "dashed",
             colour   = c("#e31a1c", "#ff7f00", "#33a02c")) +
  annotate("text",
           x     = quantile(x, c(0.75, 0.85, 0.90)),
           y     = max(mep_df$mean_excess, na.rm = TRUE) * 0.92,
           label = c("p75", "p85", "p90"),
           colour = c("#e31a1c", "#ff7f00", "#33a02c"),
           hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Mean Excess Plot (MEP) — BI Claims",
    subtitle = "Rising MEP initially suggested heavy tail;\nTail Q-Q confirms LogNormal is the better fit",
    x = "Threshold  u",
    y = "e(u) = E[X - u | X > u]"
  ) +
  theme_minimal(base_size = 12)

print(p_mep)

# -----------------------------------------------------------------------------
# 2.2  Hill Plot (diagnostic only — no clean plateau found)
# -----------------------------------------------------------------------------

xs_desc <- sort(x, decreasing = TRUE)
k_seq   <- 5:floor(n / 5)

xi_hill <- sapply(k_seq, function(k) {
  mean(log(xs_desc[1:k])) - log(xs_desc[k + 1])
})

hill_df           <- data.frame(k = k_seq, xi = xi_hill)
smooth_k          <- 30
hill_df$xi_smooth <- rollmean(hill_df$xi, k = smooth_k, fill = NA, align = "center")

# Manual stable region: k = 300 to 700 (visual inspection — curve never truly plateaus)
k_stable_lo <- 300
k_stable_hi <- 700
xi_plateau  <- mean(hill_df$xi_smooth[
  hill_df$k >= k_stable_lo & hill_df$k <= k_stable_hi
], na.rm = TRUE)

cat(sprintf("\nHill estimate (k=%d to %d, manual): xi ≈ %.4f\n",
            k_stable_lo, k_stable_hi, xi_plateau))
cat("Note: Hill plot never plateaus → no clean Pareto structure → supports LogNormal\n")

p_hill <- ggplot(hill_df, aes(x = k, y = xi)) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  geom_line(aes(y = xi_smooth), colour = "orange", linewidth = 1) +
  geom_hline(yintercept = xi_plateau, linetype = "dashed", colour = "#e31a1c") +
  annotate("text",
           x     = max(k_seq) * 0.65,
           y     = xi_plateau * 1.05,
           label = sprintf("Manual stable region ξ ≈ %.3f (k=%d–%d)",
                           xi_plateau, k_stable_lo, k_stable_hi),
           colour = "#e31a1c", size = 3.5) +
  labs(
    title    = "Hill Plot — Tail Index Estimation (BI Claims)",
    subtitle = "Curve continuously rising — no clean plateau → LogNormal preferred over Pareto",
    x = "k (number of upper order statistics)",
    y = expression(hat(xi)[Hill](k))
  ) +
  theme_minimal(base_size = 12)

print(p_hill)


# =============================================================================
# 3.  GPD STABILITY PLOTS (diagnostic only — confirms GPD not appropriate)
# =============================================================================
# sigma* is not flat at any threshold → GPD assumption violated
# We run this for completeness and to formally justify skipping the splice.

cat("\n=== GPD Stability Plots (diagnostic only) ===\n")

u_candidates <- quantile(x, seq(0.60, 0.95, by = 0.01))

stab_list <- lapply(u_candidates, function(u) {
  exc <- x[x > u] - u
  if (length(exc) < 30) return(NULL)
  tryCatch({
    fit        <- ismev::gpd.fit(x, threshold = u, show = FALSE)
    sigma      <- fit$mle[1]
    xi         <- fit$mle[2]
    sigma_star <- sigma - xi * u
    data.frame(u = u, xi = xi, sigma = sigma, sigma_star = sigma_star,
               se_xi = fit$se[2], se_sigma = fit$se[1],
               n_excess = length(exc))
  }, error = function(e) NULL)
})

stab_df <- do.call(rbind, Filter(Negate(is.null), stab_list))

thresh_labels <- data.frame(
  u     = as.numeric(quantile(x, c(0.75, 0.85, 0.90))),
  label = c("p75", "p85", "p90"),
  col   = c("#e31a1c", "#ff7f00", "#33a02c")
)

p_stab_xi <- ggplot(stab_df, aes(x = u, y = xi)) +
  geom_ribbon(aes(ymin = xi - 1.96 * se_xi, ymax = xi + 1.96 * se_xi),
              alpha = 0.20, fill = "#1f78b4") +
  geom_line(colour = "#1f78b4", linewidth = 1) +
  geom_vline(xintercept = thresh_labels$u,
             linetype = "dashed", colour = thresh_labels$col) +
  annotate("text", x = thresh_labels$u,
           y = max(stab_df$xi + 1.96 * stab_df$se_xi) * 0.95,
           label = thresh_labels$label, colour = thresh_labels$col,
           hjust = -0.15, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_x_continuous(labels = comma) +
  labs(title    = "GPD Stability — xi (diagnostic only)",
       subtitle = "sigma* not flat at any threshold → GPD splice not used",
       x = "Threshold u", y = expression(hat(xi)(u))) +
  theme_minimal(base_size = 12)

p_stab_sig <- ggplot(stab_df, aes(x = u, y = sigma_star)) +
  geom_ribbon(aes(ymin = sigma_star - 1.96 * se_sigma,
                  ymax = sigma_star + 1.96 * se_sigma),
              alpha = 0.20, fill = "#e31a1c") +
  geom_line(colour = "#e31a1c", linewidth = 1) +
  geom_vline(xintercept = thresh_labels$u,
             linetype = "dashed", colour = thresh_labels$col) +
  annotate("text", x = thresh_labels$u,
           y = max(stab_df$sigma_star + 1.96 * stab_df$se_sigma) * 0.95,
           label = thresh_labels$label, colour = thresh_labels$col,
           hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma) +
  labs(title    = "GPD Stability — sigma* (diagnostic only)",
       subtitle = "sigma* rising throughout → GPD assumption violated",
       x = "Threshold u", y = expression(sigma^"*"(u))) +
  theme_minimal(base_size = 12)

print(p_stab_xi / p_stab_sig)
cat("Conclusion: sigma* is not flat → GPD splice is not appropriate for BI claims.\n")
cat("Final model: LogNormal fitted to all claims.\n\n")

#Conclusion: sigma* is not flat → GPD splice is not appropriate for BI claims.
# Final model: LogNormal fitted to all claims.


# =============================================================================
# 4.  FINAL MODEL — LogNormal MLE on full data
# =============================================================================

cat("\n=== Final Model: LogNormal (full data MLE) ===\n")

fit_final <- fitdist(x, "lnorm")

mu_hat    <- fit_final$estimate["meanlog"]
sigma_hat <- fit_final$estimate["sdlog"]
se_mu     <- fit_final$sd["meanlog"]
se_sigma  <- fit_final$sd["sdlog"]

cat(sprintf("mu    (meanlog) = %.4f  (SE: %.4f)\n", mu_hat,    se_mu))
cat(sprintf("sigma (sdlog)   = %.4f  (SE: %.4f)\n", sigma_hat, se_sigma))

# Implied moments on the original scale
lnorm_mean <- exp(mu_hat + sigma_hat^2 / 2)
lnorm_var  <- (exp(sigma_hat^2) - 1) * exp(2 * mu_hat + sigma_hat^2)

cat(sprintf("\nImplied mean     = %s\n", comma(round(lnorm_mean))))
cat(sprintf("Implied std dev  = %s\n",  comma(round(sqrt(lnorm_var)))))
cat(sprintf("Implied CV       = %.4f\n", sqrt(lnorm_var) / lnorm_mean))

# Implied mean     = 4,376,551
# Implied std dev  = 7,823,144
# Implied CV       = 1.7875

# =============================================================================
# 5.  UNCONDITIONAL MLE FITS — Distribution Comparison
# =============================================================================
# Fit competing distributions to confirm LogNormal wins on AIC/BIC.

scale_k <- 1000
y       <- x / scale_k   # work in $000s for numerical stability

mom_shape <- mean(y)^2 / var(y)
mom_rate  <- mean(y)   / var(y)

cat("\n=== Unconditional MLE Fits (on $000 scale) ===\n")

fit_ln <- fitdist(y, "lnorm")

fit_gam <- fitdist(y, "gamma",
                   start = list(shape = mom_shape, rate = mom_rate),
                   lower = c(1e-4, 1e-8))

fit_wei <- fitdist(y, "weibull",
                   start = list(shape = 1, scale = mean(y)),
                   lower = c(1e-3, 1e-3))

fit_llogis <- tryCatch(
  fitdist(y, "llogis",
          start = list(shape = 2, rate = 1 / median(y)),
          lower = c(1e-3, 1e-8)),
  error = function(e) { message("LogLogistic failed"); NULL }
)

fit_burr <- tryCatch(
  fitdist(y, "burr",
          start = list(shape1 = 2, shape2 = 1, rate = 1 / median(y)),
          lower = c(1e-3, 1e-3, 1e-8)),
  error = function(e) { message("Burr failed"); NULL }
)

fits_all   <- list(LogNormal   = fit_ln,
                   Gamma       = fit_gam,
                   Weibull     = fit_wei,
                   LogLogistic = fit_llogis,
                   Burr        = fit_burr)
fits_valid <- Filter(Negate(is.null), fits_all)

# 5.1  AIC / BIC table
cat("\n--- AIC / BIC Comparison ---\n")
aic_tbl <- data.frame(
  Distribution = names(fits_valid),
  AIC          = sapply(fits_valid, function(f) f$aic),
  BIC          = sapply(fits_valid, function(f) f$bic)
) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC)
print(aic_tbl)

# LogNormal have lowest (or near-lowest) AIC, confirming final choice

# 5.2  Anderson-Darling GoF (tail-sensitive)
cat("\n--- Goodness-of-Fit (Anderson-Darling) ---\n")
gof <- gofstat(fits_valid, fitnames = names(fits_valid))
print(gof)

# 5.3  Four-panel diagnostic plot
par(mfrow = c(2, 2))
denscomp(fits_valid, legendtext = names(fits_valid),
         main = "Density Comparison ($000s)", xlab = "Claim Amount ($000s)")
qqcomp  (fits_valid, legendtext = names(fits_valid), main = "Q-Q Comparison")
cdfcomp (fits_valid, legendtext = names(fits_valid),
         main = "CDF Comparison", xlab = "Claim Amount ($000s)")
ppcomp  (fits_valid, legendtext = names(fits_valid), main = "P-P Comparison")
par(mfrow = c(1, 1))

# 5.4  Tail Q-Q: zoom into top 20%
q_seq   <- seq(0.80, 0.995, by = 0.001)
emp_q   <- as.numeric(quantile(y, q_seq))

tail_qq <- data.frame(Empirical = emp_q)
tail_qq$LogNormal <- qlnorm(q_seq, fit_ln$estimate["meanlog"],
                            fit_ln$estimate["sdlog"])
if (!is.null(fit_burr))
  tail_qq$Burr <- qburr(q_seq,
                        shape1 = fit_burr$estimate["shape1"],
                        shape2 = fit_burr$estimate["shape2"],
                        rate   = fit_burr$estimate["rate"])

p_tail_qq <- tail_qq %>%
  tidyr::pivot_longer(-Empirical, names_to = "Model", values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Tail Q-Q Plot — Top 20% of Claims ($000s) (BI)",
    subtitle = "LogNormal tracks the diagonal most closely → confirmed as final model",
    x = "Theoretical quantile ($000s)",
    y = "Empirical quantile ($000s)"
  ) +
  theme_minimal(base_size = 12)

print(p_tail_qq)


# =============================================================================
# 6.  RISK MEASURES FROM LOGNORMAL MODEL
# =============================================================================
# VaR(p)  = qlnorm(p, mu, sigma)
# TVaR(p) = exp(mu + sigma^2/2) * pnorm((mu + sigma^2 - log(VaR)) / sigma) / (1-p)
# Closed-form TVaR is exact for LogNormal.

cat("\n=== Risk Measures — LogNormal Model ===\n")

p_levels <- c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 0.999)

var_vals  <- qlnorm(p_levels, mu_hat, sigma_hat)

# TVaR closed form for LogNormal:
# TVaR(p) = E[X] * Phi((mu + sigma^2 - log(VaR(p))) / sigma) / (1 - p)
tvar_lnorm <- function(p) {
  v       <- qlnorm(p, mu_hat, sigma_hat)
  e_x     <- exp(mu_hat + sigma_hat^2 / 2)
  alpha   <- (mu_hat + sigma_hat^2 - log(v)) / sigma_hat
  e_x * pnorm(alpha) / (1 - p)
}

cat(sprintf("\n%-12s  %14s  %14s  %14s\n",
            "Level", "VaR ($)", "TVaR ($)", "Empirical VaR"))
for (i in seq_along(p_levels)) {
  emp_q <- as.numeric(quantile(x, p_levels[i]))
  cat(sprintf("%-12s  %14s  %14s  %14s\n",
              paste0(100 * p_levels[i], "%"),
              comma(round(var_vals[i])),
              comma(round(tvar_lnorm(p_levels[i]))),
              comma(round(emp_q))))
}

# Monte Carlo verification
set.seed(2026)
n_mc  <- 1e6
x_mc  <- rlnorm(n_mc, mu_hat, sigma_hat)

cat("\nTVaR — Monte Carlo verification (N = 1,000,000):\n")
for (p in c(0.90, 0.95, 0.99, 0.995)) {
  v_mc    <- quantile(x_mc, p)
  tvar_mc <- mean(x_mc[x_mc > v_mc])
  cat(sprintf("  TVaR(%.1f%%)  closed-form = %s  |  MC = %s\n",
              100 * p,
              comma(round(tvar_lnorm(p))),
              comma(round(tvar_mc))))
}

# TVaR(90.0%)  closed-form = 20,416,482  |  MC = 20,382,629
# TVaR(95.0%)  closed-form = 28,648,921  |  MC = 28,592,218
# TVaR(99.0%)  closed-form = 56,665,551  |  MC = 56,519,011
# TVaR(99.5%)  closed-form = 73,566,925  |  MC = 73,339,872

# Loss concentration
cat("\n--- Loss Concentration (Monte Carlo) ---\n")
conc <- data.frame(x = x_mc) %>%
  summarise(
    total  = sum(x),
    top1   = sum(x[x >= quantile(x, 0.99)]) / total,
    top5   = sum(x[x >= quantile(x, 0.95)]) / total
  )
cat(sprintf("Top 1%% of claims: %.1f%% of total loss\n", conc$top1 * 100))
cat(sprintf("Top 5%% of claims: %.1f%% of total loss\n", conc$top5 * 100))

#Top 1% of claims: 12.9% of total loss
#Top 5% of claims: 32.7% of total loss

# =============================================================================
# 7.  FINAL SUMMARY
# =============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("SEVERITY FITTING SUMMARY — BUSINESS INTERRUPTION\n")
cat(strrep("=", 65), "\n\n", sep = "")

cat("--- Model Selection Journey ---\n")
cat("MEP:            Rising → initial heavy-tail signal\n")
cat("Hill plot:      No plateau → no clean Pareto structure\n")
cat("GPD stability:  sigma* not flat → GPD splice violated\n")
cat("Tail Q-Q:       LogNormal tracks diagonal best (Burr/Spliced overestimate)\n")
cat("AIC/BIC:        LogNormal wins or is within ΔAIC < 2 of competitors\n")
cat("CONCLUSION:     LogNormal is the final severity model\n\n")

cat("--- Final LogNormal Parameters ---\n")
cat(sprintf("mu    (meanlog) = %.4f  (SE: %.4f)\n", mu_hat,    se_mu))
cat(sprintf("sigma (sdlog)   = %.4f  (SE: %.4f)\n", sigma_hat, se_sigma))
cat(sprintf("Implied mean    = %s\n\n", comma(round(lnorm_mean))))

cat("--- Risk Measures ---\n")
for (i in seq_along(p_levels)) {
  cat(sprintf("VaR(%4.1f%%)  = %s  |  TVaR = %s\n",
              100 * p_levels[i],
              comma(round(var_vals[i])),
              comma(round(tvar_lnorm(p_levels[i])))))
}

cat("\n--- Best Competing Distribution (AIC) ---\n")
cat(sprintf("Rank 1: %s  (delta_AIC = 0.00)\n", aic_tbl$Distribution[1]))
cat(sprintf("Rank 2: %s  (delta_AIC = %.2f)\n",
            aic_tbl$Distribution[2], aic_tbl$delta_AIC[2]))
cat("\nNote: LogNormal is GLM-compatible (use Gaussian GLM on log(claim_amount))\n")
cat("      VaR and TVaR are exact via closed-form LogNormal formulas.\n")
cat("\n", strrep("=", 65), "\n", sep = "")


# =============================================================================
# Full pipeline summary
# =============================================================================
#
#  Data import
#  ↓
#  Descriptive stats (mean, CV, skewness, log-histogram, QQ plot)
#  ↓
#  Tail diagnostics: MEP → rising (initial heavy-tail signal)
#  ↓
#  Hill plot → no plateau → Pareto not supported
#  ↓
#  GPD stability plots → sigma* not flat → splice not warranted
#  ↓
#  Unconditional MLE (LogNormal, Gamma, Weibull, LogLogistic, Burr)
#  ↓
#  Tail Q-Q (top 20%) → LogNormal tracks diagonal best
#  ↓
#  AIC / BIC / Anderson-Darling → LogNormal confirmed
#  ↓
#  Final model: LogNormal MLE on full data
#  ↓
#  VaR / TVaR: closed-form LogNormal + Monte Carlo verification
