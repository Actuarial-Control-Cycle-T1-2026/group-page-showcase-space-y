# =============================================================================
# CARGO LOSSES — SEVERITY MODELLING
# =============================================================================
# Structure mirrors the Workers' Compensation severity script.
# Must-keep covariates (for any GLM extension): cargo_value, cargo_type,
# route_risk.
# Key difference from WC: we do NOT default to GLM for severity because
# cargo losses are heavily driven by high-value shipments and catastrophic
# route events — a spliced LogNormal / GPD model is more appropriate for
# both pricing and risk-measure calculation.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Install & load required packages
# -----------------------------------------------------------------------------
pkgs <- c(
  "dplyr",        # data manipulation
  "ggplot2",      # visualisation
  "MASS",         # glm.nb (kept for optional GLM comparison)
  "fitdistrplus", # fitdist / gofstat
  "actuar",       # Burr, LogLogistic, Pareto
  "car",          # VIF (if GLM path is taken)
  "lmtest",       # likelihood-ratio tests
  "patchwork",    # multi-panel plots
  "scales",       # comma / percent formatters
  "gridExtra",    # grid.arrange
  "glmnet",       # regularised GLM (optional)
  "zoo",          # rollmean for Hill smoother
  "ismev",        # gpd.fit / gpd.diag
  "evd",          # pgpd / qgpd / rgpd
  "tidyr"         # pivot_longer
)

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Prevent MASS masking dplyr
select <- dplyr::select
filter <- dplyr::filter

# -----------------------------------------------------------------------------
# Import dataset
# -----------------------------------------------------------------------------
sev_cl <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/cl_sev_cleaned.rds") 
# The cleaned dataset should contain at minimum:
#   claim_amount  — positive numeric, the loss to model
#   cargo_value   — declared/insured value of the shipment
#   cargo_type    — factor: e.g. Electronics, Perishables, Chemicals, ...
#   route_risk    — ordered factor or numeric: risk rating of the trade lane
#   (other columns kept for exploratory use)

# Quick sanity check on must-keep variables
stopifnot(
  all(c("claim_amount", "cargo_value", "cargo_type", "route_risk")
      %in% names(sev_cl))
)

# ==============================================================================
# 1.  Descriptive Statistics & Shape Diagnostics
# ==============================================================================
# Before fitting anything, answer: what KIND of distribution does this look like?
#   (a) Is it right-skewed? (almost always yes for cargo losses)
#   (b) Does log(x) look Normal? -> LogNormal candidate
#   (c) How heavy is the tail? -> CV, skewness, mean excess plot

# 1.1  Overall claim amount distribution
x <- sev_cl$claim_amount
cat("n =", length(x), "\n")
cat("Any non-positive values?", any(x <= 0), "\n")   # must be FALSE

cat(sprintf("n           = %d\n",        length(x)))
cat(sprintf("mean        = %9.2f\n",    mean(x)))
cat(sprintf("median      = %9.2f\n",    median(x)))
cat(sprintf("sd          = %9.2f\n",    sd(x)))
cat(sprintf("CV          = %9.4f\n",    sd(x) / mean(x)))
cat(sprintf("skewness    = %9.4f\n",    mean(((x - mean(x)) / sd(x))^3)))
cat(sprintf("ex.kurtosis = %9.4f\n",    mean(((x - mean(x)) / sd(x))^4) - 3))
cat(sprintf("log_mu      = %9.4f\n",    mean(log(x))))
cat(sprintf("log_sigma   = %9.4f\n",    sd(log(x))))

summary(sev_cl$claim_amount)
quantile(sev_cl$claim_amount, c(.5, .75, .9, .95, .99, .995, .999))

#  50%       75%       90%       95%       99%     99.5%     99.9% 
# 382805   4065140  23104170  42406256 104094478 134768961 237121721 

# A large gap between p99 and the median signals a heavy tail — common in cargo
# because a single catastrophic loss (container fire, ship sinking) can dwarf
# hundreds of routine claims.

hist(log(sev_cl$claim_amount), breaks = 60,
     main = "log(Cargo Claim Amount)", xlab = "")

# 1.2  Tail concentration — how much of total loss sits in the extreme quantiles?
sev_cl %>%
  summarise(
    total = sum(claim_amount),
    top1  = sum(claim_amount[claim_amount >= quantile(claim_amount, .99)]) / total,
    top5  = sum(claim_amount[claim_amount >= quantile(claim_amount, .95)]) / total
  )
# If top 1% > 15% of total losses, EVT / spliced modelling is justified.

#total  top1  top5
#  1 229246486577. 0.217 0.538
# 21.7% > 15%, EVT


# 1.3  Breakdown by must-keep categorical covariates

## By cargo_type
sev_cl %>%
  group_by(cargo_type) %>%
  summarise(
    n    = n(),
    mean = mean(claim_amount),
    med  = median(claim_amount),
    p90  = quantile(claim_amount, .90),
    p99  = quantile(claim_amount, .99)
  ) %>%
  arrange(desc(p99))

## By route_risk
sev_cl %>%
  group_by(route_risk) %>%
  summarise(
    n    = n(),
    mean = mean(claim_amount),
    med  = median(claim_amount),
    p90  = quantile(claim_amount, .90),
    p99  = quantile(claim_amount, .99)
  ) %>%
  arrange(desc(p99))

# 1.4  cargo_value vs claim_amount — is there a proportionality signal?
# For cargo, claim_amount is often a fraction of cargo_value (partial loss).
# A log-log scatter helps spot the relationship that drives the GLM later.

p_val_claim <- ggplot(
  sev_cl,
  aes(x = log10(cargo_value),
      y = log10(claim_amount),
      colour = cargo_type)
) +
  geom_point(alpha = 0.30, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  labs(
    title    = "Cargo Value vs Claim Amount (log–log relationship)",
    subtitle = "Each line shows the fitted log–log linear trend for each cargo type",
    x        = "log10(Cargo Value)",
    y        = "log10(Claim Amount)",
    colour   = "Cargo Type"
  ) +
  theme_minimal(base_size = 12)

print(p_val_claim)

# 1.5  QQ-plot of log(claim_amount) vs Normal
p_qq_ln <- ggplot(sev_cl, aes(sample = log(claim_amount))) +
  stat_qq(alpha = 0.4, size = 0.9, colour = "#636363") +
  stat_qq_line(colour = "#d6604d", linewidth = 1) +
  labs(
    title    = "Q-Q Plot: log(Cargo Claim Amount) vs Normal",
    subtitle = "Curvature at right tail = heavier than LogNormal",
    x        = "Theoretical Normal quantiles",
    y        = "Sample quantiles"
  ) +
  theme_minimal(base_size = 12)

print(p_qq_ln)

# 1.6  Log-log survival plot — quick visual for power-law behaviour
xs   <- sort(x)
surv <- 1 - ecdf(xs)(xs)
plot(log(xs), log(surv),
     xlab = "log(claim)", ylab = "log(P(X > t))",
     main = "Log-Log Survival Plot — Cargo Losses")
# A straight line on this plot suggests a Pareto (pure power-law) tail.


# ==============================================================================
# 2.  Tail Diagnostics
# ==============================================================================
# Three complementary tools:
#   (a) Mean Excess Plot  — rising line -> Pareto-type tail
#   (b) Hill estimator    — direct estimate of tail index xi as fn of k
#   (c) Log-log survival  — power-law check (already done above)

n  <- length(x)
xs <- sort(x)

# ------------------------------------------------------------------------------
# 2.1  Mean Excess Plot (MEP)
# ------------------------------------------------------------------------------
# e(u) = E[X - u | X > u]
# Rising slope -> GPD with xi > 0 (Pareto-type)
# Flat         -> Exponential
# Falling      -> Bounded tail

u_grid <- quantile(xs, seq(0.10, 0.97, by = 0.005))

mep_df <- data.frame(
  u           = as.numeric(u_grid),
  mean_excess = sapply(u_grid, function(u) mean(xs[xs > u] - u)),
  n_above     = sapply(u_grid, function(u) sum(xs > u))
)

mep_df <- mep_df %>%
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
    title    = "Mean Excess Plot (MEP) — Cargo Losses",
    subtitle = paste0(
      "Monotonically rising -> Pareto-type tail (GPD xi > 0)\n",
      "Shaded band = 95% CI for e(u)"
    ),
    x = "Threshold  u",
    y = "e(u) = E[X - u | X > u]"
  ) +
  theme_minimal(base_size = 12)

print(p_mep)
# For cargo: rising MEP is expected — large container / bulk losses dominate.
# A rising MEP
# A plateau or drop near the very top may reflect thin data, not tail behaviour.

# ------------------------------------------------------------------------------
# 2.2  Hill Estimator — tail index xi
# ------------------------------------------------------------------------------
xs_desc <- sort(x, decreasing = TRUE)
k_seq   <- 5:floor(n / 5)

xi_hill <- sapply(k_seq, function(k) {
  mean(log(xs_desc[1:k])) - log(xs_desc[k + 1])
})

smooth_k   <- 30
hill_df    <- data.frame(k = k_seq, xi = xi_hill)
hill_df$xi_smooth <- rollmean(hill_df$xi, k = smooth_k, fill = NA, align = "center")

diff_xi      <- abs(diff(hill_df$xi_smooth))
diff_smooth  <- rollmean(diff_xi, k = smooth_k, fill = NA, align = "center")
plateau_idx  <- which.min(diff_smooth)
xi_plateau   <- hill_df$xi_smooth[plateau_idx]
k_plateau    <- hill_df$k[plateau_idx]

p_hill <- ggplot(hill_df, aes(x = k, y = xi)) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  geom_line(aes(y = xi_smooth), colour = "orange", linewidth = 1) +
  geom_hline(yintercept = xi_plateau, linetype = "dashed", colour = "#e31a1c") +
  geom_vline(xintercept = k_plateau,  linetype = "dashed", colour = "grey40") +
  annotate("text",
           x     = max(k_seq) * 0.65,
           y     = xi_plateau * 1.05,
           label = sprintf("Plateau ξ ≈ %.3f", xi_plateau),
           colour = "#e31a1c", size = 3.5) +
  labs(
    title    = "Hill Plot — Tail Index Estimation (Cargo Losses)",
    subtitle = "Blue: raw | Orange: smoothed | Dashed: plateau region",
    x        = "k (number of upper order statistics)",
    y        = expression(hat(xi)[Hill](k))
  ) +
  theme_minimal(base_size = 12)

print(p_hill)

cat("\n--- Hill Plateau Estimate ---\n")
cat(sprintf("Plateau k ≈ %d\n",           k_plateau))
cat(sprintf("Estimated xi ≈ %.4f\n",      xi_plateau))


# ==============================================================================
# 3.  Threshold Selection — GPD Stability Plots
# ==============================================================================
# Under the Pickands-Balkema-de Haan theorem:
#   (X - u | X > u) ~ GPD(xi, sigma_u)  for u large enough.
# If GPD holds above u_0:
#   xi        is CONSTANT in u (shape stability plot)
#   sigma*(u) = sigma(u) - xi*u  is CONSTANT in u (modified scale stability)
#
# Choose the LOWEST u where BOTH plots flatline — balances bias vs variance.

cat("\n=== Threshold Selection via Stability Plots ===\n")

u_candidates <- quantile(x, seq(0.60, 0.95, by = 0.01))

stab_list <- lapply(u_candidates, function(u) {
  exc <- x[x > u] - u
  if (length(exc) < 30) return(NULL)
  tryCatch({
    fit        <- ismev::gpd.fit(x, threshold = u, show = FALSE)
    sigma      <- fit$mle[1]
    xi         <- fit$mle[2]
    sigma_star <- sigma - xi * u
    data.frame(
      u          = u,
      xi         = xi,
      sigma      = sigma,
      sigma_star = sigma_star,
      se_xi      = fit$se[2],
      se_sigma   = fit$se[1],
      n_excess   = length(exc)
    )
  }, error = function(e) NULL)
})

stab_df <- do.call(rbind, Filter(Negate(is.null), stab_list))

thresh_labels <- data.frame(
  u     = as.numeric(quantile(x, c(0.75, 0.85, 0.90))),
  label = c("p75", "p85", "p90"),
  col   = c("#e31a1c", "#ff7f00", "#33a02c")
)

p_stab_xi <- ggplot(stab_df, aes(x = u, y = xi)) +
  geom_ribbon(aes(ymin = xi - 1.96 * se_xi,
                  ymax = xi + 1.96 * se_xi),
              alpha = 0.20, fill = "#1f78b4") +
  geom_line(colour = "#1f78b4", linewidth = 1) +
  geom_vline(xintercept = thresh_labels$u,
             linetype = "dashed", colour = thresh_labels$col) +
  annotate("text",
           x     = thresh_labels$u,
           y     = max(stab_df$xi + 1.96 * stab_df$se_xi) * 0.95,
           label = thresh_labels$label,
           colour = thresh_labels$col, hjust = -0.15, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "GPD Stability Plot — Shape Parameter xi (Cargo)",
    subtitle = "xi should be constant above the true threshold",
    x = "Threshold  u",
    y = expression(hat(xi)(u))
  ) +
  theme_minimal(base_size = 12)

p_stab_sig <- ggplot(stab_df, aes(x = u, y = sigma_star)) +
  geom_ribbon(aes(ymin = sigma_star - 1.96 * se_sigma,
                  ymax = sigma_star + 1.96 * se_sigma),
              alpha = 0.20, fill = "#e31a1c") +
  geom_line(colour = "#e31a1c", linewidth = 1) +
  geom_vline(xintercept = thresh_labels$u,
             linetype = "dashed", colour = thresh_labels$col) +
  annotate("text",
           x     = thresh_labels$u,
           y     = max(stab_df$sigma_star + 1.96 * stab_df$se_sigma) * 0.95,
           label = thresh_labels$label,
           colour = thresh_labels$col, hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "GPD Stability Plot — Modified Scale sigma* (Cargo)",
    subtitle = expression(sigma^"*"(u) == sigma(u) - xi %.% u ~~ "should be constant above threshold"),
    x = "Threshold  u",
    y = expression(sigma^"*"(u))
  ) +
  theme_minimal(base_size = 12)

print(p_stab_xi / p_stab_sig)


## From the plot, choose p85 as the threshold u.


cat("\nEstimates at candidate thresholds:\n")
cat(sprintf("%-6s  %-12s  %-8s  %-8s  %-8s  %-8s  %s\n",
            "Level", "u", "sigma", "xi", "SE_sig", "SE_xi", "n_excess"))
for (pct in c(0.75, 0.85, 0.90)) {
  u_v <- as.numeric(quantile(x, pct))
  row <- stab_df[which.min(abs(stab_df$u - u_v)), ]
  cat(sprintf("p%-5.0f  %-12s  %-8.3f  %-8.4f  %-8.4f  %-8.4f  %d\n",
              100 * pct, comma(round(u_v)),
              row$sigma, row$xi, row$se_sigma, row$se_xi, row$n_excess))
}


# Decision: choose the lowest u where xi and sigma* are both stable.
# For cargo data, the p85 threshold is a sensible default starting point.
# Lower threshold (p85) = more exceedances and often a heavier fitted tail (larger xi),
# so the GPD Q-Q plot (theoretical vs empirical) aligns better — fewer points above the line.
u_chosen <- quantile(x, 0.85)
u_alt    <- quantile(x, 0.80)


# ==============================================================================
# 4.  GPD Fitting at Chosen Threshold
# ==============================================================================

cat(sprintf("\n=== GPD Fitting at u = p85 = %s ===\n", comma(round(u_chosen))))
cat(sprintf("    (sensitivity at u = p80 = %s)\n",     comma(round(u_alt))))

gpd_primary <- ismev::gpd.fit(x, threshold = u_chosen, show = FALSE)
gpd_alt     <- ismev::gpd.fit(x, threshold = u_alt,    show = FALSE)

xi_hat    <- gpd_primary$mle[2]
sigma_hat <- gpd_primary$mle[1]
se_xi     <- gpd_primary$se[2]
se_sigma  <- gpd_primary$se[1]
n_excess  <- sum(x > u_chosen)
n_total   <- length(x)
phi_u     <- 1 - n_excess / n_total

cat("\n--- Primary GPD Parameter Estimates ---\n")
cat(sprintf("Threshold u   = %s  (p85)\n",     comma(round(u_chosen))))
cat(sprintf("n_excess      = %d  (%.1f%% of claims above threshold)\n",
            n_excess, 100 * (1 - phi_u)))
cat(sprintf("xi  (shape)   = %6.4f  (95%% CI: [%.4f, %.4f])\n",
            xi_hat, xi_hat - 1.96 * se_xi, xi_hat + 1.96 * se_xi))
# xi = 0.2706 > 0  ->  Heavy Pareto-type tail

cat(sprintf("sigma (scale) = %6.4f  (95%% CI: [%.4f, %.4f])\n",
            sigma_hat, sigma_hat - 1.96 * se_sigma, sigma_hat + 1.96 * se_sigma))

cat("\n--- Interpretation of xi ---\n")
if (xi_hat > 0) {
  cat(sprintf("xi = %.4f > 0  ->  Heavy Pareto-type tail\n", xi_hat))
  cat(sprintf("Finite moments exist only for order < 1/xi = %.2f\n", 1 / xi_hat))
  cat(sprintf("  Mean exists?     %s\n", ifelse(1 / xi_hat > 1, "YES", "NO")))
  cat(sprintf("  Variance exists? %s\n", ifelse(1 / xi_hat > 2, "YES", "NO")))
} else if (xi_hat == 0) {
  cat("xi = 0  ->  Exponential tail (all moments finite)\n")
} else {
  cat(sprintf("xi = %.4f < 0  ->  Bounded tail\n", xi_hat))
  cat(sprintf("Upper endpoint = u - sigma/xi = %s\n",
              comma(round(u_chosen - sigma_hat / xi_hat))))
}

cat("\n--- Sensitivity: p85 vs p80 ---\n")
cat(sprintf("p85: xi = %.4f  sigma = %.4f  n_excess = %d\n",
            xi_hat, sigma_hat, n_excess))
cat(sprintf("p80: xi = %.4f  sigma = %.4f  n_excess = %d\n",
            gpd_alt$mle[2], gpd_alt$mle[1], sum(x > u_alt)))
cat("If xi estimates are close -> threshold choice is stable\n")



# GPD built-in diagnostic plots (probability, quantile, return level, density)
par(mfrow = c(2, 2))
ismev::gpd.diag(gpd_primary)
mtext(sprintf("GPD Diagnostics  (u = p85 = %s)", comma(round(u_chosen))),
      outer = TRUE, cex = 1.1, line = -1.5)
par(mfrow = c(1, 1))

# Manual ggplot Q-Q for exceedances
exceedances <- sort(x[x > u_chosen] - u_chosen)
n_exc       <- length(exceedances)
p_plot      <- (1:n_exc) / (n_exc + 1)
q_theory    <- evd::qgpd(p_plot, loc = 0, scale = sigma_hat, shape = xi_hat)

p_qq_exc <- ggplot(data.frame(empirical = exceedances, theoretical = q_theory),
                   aes(x = theoretical, y = empirical)) +
  geom_point(alpha = 0.5, size = 1.5, colour = "#1f78b4") +
  geom_abline(slope = 1, intercept = 0,
              colour = "#e31a1c", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Q-Q Plot: Exceedances vs Fitted GPD (Cargo)",
    subtitle = sprintf("u = %s  |  xi = %.4f  |  sigma = %.4f",
                       comma(round(u_chosen)), xi_hat, sigma_hat),
    x = "Theoretical GPD quantile",
    y = "Empirical exceedance"
  ) +
  theme_minimal(base_size = 12)

print(p_qq_exc)
# ==============================================================================
# 5.  SPLICED MODEL - LogNormal Body + GPD Tail
# ==============================================================================
# Why splice? A single distribution fits the body but underestimates the tail.
# GPD is only valid above u. Splicing: body below u, GPD above u.
#
#   F(x) = phi_u * F_body(x) / F_body(u)        for x <= u
#   F(x) = phi_u + (1-phi_u) * G_GPD(x - u)     for x >  u
# phi_u = P(X <= u), F_body = LogNormal on claims <= u, G_GPD = GPD from Section 4.

cat("\n=== Building Spliced Model (LogNormal body + GPD tail) ===\n")

x_body   <- x[x <= u_chosen]
fit_body <- fitdist(x_body, "lnorm")
mu_body  <- fit_body$estimate["meanlog"]
sd_body  <- fit_body$estimate["sdlog"]

cat(sprintf("Body LogNormal: meanlog = %.4f, sdlog = %.4f  [n = %d, %.1f%% of data]\n",
            mu_body, sd_body, length(x_body), 100 * phi_u))
cat(sprintf("Tail GPD:       xi = %.4f, sigma = %.4f  [n = %d, %.1f%% of data]\n",
            xi_hat, sigma_hat, n_excess, 100 * (1 - phi_u)))

# Spliced CDF and quantile
F_spliced <- function(q) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  ifelse(
    q <= u_chosen,
    phi_u * plnorm(q, mu_body, sd_body) / F_body_at_u,
    phi_u + (1 - phi_u) *
      evd::pgpd(q - u_chosen, loc = 0, scale = sigma_hat, shape = xi_hat)
  )
}

Q_spliced <- function(p) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  sapply(p, function(prob) {
    if (prob <= phi_u) {
      qlnorm(prob * F_body_at_u / phi_u, mu_body, sd_body)
    } else {
      p_gpd <- (prob - phi_u) / (1 - phi_u)
      u_chosen + evd::qgpd(p_gpd, loc = 0, scale = sigma_hat, shape = xi_hat)
    }
  })
}

r_spliced <- function(n_samp) {
  from_tail    <- runif(n_samp) > phi_u
  F_body_at_u  <- plnorm(u_chosen, mu_body, sd_body)
  x_body_draw  <- qlnorm(runif(n_samp) * F_body_at_u, mu_body, sd_body)
  x_tail_draw  <- u_chosen + evd::rgpd(n_samp, loc = 0, scale = sigma_hat, shape = xi_hat)
  ifelse(from_tail, x_tail_draw, x_body_draw)
}

# CDF comparison: empirical vs spliced vs full-data LogNormal
x_grid <- seq(min(x), quantile(x, 0.995), length.out = 800)
cdf_compare <- data.frame(
  x         = x_grid,
  Empirical = ecdf(x)(x_grid),
  Spliced   = F_spliced(x_grid),
  LogNormal = plnorm(x_grid, mean(log(x)), sd(log(x)))
)

p_cdf_full <- cdf_compare %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "CDF") %>%
  ggplot(aes(x = x, y = CDF, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  scale_colour_manual(values = c(Empirical = "#333333", Spliced = "#e31a1c", LogNormal = "#1f78b4")) +
  scale_linetype_manual(values = c(Empirical = "solid", Spliced = "solid", LogNormal = "dashed")) +
  geom_vline(xintercept = u_chosen, linetype = "dotted", colour = "grey40") +
  annotate("text", x = u_chosen * 1.05, y = 0.15, label = "threshold u", colour = "grey40", hjust = 0, size = 3.2) +
  labs(title = "CDF: Empirical vs Spliced vs LogNormal (Cargo)", x = "Claim Amount (log scale)", y = "F(x)") +
  theme_minimal(base_size = 12)

surv_compare <- cdf_compare %>%
  mutate(across(c(Empirical, Spliced, LogNormal), function(p) 1 - p)) %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "Survival") %>%
  filter(Survival > 0.001)

p_cdf_tail <- ggplot(surv_compare, aes(x = x, y = Survival, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = scales::percent_format(accuracy = 0.01)) +
  scale_colour_manual(values = c(Empirical = "#333333", Spliced = "#e31a1c", LogNormal = "#1f78b4")) +
  scale_linetype_manual(values = c(Empirical = "solid", Spliced = "solid", LogNormal = "dashed")) +
  labs(title = "Survival (tail zoom, log-log)", subtitle = "Spliced should track Empirical in the tail",
       x = "Claim Amount (log scale)", y = "P(X > x)") +
  theme_minimal(base_size = 12)

print(p_cdf_full / p_cdf_tail)


# ==============================================================================
# 6.  RISK MEASURES FROM THE SPLICED MODEL
# ==============================================================================
cat("\n=== Risk Measures (Spliced) ===\n")
p_levels <- c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 0.999)
var_vals  <- Q_spliced(p_levels)
cat(sprintf("\n%-14s  %14s  %14s\n", "Level", "VaR (Spliced)", "Empirical"))
for (i in seq_along(p_levels)) {
  emp_q <- as.numeric(quantile(x, p_levels[i]))
  cat(sprintf("%-14s  %14s  %14s\n", paste0("VaR(", 100*p_levels[i], "%)"), comma(round(var_vals[i])), comma(round(emp_q))))
}
if (xi_hat < 1) {
  cat("\nTVaR (closed-form, xi < 1):\n")
  tvar_closed <- function(p) { v <- Q_spliced(p); y0 <- v - u_chosen; v + (sigma_hat + xi_hat * y0) / (1 - xi_hat) }
  for (p in c(0.90, 0.95, 0.99, 0.995)) cat(sprintf("  TVaR(%.1f%%)  =  %s\n", 100*p, comma(round(tvar_closed(p)))))
} else {
  cat("\nTVaR: xi >= 1, use MC.\n")
}
set.seed(2026)
x_mc <- r_spliced(1e6)
cat("\nTVaR (MC, n=1e6):\n")
for (p in c(0.90, 0.95, 0.99, 0.995)) {
  v_mc <- quantile(x_mc, p); tvar_mc <- mean(x_mc[x_mc > v_mc])
  cat(sprintf("  TVaR(%.1f%%)  =  %s\n", 100*p, comma(round(tvar_mc))))
}


# ==============================================================================
# 7.  DISTRIBUTION COMPARISON - Which Single Distribution Fits Best?
# ==============================================================================
# Fit several candidates on the full data (scaled to $000 for stability).
# Compare AIC/BIC and goodness-of-fit; then compare to spliced in the tail.

scale_k <- 1000
y       <- x / scale_k
mom_shape <- mean(y)^2 / var(y)
mom_rate  <- mean(y) / var(y)

cat("\n=== Distribution Comparison (full data, $000 scale) ===\n")

fit_ln   <- fitdist(y, "lnorm")
fit_gam  <- fitdist(y, "gamma", start = list(shape = mom_shape, rate = mom_rate), lower = c(1e-4, 1e-8))
fit_wei  <- fitdist(y, "weibull", start = list(shape = 1, scale = mean(y)), lower = c(1e-3, 1e-3))
fit_llogis <- tryCatch(
  fitdist(y, "llogis", start = list(shape = 2, rate = 1 / median(y)), lower = c(1e-3, 1e-8)),
  error = function(e) NULL)
fit_burr <- tryCatch(
  fitdist(y, "burr", start = list(shape1 = 2, shape2 = 1, rate = 1 / median(y)), lower = c(1e-3, 1e-3, 1e-8)),
  error = function(e) NULL)

fits_all   <- list(LogNormal = fit_ln, Gamma = fit_gam, Weibull = fit_wei, LogLogistic = fit_llogis, Burr = fit_burr)
fits_valid <- Filter(Negate(is.null), fits_all)

aic_tbl <- data.frame(
  Distribution = names(fits_valid),
  AIC = sapply(fits_valid, function(f) f$aic),
  BIC = sapply(fits_valid, function(f) f$bic)
) %>% mutate(delta_AIC = AIC - min(AIC)) %>% arrange(AIC)
cat("\n--- AIC / BIC ---\n"); print(aic_tbl)

cat("\n--- Goodness-of-Fit (Anderson-Darling, CvM, KS) ---\n")
gof <- gofstat(fits_valid, fitnames = names(fits_valid))
print(gof)

par(mfrow = c(2, 2))
denscomp(fits_valid, legendtext = names(fits_valid), main = "Density ($000s)", xlab = "Claim ($000s)")
qqcomp(fits_valid, legendtext = names(fits_valid), main = "Q-Q")
cdfcomp(fits_valid, legendtext = names(fits_valid), main = "CDF", xlab = "Claim ($000s)")
ppcomp(fits_valid, legendtext = names(fits_valid), main = "P-P")
par(mfrow = c(1, 1))

# Tail Q-Q (top 20%): include Spliced
q_seq <- seq(0.80, 0.995, by = 0.001)
tail_qq <- data.frame(
  Empirical = as.numeric(quantile(y, q_seq)),
  LogNormal = qlnorm(q_seq, fit_ln$estimate["meanlog"], fit_ln$estimate["sdlog"])
)
if (!is.null(fit_burr)) tail_qq$Burr <- actuar::qburr(q_seq, shape1 = fit_burr$estimate["shape1"], shape2 = fit_burr$estimate["shape2"], rate = fit_burr$estimate["rate"])
tail_qq$Spliced <- Q_spliced(q_seq) / scale_k

p_tail_qq <- tail_qq %>%
  tidyr::pivot_longer(-Empirical, names_to = "Model", values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Tail Q-Q (top 20%, $000s)", subtitle = "Spliced should track diagonal best in the tail",
       x = "Theoretical", y = "Empirical") +
  theme_minimal(base_size = 12)
print(p_tail_qq)


# ==============================================================================
# 8.  SPLICED MODEL VALIDATION - Does the Spliced Model Work Well?
# ==============================================================================
# Checks: (1) P-P plot  (2) VaR backtest  (3) KS/AD tests  (4) PIT histogram

cat("\n=== Spliced Model Validation ===\n")

# 8.1  P-P plot: F_spliced(x_i) should be ~ Uniform(0,1)
u_pit <- F_spliced(x)
u_pit <- pmin(pmax(u_pit, 1e-6), 1 - 1e-6)  # avoid 0/1 for plotting
pp_df <- data.frame(theoretical = (1:length(u_pit)) / (length(u_pit) + 1), empirical = sort(u_pit))
p_pp <- ggplot(pp_df, aes(x = theoretical, y = empirical)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, colour = "#e31a1c", linewidth = 1) +
  labs(title = "P-P Plot: Spliced Model", subtitle = "F_spliced(X) vs Uniform; points on line = good fit",
       x = "Theoretical (Uniform)", y = "Empirical F_spliced(X)") +
  theme_minimal(base_size = 12)
print(p_pp)

# 8.2  VaR backtest: compare empirical quantiles to spliced VaR at several levels
cat("\n--- VaR backtest (spliced vs empirical) ---\n")
backtest_p <- c(0.90, 0.95, 0.99, 0.995)
for (p in backtest_p) {
  var_s <- Q_spliced(p)
  emp_q <- as.numeric(quantile(x, p))
  rel_err <- (var_s - emp_q) / emp_q
  cat(sprintf("p = %.3f:  Spliced VaR = %s  Empirical = %s  Rel diff = %+.2f%%\n", p, comma(round(var_s)), comma(round(emp_q)), 100 * rel_err))
}

# 8.3  KS and Anderson-Darling (spliced vs empirical)
# KS: H0 = data from F_spliced. AD is tail-weighted.
ks_spliced <- ks.test(u_pit, "punif", 0, 1)
cat(sprintf("\nKolmogorov-Smirnov: stat = %.4f  p-value = %.4f  (large p = consistent with spliced)\n", ks_spliced$statistic, ks_spliced$p.value))
# Simple AD-like: mean squared deviation of PIT from uniform
pit_sorted <- sort(F_spliced(x))
pit_sorted <- pmin(pmax(pit_sorted, 1e-6), 1 - 1e-6)
n_pit <- length(pit_sorted)
ad_approx <- -n_pit - mean((2 * (1:n_pit) - 1) * (log(pit_sorted) + log(1 - rev(pit_sorted))))
cat(sprintf("Anderson-Darling (approx): A2 = %.4f  (smaller = better fit)\n", ad_approx))

# 8.4  PIT histogram: F_spliced(X) should be Unif(0,1)
pit_hist_df <- data.frame(u = u_pit)
p_pit <- ggplot(pit_hist_df, aes(x = u)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#1f78b4", alpha = 0.7) +
  geom_hline(yintercept = 1, colour = "#e31a1c", linewidth = 1, linetype = "dashed") +
  labs(title = "PIT Histogram: F_spliced(X) (should be Uniform)",
       x = "F_spliced(X)", y = "Density") +
  theme_minimal(base_size = 12)
print(p_pit)

cat("\n--- Spliced validation summary ---\n")
cat("  P-P on line and VaR close to empirical = good. For large n, KS often rejects; prefer P-P and VaR backtest.\n")
