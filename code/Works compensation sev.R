# -----------------------------------------------------------------------------
# 0. 安装与加载所需包
# -----------------------------------------------------------------------------
pkgs <- c(
  "readxl",       # 读取 xlsx
  "dplyr",        # 数据处理
  "ggplot2",      # 可视化
  "MASS",         # glm.nb (负二项)
  "pscl",         # zeroinfl (零膨胀模型)
  "fitdistrplus", # 分布拟合与比较
  "actuar",       # 精算分布 (Burr, Pareto, etc.)
  "insurancerating", # 保险精算工具
  "car",          # 方差膨胀因子 VIF
  "lmtest",       # 似然比检验
  "ggfortify",    # GLM 诊断图
  "patchwork",    # 拼图
  "scales",       # 坐标轴格式化
  "gridExtra",     # 图表排列
  "glmnet",
  "zoo",
  "ismev",
  "evd"
)

# 自动安装缺失的包
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))

## Import dataset

freq_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_freq_cleaned.rds")
sev_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_sev_cleaned.rds")

sev_wc <- sev_raw[, -ncol(sev_raw)] # Delete the last column flag_high_claim_amount

# 1.  Descriptive Statistics & Shape Diagnostics
# ==============================================================================
# Before fitting anything, answer: what KIND of distribution does this look like?
# Key questions:
#   (a) Is it right-skewed? (almost always yes for claim data)
#   (b) Does log(x) look Normal? -> LogNormal
#   (c) How heavy is the tail? -> CV, skewness, mean excess plot

# 1.1 Look into the claim amount distribution and in different solar systems

x <- sev_wc$claim_amount          # positive numeric vector, length ~1,772
cat("n =", length(x), "\n")
cat("Any non-positive values?", any(x <= 0), "\n")   # must be FALSE

cat(sprintf("n          = %d\n",       length(x)))
cat(sprintf("mean       = %9.2f\n",   mean(x)))
cat(sprintf("median     = %9.2f\n",   median(x)))
cat(sprintf("sd         = %9.2f\n",   sd(x)))
cat(sprintf("CV         = %9.4f\n",   sd(x)/mean(x)))
cat(sprintf("skewness   = %9.4f\n",   mean(((x - mean(x))/sd(x))^3)))
cat(sprintf("ex.kurtosis= %9.4f\n",   mean(((x - mean(x))/sd(x))^4) - 3))
cat(sprintf("log_mu     = %9.4f\n",   mean(log(x))))
cat(sprintf("log_sigma  = %9.4f\n",   sd(log(x))))

summary(sev_wc$claim_amount) 
quantile(sev_wc$claim_amount, c(.5,.75,.9,.95,.99,.995,.999))

# From quantile we can tell the 99% is way higher than the median, implying heavy tail

hist(log(sev_wc$claim_amount), breaks=60, main="log(claim_amount)", xlab="")

# Histogram shows that the main body of the log(claim_amount) is normal, and there is a heavy tail and little bump

#check how concentrated the claim severity is in the tail.
sev_wc %>%
  summarise(
    total = sum(claim_amount),
    top1 = sum(claim_amount[claim_amount >= quantile(claim_amount, .99)]) / total,
    top5 = sum(claim_amount[claim_amount >= quantile(claim_amount, .95)]) / total
  )

#       total  top1  top5
#  14767744. 0.162 0.512
# first 1% top claim amounts sums to account for 16.2% of total claims!
# EVT is necessary

# Check whether these is major difference accross different solar systems
sev_wc %>%
  group_by(solar_system) %>%
  summarise(n=n(),
            mean=mean(claim_amount),
            med=median(claim_amount),
            p95=quantile(claim_amount,.95),
            p99=quantile(claim_amount,.99)) %>%
  arrange(desc(p99))

# 1.2 Tail EDA 

## QQ-plot:

p_qq_ln <- ggplot(sev_wc, aes(sample = log(claim_amount))) +
  stat_qq(alpha = 0.4, size = 0.9, colour = "#636363") +
  stat_qq_line(colour = "#d6604d", linewidth = 1) +
  labs(title = "Q-Q Plot: log(Claim Amount) vs Normal",
       subtitle = "Curvature at right tail = heavier than LogNormal",
       x = "Theoretical Normal quantiles",
       y = "Sample quantiles") +
  theme_minimal(base_size = 12)

# From QQ plot we can tell that there is indeed heavy tails 


# Survival plot
x <- sort(sev_wc$claim_amount)

surv <- 1 - ecdf(x)(x)

plot(log(x), log(surv),
     xlab="log(claim)",
     ylab="log(P(X>t))",
     main="Log-Log Survival Plot")


# Prevent MASS from masking dplyr verbs (run after all libraries loaded)
select <- dplyr::select
filter <- dplyr::filter

# Convenience: working vector of claim amounts (already cleaned)
x <- sev_wc$claim_amount
n <- length(x)

# ==============================================================================
# 2.  TAIL DIAGNOSTICS
# ==============================================================================
# Goal: decide HOW heavy the tail is, and which family is appropriate.
# Three complementary tools:
#   (a) Mean Excess Plot   — shape of conditional mean above threshold
#   (b) Hill estimator     — direct estimate of tail index as fn of k
#   (c) Log-log survival   — check power law

# ------------------------------------------------------------------------------
# 2.1  Mean Excess Plot (MEP)  /  Mean Residual Life Plot
# ------------------------------------------------------------------------------
# e(u) = E[X - u | X > u]
#
# Reading the MEP:
#   Slope > 0, roughly linear  ->  Pareto heavy tail  (GPD with xi > 0)
#   Hump shape (up then down)  ->  LogNormal body with moderate tail
#   Flat line                  ->  Exponential tail   (GPD with xi = 0)
#   Slope < 0                  ->  Bounded / Weibull  (GPD with xi < 0)
#
# From your earlier summary: top 1% accounts for 16.2% of total losses.
# This is a strong overdispersion signal.  We expect a rising MEP.

xs <- sort(x)

# Compute e(u) at each percentile from percentile 10 to percentile 97
# (avoid very high thresholds where n_above becomes too small to trust)
u_grid <- quantile(xs, seq(0.10, 0.97, by = 0.005))

mep_df <- data.frame(
  u           = as.numeric(u_grid),
  mean_excess = sapply(u_grid, function(u) mean(xs[xs > u] - u)),
  n_above     = sapply(u_grid, function(u) sum(xs > u))
)

# Add 95% CI for mean excess (based on CLT: SE = sd(excess)/sqrt(n_above))
mep_df <- mep_df %>%
  mutate(
    sd_excess = sapply(u, function(u) {
      exc <- xs[xs > u] - u
      if (length(exc) < 2) return(NA)
      sd(exc)
    }),
    se        = sd_excess / sqrt(n_above),
    ci_lo     = mean_excess - 1.96 * se,
    ci_hi     = mean_excess + 1.96 * se
  )

p_mep <- ggplot(mep_df, aes(x = u, y = mean_excess)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              alpha = 0.15, fill = "#7b2d8b") +
  geom_line(colour = "#7b2d8b", linewidth = 1) +
  # Mark common threshold candidates
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
    title    = "Mean Excess Plot (MEP)",
    subtitle = paste0(
      "Monotonically rising -> Pareto-type tail (GPD xi > 0)\n",
      "Shaded band = 95% CI for e(u)"
    ),
    x = "Threshold  u",
    y = "e(u) = E[X - u | X > u]"
  ) +
  theme_minimal(base_size = 12)

print(p_mep)

# While we observe that when u is above 25000, the MEP drops, this is probably becasue not many sample.

#The plot stabilizes around the 90th percentile, suggesting that thresholds near this region provide a reasonable starting point for tail modeling.

# Candidate u: 16373.2020

# ------------------------------------------------------------------------------
# 2.2  Hill Estimator  — direct tail index estimation
# ------------------------------------------------------------------------------
# For a heavy-tailed distribution, the tail behaves like:
#   P(X > x) ~ C * x^{-alpha}   (power law)
#
# The Hill estimator uses the top k order statistics to estimate alpha:
#   alpha_Hill(k) = 1 / [ (1/k) * sum_{i=1}^{k} log(X_{n-i+1}) - log(X_{n-k}) ]
#
# Then:  xi_Hill = 1 / alpha_Hill   (the GPD shape parameter)
#
# The Hill plot shows xi_Hill as a function of k (number of upper order stats).
# You look for a STABLE PLATEAU — the region where the estimate stops changing
# as k increases.  That plateau value is your estimate of xi.
# If the plot never stabilises, the tail may not be purely Pareto.


### Hill plot is used to estimate the tail index xi and nd identify a stable region in the largest observations where the tail behaves like a Pareto distribution.
# However, the optimal fit may not be Pareto distribution

xs_desc <- sort(x, decreasing = TRUE)   # descending order statistics

# Compute Hill estimate for each k from 5 to n/5
k_seq     <- 5:floor(n / 5)
xi_hill   <- sapply(k_seq, function(k) {
  # Mean of log-spacings above the k-th largest value
  mean(log(xs_desc[1:k])) - log(xs_desc[k + 1])
})
library(zoo)

hill_df <- data.frame(k = k_seq, xi = xi_hill)
# -----------------------------
# Smooth the Hill estimates
# -----------------------------
smooth_k <- 30

hill_df$xi_smooth <- rollmean(hill_df$xi, k = smooth_k, fill = NA, align = "center")

# -----------------------------
# Find plateau region
# -----------------------------
diff_xi <- abs(diff(hill_df$xi_smooth))

# smooth the differences to stabilize
diff_smooth <- rollmean(diff_xi, k = smooth_k, fill = NA, align = "center")

plateau_idx <- which.min(diff_smooth)

# map back to xi value
xi_plateau <- hill_df$xi_smooth[plateau_idx]

# corresponding k
k_plateau <- hill_df$k[plateau_idx]

# -----------------------------
# Plot Hill curve
# -----------------------------
p_hill <- ggplot(hill_df, aes(x = k, y = xi)) +
  
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  
  geom_line(aes(y = xi_smooth),
            colour = "orange",
            linewidth = 1) +
  
  geom_hline(yintercept = xi_plateau,
             linetype = "dashed",
             colour = "#e31a1c") +
  
  geom_vline(xintercept = k_plateau,
             linetype = "dashed",
             colour = "grey40") +
  
  annotate("text",
           x = max(k_seq) * 0.65,
           y = xi_plateau * 1.05,
           label = sprintf("Plateau ξ ≈ %.3f", xi_plateau),
           colour = "#e31a1c",
           size = 3.5) +
  
  labs(
    title = "Hill Plot — Tail Index Estimation",
    subtitle = "Blue: raw Hill estimates | Orange: smoothed estimates",
    x = "k (number of upper order statistics)",
    y = expression(hat(xi)[Hill](k))
  ) +
  
  theme_minimal(base_size = 12)

print(p_hill)

# From the hill plot we find the plateau when k is above 279, the plot is flat, indicating that from the here, the tail behaves like a Pareto distribution


# -----------------------------
# Print summary
# -----------------------------
cat("\n--- Hill Plateau Estimate ---\n")
cat(sprintf("Plateau k ≈ %d\n", k_plateau)) # 279
cat(sprintf("Estimated xi ≈ %.4f\n", xi_plateau)) #1.1697


# ==============================================================================
# 3.  THRESHOLD SELECTION — GPD STABILITY PLOTS
# ==============================================================================
# The Pickands-Balkema-de Haan theorem says: for u large enough,
#   (X - u | X > u)  ~  GPD(xi, sigma_u)
#
# KEY PROPERTY: if the GPD holds above u_0, then for any u > u_0:
#   xi    is CONSTANT  (does not depend on u)
#   sigma*(u) = sigma(u) - xi * u  is CONSTANT  (modified scale)
#
# So the stability plots show:
#   x-axis: threshold u
#   y-axis: estimated xi  (should flatline above the true threshold)
#   y-axis: estimated sigma* (should also flatline)
#
# Choose the LOWEST u where BOTH lines are approximately flat.
# Lower u = more data in tail = more precise estimates.
# Higher u = better GPD approximation = less bias.
# Trade-off: bias vs variance.

cat("\n=== Threshold Selection via Stability Plots ===\n")

# Fit GPD at each threshold in the grid
# We use ismev::gpd.fit which uses MLE with numerical optimisation
u_candidates <- quantile(x, seq(0.60, 0.95, by = 0.01))

stab_list <- lapply(u_candidates, function(u) {
  exc <- x[x > u] - u
  # Require at least 30 exceedances for reliable estimation
  if (length(exc) < 30) return(NULL)
  tryCatch({
    fit        <- ismev::gpd.fit(x, threshold = u, show = FALSE)
    sigma      <- fit$mle[1]
    xi         <- fit$mle[2]
    # Modified scale: sigma* = sigma - xi*u
    # Under the true GPD model, sigma*(u) is constant in u
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

# Mark our three candidate thresholds on the plot
thresh_labels <- data.frame(
  u     = as.numeric(quantile(x, c(0.75, 0.85, 0.90))),
  label = c("p75", "p85", "p90"),
  col   = c("#e31a1c", "#ff7f00", "#33a02c")
)

p_stab_xi <- ggplot(stab_df, aes(x = u, y = xi)) +
  # 95% confidence band
  geom_ribbon(aes(ymin = xi - 1.96 * se_xi,
                  ymax = xi + 1.96 * se_xi),
              alpha = 0.20, fill = "#1f78b4") +
  geom_line(colour = "#1f78b4", linewidth = 1) +
  geom_vline(xintercept = thresh_labels$u,
             linetype = "dashed",
             colour   = thresh_labels$col) +
  annotate("text",
           x     = thresh_labels$u,
           y     = max(stab_df$xi + 1.96 * stab_df$se_xi) * 0.95,
           label = thresh_labels$label,
           colour = thresh_labels$col,
           hjust  = -0.15, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "GPD Stability Plot — Shape Parameter xi",
    subtitle = "xi should be constant for u above the true threshold\nChoose lowest u where the line stabilises within the CI band",
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
             linetype = "dashed",
             colour   = thresh_labels$col) +
  annotate("text",
           x     = thresh_labels$u,
           y     = max(stab_df$sigma_star + 1.96 * stab_df$se_sigma) * 0.95,
           label = thresh_labels$label,
           colour = thresh_labels$col,
           hjust  = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma) +
  labs(
    title    = "GPD Stability Plot — Modified Scale sigma*",
    subtitle = expression(sigma^"*"(u) == sigma(u) - xi %.% u ~~ "should be constant above true threshold"),
    x = "Threshold  u",
    y = expression(sigma^"*"(u))
  ) +
  theme_minimal(base_size = 12)

# Print both stability plots stacked
print(p_stab_xi / p_stab_sig)

# Print table of estimates at key candidate thresholds
cat("\nEstimates at candidate thresholds:\n")
cat(sprintf("%-6s  %-10s  %-8s  %-8s  %-8s  %-8s  %s\n",
            "Level", "u", "sigma", "xi", "SE_sig", "SE_xi", "n_excess"))
for (pct in c(0.75, 0.85, 0.90)) {
  u_v <- as.numeric(quantile(x, pct))
  row <- stab_df[which.min(abs(stab_df$u - u_v)), ]
  cat(sprintf("p%-5.0f  %-10s  %-8.3f  %-8.4f  %-8.4f  %-8.4f  %d\n",
              100*pct,
              comma(round(u_v)),
              row$sigma, row$xi, row$se_sigma, row$se_xi,
              row$n_excess))
}


#The stability plots show that the estimated shape parameter \xi and modified scale \sigma^*
# become approximately constant for thresholds above the 90th percentile. Therefore, a threshold near the 90th percentile is selected for the GPD tail modelling.

u <- quantile(x,0.9) # 16373
xi_op <-  quantile(row$xi,0.9) # \xi = -0.16176
 
# ==============================================================================
# 4.  GPD FITTING AT CHOSEN THRESHOLD
# ==============================================================================
# After inspecting the stability plots:
#   - Choose the LOWEST threshold where xi and sigma* are both stable
#   - Common starting point: p90.  Adjust downward if stable earlier.
#
# We fit at p85 and p90 and compare.  Use the one where:
#   (a) xi estimates are similar between thresholds (stability confirmed)
#   (b) The GPD diagnostic plots look clean
#   (c) There are at least ~50-100 exceedances

# --- Set your chosen threshold here (adjust after inspecting Section 3 plots) ---
u_chosen <- quantile(x, 0.9)     # primary: p90
u_alt    <- quantile(x, 0.92)     # sensitivity check: p92

cat(sprintf("\n=== GPD Fitting at u = p90 = %s ===\n", comma(round(u_chosen))))
cat(sprintf("    (sensitivity at u = p92 = %s)\n",    comma(round(u_alt))))

# Fit primary GPD
gpd_primary <- ismev::gpd.fit(x, threshold = u_chosen, show = FALSE)
gpd_alt     <- ismev::gpd.fit(x, threshold = u_alt,    show = FALSE)

# Extract parameters
xi_hat    <- gpd_primary$mle[2]     # shape:  > 0 = heavy tail
sigma_hat <- gpd_primary$mle[1]     # scale:  spread of exceedances
se_xi     <- gpd_primary$se[2]
se_sigma  <- gpd_primary$se[1]
n_excess  <- sum(x > u_chosen)
n_total   <- length(x)
phi_u     <- 1 - n_excess / n_total  # fraction of data BELOW threshold

cat("\n--- Primary GPD Parameter Estimates ---\n")
cat(sprintf("Threshold u   = %s  (p90)\n", comma(round(u_chosen))))
cat(sprintf("n_excess      = %d  (%.1f%% of claims above threshold)\n",
            n_excess, 100 * (1 - phi_u)))
cat(sprintf("xi  (shape)   = %6.4f  (95%% CI: [%.4f, %.4f])\n",
            xi_hat, xi_hat - 1.96*se_xi, xi_hat + 1.96*se_xi))
cat(sprintf("sigma (scale) = %6.4f  (95%% CI: [%.4f, %.4f])\n",
            sigma_hat, sigma_hat - 1.96*se_sigma, sigma_hat + 1.96*se_sigma))

# Interpret xi
cat("\n--- Interpretation of xi ---\n")
if (xi_hat > 0) {
  cat(sprintf("xi = %.4f > 0  ->  Heavy Pareto-type tail\n", xi_hat))
  cat(sprintf("Finite moments exist only for order < 1/xi = %.2f\n", 1/xi_hat))
  cat("Specifically:\n")
  cat(sprintf("  Mean (1st moment) exists?  %s\n", ifelse(1/xi_hat > 1, "YES","NO")))
  cat(sprintf("  Variance (2nd moment)?     %s\n", ifelse(1/xi_hat > 2, "YES","NO")))
  cat(sprintf("  Skewness (3rd moment)?     %s\n", ifelse(1/xi_hat > 3, "YES","NO")))
} else if (xi_hat == 0) {
  cat("xi = 0  ->  Exponential tail (all moments exist)\n")
} else {
  cat(sprintf("xi = %.4f < 0  ->  Bounded tail (finite upper endpoint)\n", xi_hat))
  cat(sprintf("Upper endpoint = u - sigma/xi = %s\n",
              comma(round(u_chosen - sigma_hat / xi_hat))))
}

# Weibull: xi = -0.1618 < 0  ->  Bounded tail (finite upper endpoint)
#Upper endpoint = u - sigma/xi = 290,526

cat("\n--- Sensitivity: p90 vs p92 ---\n")
cat(sprintf("p92: xi = %.4f  sigma = %.4f  n_excess = %d\n",
            gpd_alt$mle[2], gpd_alt$mle[1], sum(x > u_alt)))
cat(sprintf("p90: xi = %.4f  sigma = %.4f  n_excess = %d\n",
            xi_hat, sigma_hat, n_excess))
cat("If xi estimates are close -> threshold choice is stable\n")

# --- GPD Built-in Diagnostic Plots ---
# Four plots from ismev:
#   (a) Probability plot  — should be close to diagonal if GPD fits
#   (b) Quantile plot     — same idea on quantile scale
#   (c) Return level plot — extrapolated return levels with CI
#   (d) Density           — fitted GPD density vs histogram of exceedances
par(mfrow = c(2, 2))
ismev::gpd.diag(gpd_primary)
mtext(sprintf("GPD Diagnostics  (u = p90 = %s)", comma(round(u_chosen))),
      outer = TRUE, cex = 1.1, line = -1.5)
par(mfrow = c(1, 1))

# Results show GPD fits well

# --- Additional: manual Q-Q plot for exceedances (ggplot version) ---
exceedances <- sort(x[x > u_chosen] - u_chosen)  # excess values Y = X - u
n_exc       <- length(exceedances)

# Theoretical GPD quantiles at empirical plotting positions
p_plot   <- (1:n_exc) / (n_exc + 1)
q_theory <- evd::qgpd(p_plot, loc = 0, scale = sigma_hat, shape = xi_hat)

qq_exc_df <- data.frame(
  empirical   = exceedances,
  theoretical = q_theory
)

p_qq_exc <- ggplot(qq_exc_df, aes(x = theoretical, y = empirical)) +
  geom_point(alpha = 0.5, size = 1.5, colour = "#1f78b4") +
  geom_abline(slope = 1, intercept = 0,
              colour = "#e31a1c", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Q-Q Plot: Exceedances vs Fitted GPD",
    subtitle = sprintf("u = %s  |  xi = %.4f  |  sigma = %.4f",
                       comma(round(u_chosen)), xi_hat, sigma_hat),
    x = "Theoretical GPD quantile",
    y = "Empirical exceedance"
  ) +
  theme_minimal(base_size = 12)

print(p_qq_exc)

# QQ plot also shows that the GPD is a good fit for larger-than-threshold losses

# ==============================================================================
# 5.  SPLICED MODEL — LogNormal Body + GPD Tail
# ==============================================================================
# Why splice?
#   A single distribution (e.g. LogNormal) fits the body well but
#   underestimates the tail.  GPD is only valid ABOVE the threshold.
#   Splicing combines both: exact fit in the body, theoretically
#   justified extrapolation in the tail.
#
# The spliced CDF is:
#
#   F(x) = phi_u * F_body(x) / F_body(u)        for x <= u
#   F(x) = phi_u + (1-phi_u) * G_GPD(x - u)     for x >  u
#
# where:
#   phi_u  = P(X <= u) = n_below / n   (empirical mixing weight)
#   F_body = LogNormal(mu, sigma) fitted to claims AT OR BELOW u
#   G_GPD  = GPD(xi, sigma) with parameters from Section 4
#
# This is a 2-component mixture, NOT a convex combination of full distributions.
# The body and tail each own their region of the support.

cat("\n=== Building Spliced Model ===\n")

# --- 5.1  Fit LogNormal to the body (claims at or below threshold) ---
x_body   <- x[x <= u_chosen]
fit_body <- fitdist(x_body, "lnorm")     # MLE on body data only
mu_body  <- fit_body$estimate["meanlog"]
sd_body  <- fit_body$estimate["sdlog"]

cat(sprintf("Body LogNormal: mu = %.4f, sigma = %.4f  [n = %d, %.1f%% of data]\n",
            mu_body, sd_body, length(x_body), 100 * phi_u))
# Body LogNormal: mu = 7.4918, sigma = 0.9348  [n = 1674, 90.0% of data]
cat(sprintf("Tail GPD:       xi = %.4f, sigma = %.4f  [n = %d, %.1f%% of data]\n",
            xi_hat, sigma_hat, n_excess, 100 * (1 - phi_u)))
# Tail GPD:       xi = -0.1618, sigma = 44348.6479  [n = 186, 10.0% of data]


# --- 5.2  Spliced functions ---

# CDF:  F(q)
F_spliced <- function(q) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  ifelse(
    q <= u_chosen,
    # Rescale body CDF to [0, phi_u]
    phi_u * plnorm(q, mu_body, sd_body) / F_body_at_u,
    # Tail: phi_u + (1-phi_u) * GPD CDF of excess
    phi_u + (1 - phi_u) *
      evd::pgpd(q - u_chosen, loc = 0, scale = sigma_hat, shape = xi_hat)
  )
}

# This gives us the fitted distribution

# Quantile function:  Q(p)  — needed for VaR
Q_spliced <- function(p) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  sapply(p, function(prob) {
    if (prob <= phi_u) {
      # Invert the rescaled body CDF
      qlnorm(prob * F_body_at_u / phi_u, mu_body, sd_body)
    } else {
      # Invert the GPD part
      p_gpd <- (prob - phi_u) / (1 - phi_u)
      u_chosen + evd::qgpd(p_gpd, loc = 0, scale = sigma_hat, shape = xi_hat)
    }
  })
}

# Random sampler:  r_spliced(n)
r_spliced <- function(n_samp) {
  # Each draw: with prob phi_u from body, else from tail
  from_tail    <- runif(n_samp) > phi_u
  F_body_at_u  <- plnorm(u_chosen, mu_body, sd_body)
  # Body draws: inverse-CDF of truncated LogNormal
  x_body_draw  <- qlnorm(runif(n_samp) * F_body_at_u, mu_body, sd_body)
  # Tail draws: shift GPD by threshold
  x_tail_draw  <- u_chosen +
    evd::rgpd(n_samp, loc = 0, scale = sigma_hat, shape = xi_hat)
  ifelse(from_tail, x_tail_draw, x_body_draw)
}

# --- 5.3  Validate: compare empirical vs spliced CDF ---
# Grid from min to p99.5 (avoid extreme extrapolation for visual)
x_grid <- seq(min(x), quantile(x, 0.995), length.out = 800)

cdf_compare <- data.frame(
  x         = x_grid,
  Empirical = ecdf(x)(x_grid),
  Spliced   = F_spliced(x_grid),
  # Also compare to naive full-data LogNormal for reference
  LogNormal = plnorm(x_grid, mean(log(x)), sd(log(x)))
)

# Full CDF comparison
p_cdf_full <- cdf_compare %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "CDF") %>%
  ggplot(aes(x = x, y = CDF, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  scale_colour_manual(values = c(Empirical = "#333333",
                                 Spliced   = "#e31a1c",
                                 LogNormal = "#1f78b4")) +
  scale_linetype_manual(values = c(Empirical = "solid",
                                   Spliced   = "solid",
                                   LogNormal = "dashed")) +
  geom_vline(xintercept = u_chosen, linetype = "dotted",
             colour = "grey40") +
  annotate("text", x = u_chosen * 1.05, y = 0.15,
           label = "threshold u", colour = "grey40",
           hjust = 0, size = 3.2) +
  labs(title = "CDF Comparison: Empirical vs Spliced vs LogNormal",
       x = "Claim Amount (log scale)", y = "F(x)") +
  theme_minimal(base_size = 12)

# Tail zoom: survival function on log-log scale
# This is the most sensitive view for tail misfit
surv_compare <- cdf_compare %>%
  mutate(across(c(Empirical, Spliced, LogNormal), function(p) 1 - p)) %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "Survival") %>%
  filter(Survival > 0.001)   # avoid log(0)

p_cdf_tail <- ggplot(surv_compare,
                     aes(x = x, y = Survival,
                         colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = percent_format(accuracy = 0.01)) +
  scale_colour_manual(values = c(Empirical = "#333333",
                                 Spliced   = "#e31a1c",
                                 LogNormal = "#1f78b4")) +
  scale_linetype_manual(values = c(Empirical = "solid",
                                   Spliced   = "solid",
                                   LogNormal = "dashed")) +
  labs(
    title    = "Survival Function — Tail Zoom (log-log scale)",
    subtitle = "Spliced (red) should track Empirical (black) closely in the tail\nLogNormal (blue dashed) typically underestimates the tail",
    x = "Claim Amount (log scale)",
    y = "P(X > x)  (log scale)"
  ) +
  theme_minimal(base_size = 12)

print(p_cdf_full / p_cdf_tail)

# The graph shows that spliced model fits the empirical distribution closely across the entire range of claim sizes, while the single LogNormal model substantially underestimates the tail probabilities.



# ==============================================================================
# 6.  RISK MEASURES FROM THE SPLICED MODEL
# ==============================================================================
# VaR(p)  = Q_spliced(p)                   — the p-th quantile
# TVaR(p) = E[X | X > VaR(p)]              — expected loss given exceedance
#
# For the GPD tail, TVaR has a closed-form formula when xi < 1:
#   TVaR(p) = VaR(p) + (sigma + xi*(VaR(p) - u)) / (1 - xi)
#
# We also provide a Monte Carlo estimate as a sanity check.

cat("\n=== Risk Measures from Spliced Model ===\n")

# VaR at standard actuarial levels
p_levels <- c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 0.999)
var_vals  <- Q_spliced(p_levels)

cat(sprintf("\n%-12s  %12s  %12s\n",
            "Level", "VaR ($)", "vs. Empirical p"))
for (i in seq_along(p_levels)) {
  emp_q <- as.numeric(quantile(x, p_levels[i]))
  cat(sprintf("%-12s  %12s  %12s\n",
              paste0("VaR(", 100*p_levels[i], "%)"),
              comma(round(var_vals[i])),
              comma(round(emp_q))))
}

# TVaR — closed form (valid when xi < 1)
cat("\n")
if (xi_hat < 1) {
  cat("TVaR — closed-form GPD formula (valid because xi < 1):\n")
  # For x > u: TVaR(p) = VaR(p) + E[Y | Y > VaR(p)-u]
  # where Y ~ GPD, E[Y | Y > y0] = (sigma + xi*y0)/(1-xi)
  tvar_closed <- function(p) {
    v    <- Q_spliced(p)
    y0   <- v - u_chosen              # excess above threshold
    v + (sigma_hat + xi_hat * y0) / (1 - xi_hat)
  }
  for (p in c(0.90, 0.95, 0.99, 0.995)) {
    cat(sprintf("  TVaR(%.1f%%)  =  %s\n",
                100*p, comma(round(tvar_closed(p)))))
  }
} else {
  cat("WARNING: xi >= 1, TVaR closed form not valid. Use MC below.\n")
}

# TVaR — Monte Carlo verification
set.seed(2026)
n_mc     <- 1e6
x_mc     <- r_spliced(n_mc)

cat("\nTVaR — Monte Carlo verification (N = 1,000,000):\n")
for (p in c(0.90, 0.95, 0.99, 0.995)) {
  v_mc    <- quantile(x_mc, p)
  tvar_mc <- mean(x_mc[x_mc > v_mc])
  cat(sprintf("  TVaR(%.1f%%)  =  %s\n", 100*p, comma(round(tvar_mc))))
}

# Concentration measures
cat("\n--- Loss Concentration ---\n")

conc <- data.frame(x = x_mc) %>%
  summarise(
    total_loss = sum(x),
    top1pct    = sum(x[x >= quantile(x, 0.99)]) / total_loss,
    top5pct    = sum(x[x >= quantile(x, 0.95)]) / total_loss
  )

cat(sprintf("Top 1%% of claims account for %.1f%% of total loss (MC)\n",
            conc$top1pct * 100))


# ==============================================================================
# 7.  UNCONDITIONAL MLE FITS — Body Distributions for Comparison
# ==============================================================================
# These parametric fits cover the WHOLE distribution (not just the tail).
# Useful for:
#   (a) Comparing AIC across families to find the "best" single distribution
#   (b) Feeding into the GLM (Gamma or LogNormal are GLM-compatible)
#   (c) Showing WHY a spliced model is needed (pure fits underestimate tail)
#
# We scale to $1,000 units for numerical stability in optim().

scale_k <- 1000
y       <- x / scale_k    # work in $000s

# Method-of-moments starting values (prevent optim failure)
mom_shape <- mean(y)^2 / var(y)
mom_rate  <- mean(y)   / var(y)

cat("\n=== Unconditional MLE Fits (on $000 scale) ===\n")

# LogNormal — no starting values needed (closed-form MLE)
fit_ln <- fitdist(y, "lnorm")

# Gamma — provide MoM starting values
fit_gam <- fitdist(y, "gamma",
                   start = list(shape = mom_shape, rate = mom_rate),
                   lower = c(1e-4, 1e-8))

# Weibull
fit_wei <- fitdist(y, "weibull",
                   start = list(shape = 1, scale = mean(y)),
                   lower = c(1e-3, 1e-3))

# Log-Logistic (actuar) — heavier tail than LogNormal
fit_llogis <- tryCatch(
  fitdist(y, "llogis",
          start = list(shape = 2, rate = 1 / median(y)),
          lower = c(1e-3, 1e-8)),
  error = function(e) { message("LogLogistic failed"); NULL }
)

# Burr XII (actuar) — most flexible, 3 parameters
# F(x) = 1 - (1 + (x/scale)^shape1)^{-shape2}
fit_burr <- tryCatch(
  fitdist(y, "burr",
          start = list(shape1 = 2, shape2 = 1, rate = 1 / median(y)),
          lower = c(1e-3, 1e-3, 1e-8)),
  error = function(e) { message("Burr failed"); NULL }
)

# Collect valid fits
fits_all   <- list(LogNormal   = fit_ln,
                   Gamma       = fit_gam,
                   Weibull     = fit_wei,
                   LogLogistic = fit_llogis,
                   Burr        = fit_burr)
fits_valid <- Filter(Negate(is.null), fits_all)

# --- 7.1  AIC / BIC table ---
cat("\n--- AIC / BIC Comparison ---\n")
aic_tbl <- data.frame(
  Distribution = names(fits_valid),
  AIC          = sapply(fits_valid, function(f) f$aic),
  BIC          = sapply(fits_valid, function(f) f$bic)
) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>%
  arrange(AIC)
print(aic_tbl)

# --- 7.2  Anderson-Darling GoF (tail-sensitive) ---
cat("\n--- Goodness-of-Fit (Anderson-Darling — more sensitive to tail) ---\n")
gof <- gofstat(fits_valid, fitnames = names(fits_valid))
print(gof)

# --- 7.3  Four-panel diagnostic plot ---
par(mfrow = c(2, 2))
denscomp(fits_valid,
         legendtext = names(fits_valid),
         main = "Density Comparison ($000s)",
         xlab = "Claim Amount ($000s)")
qqcomp  (fits_valid,
         legendtext = names(fits_valid),
         main = "Q-Q Comparison")
cdfcomp (fits_valid,
         legendtext = names(fits_valid),
         main = "CDF Comparison",
         xlab = "Claim Amount ($000s)")
ppcomp  (fits_valid,
         legendtext = names(fits_valid),
         main = "P-P Comparison")
par(mfrow = c(1, 1))

# --- 7.4  Tail Q-Q: zoom into top 20% ---
# Standard diagnostic plots look at the whole range.
# This zoomed version focuses on the top 20% where tail fit matters most.
q_seq <- seq(0.80, 0.995, by = 0.001)

tail_qq <- data.frame(
  Empirical   = as.numeric(quantile(y, q_seq)),
  LogNormal   = qlnorm(q_seq,
                       fit_ln$estimate["meanlog"],
                       fit_ln$estimate["sdlog"])
)
if (!is.null(fit_burr))
  tail_qq$Burr <- qburr(q_seq,
                        shape1 = fit_burr$estimate["shape1"],
                        shape2 = fit_burr$estimate["shape2"],
                        rate   = fit_burr$estimate["rate"])

# Add spliced model quantiles (converted back to $000s)
tail_qq$Spliced <- Q_spliced(q_seq) / scale_k

p_tail_qq <- tail_qq %>%
  tidyr::pivot_longer(-Empirical,
                      names_to  = "Model",
                      values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Tail Q-Q Plot — Top 20% of Claims ($000s)",
    subtitle = "Points BELOW the diagonal -> model underestimates the tail\nSpliced should track the diagonal most closely",
    x = "Theoretical quantile ($000s)",
    y = "Empirical quantile ($000s)"
  ) +
  theme_minimal(base_size = 12)

print(p_tail_qq)

# From the plot, we can tell that Spliced model fits the best

# ==============================================================================
# 8.  FINAL SUMMARY
# ==============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("SEVERITY FITTING SUMMARY\n")
cat(strrep("=", 65), "\n\n", sep = "")

# ------------------------------------------------------------------
# 8.1 Data summary
# ------------------------------------------------------------------

cat(sprintf(
  "Data summary: n = %d | mean = %s | CV = %.3f\n\n",
  n,
  scales::comma(round(mean(x))),
  sd(x)/mean(x)
))

# ------------------------------------------------------------------
# 8.2 Tail diagnostics
# ------------------------------------------------------------------

cat("--- Tail Diagnostics ---\n")

cat("Mean Excess Plot: increasing mean excess -> Pareto-type heavy tail\n")

cat(sprintf(
  "Hill plot (plateau region): xi ≈ %.3f\n",
  xi_plateau
))

cat("\n")

# ------------------------------------------------------------------
# 8.3 GPD tail model
# ------------------------------------------------------------------

cat("--- Chosen GPD Tail Model ---\n")

cat(sprintf(
  "Threshold u = %s (p90)\n",
  scales::comma(round(u_chosen))
))

cat(sprintf(
  "xi    = %.4f  (SE: %.4f)  -> %s\n",
  xi_hat,
  se_xi,
  ifelse(xi_hat > 0, "Heavy tail confirmed", "Light tail")
))

cat(sprintf(
  "sigma = %.4f  (SE: %.4f)\n",
  sigma_hat,
  se_sigma
))

cat("\n")

# ------------------------------------------------------------------
# 8.4 VaR results
# ------------------------------------------------------------------

cat("--- Spliced Model Risk Measures ---\n")

for (i in seq_along(p_levels)) {
  
  cat(sprintf(
    "VaR(%4.1f%%) = %s\n",
    100 * p_levels[i],
    scales::comma(round(var_vals[i]))
  ))
  
}

cat("\n")

# ------------------------------------------------------------------
# 8.5 TVaR via Monte Carlo
# ------------------------------------------------------------------

set.seed(2026)

x_mc <- r_spliced(1e6)

var99 <- quantile(x_mc, 0.99)

tvar99 <- mean(x_mc[x_mc > var99])

cat(sprintf(
  "TVaR(99%%) = %s  [Monte Carlo, N = 1,000,000]\n",
  scales::comma(round(tvar99))
))

cat("\n")

# ------------------------------------------------------------------
# 8.6 Best single distribution comparison
# ------------------------------------------------------------------

cat("--- Best Unconditional MLE Fit ---\n")

cat(sprintf(
  "Best distribution by AIC: %s\n",
  aic_tbl$Distribution[1]
))

cat(
  "Note: For GLM severity modelling use Gamma or LogNormal family.\n"
)

cat(
  "      GPD is used for tail modelling and simulation, not directly in GLM.\n"
)

cat("\n", strrep("=", 65), "\n", sep = "")

# If we can only use one distribution that fits the severity, then burr distribution is the best.