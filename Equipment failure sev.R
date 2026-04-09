# ==============================================================================
# Equipment Failure — Severity Modelling
# ==============================================================================
# Group A (Quantum Bore): Spliced = truncated parametric body + empirical
#   bootstrap tail. Body fitted on x <= u*; tail = resample of x > u*.
#   (No GPD; no full-scale draw that can exceed tail.)
# Group B (Other Equipment): Best parametric fit (e.g. LogNormal).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------------------------
pkgs <- c(
  "dplyr", "ggplot2", "fitdistrplus", "actuar",
  "patchwork", "scales", "zoo", "ismev", "evd", "tidyr"
)
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

select <- dplyr::select
filter <- dplyr::filter

# ------------------------------------------------------------------------------
# 1. Import & clean
# ------------------------------------------------------------------------------
# path_ef_sev <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/ef_sev_cleaned.rds"
path_ef_sev <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/ef_sev_cleaned.rds"

sev_raw <- readRDS(path_ef_sev)

sev_raw <- sev_raw %>%
  mutate(
    equipment_type = gsub("_\\d+$", "", equipment_type),
    solar_system   = gsub("_\\d+$", "", solar_system)
  )
if (is.character(sev_raw$equipment_type))
  sev_raw$equipment_type <- sub("_.*$", "", sev_raw$equipment_type)
if (is.character(sev_raw$solar_system))
  sev_raw$solar_system   <- sub("_.*$", "", sev_raw$solar_system)

cat("Rows:", nrow(sev_raw), "\n")
cat("Columns:", ncol(sev_raw), "\n")
cat("Any non-positive claim_amount?", any(sev_raw$claim_amount <= 0, na.rm = TRUE), "\n")
cat("Any NA in claim_amount?",        any(is.na(sev_raw$claim_amount)), "\n")
cat("\nEquipment types (after cleaning):\n")
print(table(sev_raw$equipment_type))

# ------------------------------------------------------------------------------
# 2. Split groups and severity vectors
# ------------------------------------------------------------------------------
sev_qb    <- sev_raw %>% filter(equipment_type == "Quantum Bore")
sev_other <- sev_raw %>% filter(equipment_type != "Quantum Bore")

x_a  <- sev_qb$claim_amount
x_a  <- x_a[is.finite(x_a) & x_a > 0]
n_a  <- length(x_a)
xs_a <- sort(x_a)

x_b  <- sev_other$claim_amount
x_b  <- x_b[is.finite(x_b) & x_b > 0]
n_b  <- length(x_b)
xs_b <- sort(x_b)

cat(sprintf("\nGroup A — Quantum Bore:  n = %d\n", n_a))
cat(sprintf("Group B — Other types:   n = %d\n", n_b))

# Cross-group comparison
sev_raw %>%
  mutate(group = ifelse(equipment_type == "Quantum Bore", "A: Quantum Bore", "B: Other")) %>%
  group_by(group) %>%
  summarise(n      = n(),
            mean   = mean(claim_amount),
            median = median(claim_amount),
            cv     = sd(claim_amount) / mean(claim_amount),
            p90    = quantile(claim_amount, .90),
            p99    = quantile(claim_amount, .99),
            max    = max(claim_amount),
            .groups = "drop") %>%
  print()

# Within Group B breakdown
cat("\n--- Group B breakdown by equipment_type ---\n")
sev_other %>%
  group_by(equipment_type) %>%
  summarise(n      = n(),
            mean   = mean(claim_amount),
            median = median(claim_amount),
            p99    = quantile(claim_amount, .99),
            max    = max(claim_amount),
            .groups = "drop") %>%
  arrange(desc(mean)) %>%
  print()

# ==============================================================================
# PLOT 0 — Overlaid density: visual justification for the split
# ==============================================================================
ggplot(
  sev_raw %>% mutate(group = ifelse(equipment_type == "Quantum Bore",
                                    "Quantum Bore", "Other")),
  aes(x = claim_amount, fill = group, colour = group)
) +
  geom_density(alpha = 0.25, adjust = 1.2, linewidth = 0.8) +
  scale_x_log10(labels = comma) +
  labs(title    = "Claim Amount Density: Quantum Bore vs Other Equipment",
       subtitle = "Log scale — separation justifies separate severity models",
       x = "Claim Amount (log scale)", y = "Density",
       fill = "Group", colour = "Group") +
  theme_minimal(base_size = 12)

# ------------------------------------------------------------------------------
# Helper 1. Safe fit for candidate distributions
# Fit is done on x / scale_k for numerical stability
# ------------------------------------------------------------------------------

safe_fitdist_scaled <- function(x, dist_name, scale_k = 1000) {
  y <- x / scale_k
  y <- y[is.finite(y) & y > 0]
  
  fit <- tryCatch({
    if (dist_name == "lnorm") {
      fitdist(y, "lnorm")
    } else if (dist_name == "gamma") {
      fitdist(
        y, "gamma",
        start = list(shape = mean(y)^2 / var(y), rate = mean(y) / var(y)),
        lower = c(1e-4, 1e-8)
      )
    } else if (dist_name == "weibull") {
      fitdist(
        y, "weibull",
        start = list(shape = 1, scale = mean(y)),
        lower = c(1e-3, 1e-3)
      )
    } else if (dist_name == "llogis") {
      fitdist(
        y, "llogis",
        start = list(shape = 2, rate = 1 / median(y)),
        lower = c(1e-3, 1e-8)
      )
    } else if (dist_name == "burr") {
      fitdist(
        y, "burr",
        start = list(shape1 = 2, shape2 = 1, rate = 1 / median(y)),
        lower = c(1e-3, 1e-3, 1e-8)
      )
    } else {
      stop("Unsupported distribution.")
    }
  }, error = function(e) NULL)
  
  if (!is.null(fit)) {
    fit$scale_k <- scale_k
    fit$dist_short <- dist_name
  }
  
  fit
}

# ------------------------------------------------------------------------------
# Helper 2. Generic CDF / Quantile / RNG from fitted distribution
# Returned on ORIGINAL dollar scale
# ------------------------------------------------------------------------------

cdf_from_fit <- function(x, fit) {
  y <- x / fit$scale_k
  est <- as.list(fit$estimate)
  
  switch(
    fit$distname,
    "lnorm"   = plnorm(y, meanlog = est$meanlog, sdlog = est$sdlog),
    "gamma"   = pgamma(y, shape = est$shape, rate = est$rate),
    "weibull" = pweibull(y, shape = est$shape, scale = est$scale),
    "llogis"  = actuar::pllogis(y, shape = est$shape, rate = est$rate),
    "burr"    = actuar::pburr(y, shape1 = est$shape1, shape2 = est$shape2, rate = est$rate)
  )
}

pdf_from_fit <- function(x, fit) {
  y <- x / fit$scale_k
  est <- as.list(fit$estimate)
  
  d <- switch(
    fit$distname,
    "lnorm"   = dlnorm(y, meanlog = est$meanlog, sdlog = est$sdlog),
    "gamma"   = dgamma(y, shape = est$shape, rate = est$rate),
    "weibull" = dweibull(y, shape = est$shape, scale = est$scale),
    "llogis"  = actuar::dllogis(y, shape = est$shape, rate = est$rate),
    "burr"    = actuar::dburr(y, shape1 = est$shape1, shape2 = est$shape2, rate = est$rate)
  )
  
  # Jacobian for scaling x = y * scale_k
  d / fit$scale_k
}

quant_from_fit <- function(p, fit) {
  est <- as.list(fit$estimate)
  
  q <- switch(
    fit$distname,
    "lnorm"   = qlnorm(p, meanlog = est$meanlog, sdlog = est$sdlog),
    "gamma"   = qgamma(p, shape = est$shape, rate = est$rate),
    "weibull" = qweibull(p, shape = est$shape, scale = est$scale),
    "llogis"  = actuar::qllogis(p, shape = est$shape, rate = est$rate),
    "burr"    = actuar::qburr(p, shape1 = est$shape1, shape2 = est$shape2, rate = est$rate)
  )
  
  q * fit$scale_k
}

r_from_fit <- function(n, fit) {
  est <- as.list(fit$estimate)
  
  x <- switch(
    fit$distname,
    "lnorm"   = rlnorm(n, meanlog = est$meanlog, sdlog = est$sdlog),
    "gamma"   = rgamma(n, shape = est$shape, rate = est$rate),
    "weibull" = rweibull(n, shape = est$shape, scale = est$scale),
    "llogis"  = actuar::rllogis(n, shape = est$shape, rate = est$rate),
    "burr"    = actuar::rburr(n, shape1 = est$shape1, shape2 = est$shape2, rate = est$rate)
  )
  
  x * fit$scale_k
}

# ------------------------------------------------------------------------------
# Helper 3. QQ RMSE
# overall_qq_rmse: body fit check
# tail_qq_rmse   : upper tail fit check
# ------------------------------------------------------------------------------

qq_rmse_from_fit <- function(x, fit, probs = seq(0.05, 0.95, by = 0.05)) {
  emp_q  <- as.numeric(quantile(x, probs, na.rm = TRUE))
  theo_q <- quant_from_fit(probs, fit)
  sqrt(mean((emp_q - theo_q)^2))
}

tail_qq_rmse_from_fit <- function(x, fit, probs = seq(0.80, 0.995, by = 0.005)) {
  emp_q  <- as.numeric(quantile(x, probs, na.rm = TRUE))
  theo_q <- quant_from_fit(probs, fit)
  sqrt(mean((emp_q - theo_q)^2))
}

# ------------------------------------------------------------------------------
# Helper 4. Fit candidate models and produce comparison table
# ------------------------------------------------------------------------------

fit_candidate_models <- function(x,
                                 dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
                                 scale_k = 1000) {
  fits <- lapply(dist_candidates, function(d) safe_fitdist_scaled(x, d, scale_k = scale_k))
  names(fits) <- dist_candidates
  fits <- Filter(Negate(is.null), fits)
  
  if (length(fits) == 0) stop("No candidate distribution fit succeeded.")
  
  summary_tbl <- data.frame(
    Distribution    = names(fits),
    AIC             = sapply(fits, function(f) f$aic),
    BIC             = sapply(fits, function(f) f$bic),
    overall_qq_rmse = sapply(fits, function(f) qq_rmse_from_fit(x, f)),
    tail_qq_rmse    = sapply(fits, function(f) tail_qq_rmse_from_fit(x, f))
  ) %>%
    mutate(delta_AIC = AIC - min(AIC, na.rm = TRUE)) %>%
    arrange(tail_qq_rmse, delta_AIC, overall_qq_rmse)
  
  list(fits = fits, summary = summary_tbl)
}

choose_best_model <- function(summary_tbl) {
  summary_tbl %>%
    arrange(tail_qq_rmse, delta_AIC, overall_qq_rmse) %>%
    slice(1)
}

# ------------------------------------------------------------------------------
# Helper 5. Compare catastrophe thresholds for Group A
# For each q_boot:
#   - split body / cat
#   - fit candidate body models on x <= threshold
#   - choose best body model
#   - evaluate body fit
# Then choose best q:
#   among n_cat >= min_cat, smallest body_qq_rmse
# ------------------------------------------------------------------------------

compare_boot_thresholds <- function(x,
                                    q_grid = c(0.99, 0.992, 0.993, 0.994, 0.995, 0.996),
                                    min_cat = 7,
                                    dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
                                    scale_k = 1000) {
  
  rows <- lapply(q_grid, function(q) {
    u <- as.numeric(quantile(x, q, na.rm = TRUE))
    
    x_body <- x[x <= u]
    x_cat  <- x[x > u]
    
    body_cmp  <- fit_candidate_models(x_body, dist_candidates = dist_candidates, scale_k = scale_k)
    best_body <- choose_best_model(body_cmp$summary)
    
    data.frame(
      q_boot            = q,
      threshold         = u,
      n_total           = length(x),
      n_body            = length(x_body),
      n_cat             = length(x_cat),
      p_cat             = length(x_cat) / length(x),
      cat_mean          = ifelse(length(x_cat) > 0, mean(x_cat), NA),
      cat_max           = ifelse(length(x_cat) > 0, max(x_cat),  NA),
      cat_loss_share    = ifelse(length(x_cat) > 0, sum(x_cat) / sum(x), 0),
      best_body_model   = best_body$Distribution,
      body_aic          = best_body$AIC,
      body_delta_AIC    = best_body$delta_AIC,
      body_qq_rmse      = best_body$overall_qq_rmse,
      body_tail_qq_rmse = best_body$tail_qq_rmse
    )
  })
  
  cmp_tbl <- bind_rows(rows)
  
  feasible_tbl <- cmp_tbl %>%
    filter(n_cat >= min_cat)
  
  if (nrow(feasible_tbl) == 0) {
    stop("No feasible threshold with n_cat >= min_cat. Lower min_cat or widen q_grid.")
  }
  
  best_q_row <- feasible_tbl %>%
    arrange(body_qq_rmse, body_tail_qq_rmse, desc(n_cat), q_boot) %>%
    slice(1)
  
  list(
    comparison = cmp_tbl,
    best       = best_q_row
  )
}

# ------------------------------------------------------------------------------
# Helper 6. Build final distribution for Group A
# Proper spliced distribution:
#   body: fitted parametric distribution truncated at u
#   tail: empirical bootstrap distribution on x_cat
# This avoids tail double-counting
# ------------------------------------------------------------------------------

build_groupA_distribution <- function(x, q_boot,
                                      dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
                                      scale_k = 1000) {
  
  u_star <- as.numeric(quantile(x, q_boot, na.rm = TRUE))
  x_body <- x[x <= u_star]
  x_cat  <- x[x > u_star]
  
  p_cat <- length(x_cat) / length(x)
  phi   <- 1 - p_cat
  
  body_cmp  <- fit_candidate_models(x_body, dist_candidates = dist_candidates, scale_k = scale_k)
  best_body <- choose_best_model(body_cmp$summary)
  
  body_fit <- body_cmp$fits[[best_body$Distribution]]
  F_u      <- cdf_from_fit(u_star, body_fit)
  G_cat    <- ecdf(x_cat)
  
  # Continuous part of the density on (0, u_star]
  f_A_cont <- function(z) {
    z_vec <- z
    sapply(z_vec, function(v) {
      if (v <= u_star) {
        phi * pdf_from_fit(v, body_fit) / F_u
      } else {
        0
      }
    })
  }
  
  # Final CDF
  F_A <- function(q) {
    sapply(q, function(z) {
      if (z <= u_star) {
        phi * cdf_from_fit(z, body_fit) / F_u
      } else {
        phi + (1 - phi) * G_cat(z)
      }
    })
  }
  
  # Final Quantile
  Q_A <- function(p) {
    sapply(p, function(prob) {
      if (prob <= phi) {
        quant_from_fit(prob / phi * F_u, body_fit)
      } else {
        as.numeric(quantile(x_cat,
                            probs = (prob - phi) / (1 - phi),
                            type = 1, names = FALSE))
      }
    })
  }
  
  list(
    group          = "A: Quantum Bore",
    model_type     = "Spliced: truncated parametric body + empirical bootstrap tail",
    body_model     = best_body$Distribution,
    body_fit       = body_fit,
    threshold_q    = q_boot,
    threshold_u    = u_star,
    p_cat          = p_cat,
    f_continuous   = f_A_cont,
    phi_body       = phi,
    x_cat          = x_cat,
    F              = F_A,
    Q              = Q_A,
    body_compare   = body_cmp$summary
  )
}

# ------------------------------------------------------------------------------
# Helper 7. Build final distribution for Group B
# Pure best parametric fit
# ------------------------------------------------------------------------------

build_groupB_distribution <- function(x,
                                      dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
                                      scale_k = 1000) {
  
  cmp  <- fit_candidate_models(x, dist_candidates = dist_candidates, scale_k = scale_k)
  best <- choose_best_model(cmp$summary)
  fitB <- cmp$fits[[best$Distribution]]
  
  F_B <- function(q) cdf_from_fit(q, fitB)
  Q_B <- function(p) quant_from_fit(p, fitB)
  f_B <- function(q) pdf_from_fit(q, fitB)
  
  list(
    group        = "B: Other Equipment Types",
    model_type   = "Pure parametric",
    best_model   = best$Distribution,
    fit          = fitB,
    f            = f_B,
    F            = F_B,
    Q            = Q_B,
    compare      = cmp$summary
  )
}

# ==============================================================================
# ==============================================================================
#   GROUP A — QUANTUM BORE (exploratory + corrected final)
# ==============================================================================
# ==============================================================================

# ------------------------------------------------------------------------------
# Group A — Descriptive statistics
# ------------------------------------------------------------------------------
scale_k <- 1000
cat("\n======================================================================\n")
cat("  GROUP A — QUANTUM BORE\n")
cat("======================================================================\n\n")
cat(sprintf("n           = %d\n",      n_a))
cat(sprintf("mean        = %12.2f\n", mean(x_a)))
cat(sprintf("median      = %12.2f\n", median(x_a)))
cat(sprintf("sd          = %12.2f\n", sd(x_a)))
cat(sprintf("CV          = %12.4f\n", sd(x_a) / mean(x_a)))
cat(sprintf("log_mu      = %12.4f\n", mean(log(x_a))))
cat(sprintf("log_sigma   = %12.4f\n", sd(log(x_a))))
print(summary(x_a))
print(quantile(x_a, c(.50, .75, .90, .95, .99, .995, .999)))

conc_a <- sev_qb %>%
  summarise(total = sum(claim_amount),
            top1  = sum(claim_amount[claim_amount >= quantile(claim_amount, .99)]) / total,
            top5  = sum(claim_amount[claim_amount >= quantile(claim_amount, .95)]) / total,
            .groups = "drop")
cat(sprintf("\nTop 1%% of claims: %.1f%% of total loss\n", conc_a$top1 * 100))
cat(sprintf("Top 5%% of claims: %.1f%% of total loss\n",  conc_a$top5 * 100))

cat("\n--- Severity by solar_system (Group A) ---\n")
sev_qb %>%
  group_by(solar_system) %>%
  summarise(n    = n(),
            mean = mean(claim_amount),
            med  = median(claim_amount),
            p95  = quantile(claim_amount, .95),
            p99  = quantile(claim_amount, .99),
            .groups = "drop") %>%
  arrange(desc(p99)) %>%
  print()

# PLOT A1 — log(claim_amount) histogram
hist(log(x_a), breaks = 60,
     main = "log(claim_amount) — Group A — Quantum Bore", xlab = "")

# PLOT A2 — QQ: log(claim) vs Normal
ggplot(data.frame(x = x_a), aes(sample = log(x_a))) +
  stat_qq(alpha = 0.4, size = 0.9, colour = "#636363") +
  stat_qq_line(colour = "#d6604d", linewidth = 1) +
  labs(title    = "Q-Q Plot: log(Claim) vs Normal — Group A — Quantum Bore",
       subtitle = "Curvature at right tail = heavier than LogNormal",
       x = "Theoretical Normal quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 12)

# PLOT A3 — Log-log survival
surv_a <- 1 - ecdf(xs_a)(xs_a)
plot(log(xs_a), log(pmax(surv_a, 1e-10)),
     xlab = "log(claim)", ylab = "log(P(X > t))",
     main = "Log-Log Survival — Group A — Quantum Bore")

# PLOT A4 — Mean Excess Plot
u_grid_a <- quantile(xs_a, seq(0.10, 0.97, by = 0.005))
mep_a <- data.frame(
  u           = as.numeric(u_grid_a),
  mean_excess = sapply(u_grid_a, function(u) mean(xs_a[xs_a > u] - u)),
  n_above     = sapply(u_grid_a, function(u) sum(xs_a > u))
) %>%
  mutate(
    sd_excess = sapply(u, function(u) {
      exc <- xs_a[xs_a > u] - u
      if (length(exc) < 2) return(NA); sd(exc)
    }),
    se    = sd_excess / sqrt(n_above),
    ci_lo = mean_excess - 1.96 * se,
    ci_hi = mean_excess + 1.96 * se
  )
ggplot(mep_a, aes(x = u, y = mean_excess)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#7b2d8b") +
  geom_line(colour = "#7b2d8b", linewidth = 1) +
  geom_vline(xintercept = quantile(x_a, c(0.75, 0.85, 0.90)),
             linetype = "dashed", colour = c("#e31a1c", "#ff7f00", "#33a02c")) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title    = "Mean Excess Plot — Group A — Quantum Bore",
       subtitle = "Rising -> Pareto-type | Flat -> Exponential | Falling -> Bounded",
       x = "Threshold u", y = "e(u) = E[X - u | X > u]") +
  theme_minimal(base_size = 12)

# PLOT A5 — Hill Estimator
xs_desc_a <- sort(x_a, decreasing = TRUE)
k_seq_a   <- 5:floor(n_a / 5)
xi_hill_a <- sapply(k_seq_a, function(k) mean(log(xs_desc_a[1:k])) - log(xs_desc_a[k + 1]))
hill_a           <- data.frame(k = k_seq_a, xi = xi_hill_a)
hill_a$xi_smooth <- zoo::rollmean(hill_a$xi, k = 30, fill = NA, align = "center")
diff_xi_a        <- abs(diff(hill_a$xi_smooth))
diff_smooth_a    <- zoo::rollmean(diff_xi_a, k = 30, fill = NA, align = "center")
plateau_idx_a    <- which.min(diff_smooth_a)
xi_plateau_a     <- hill_a$xi_smooth[plateau_idx_a]
k_plateau_a      <- hill_a$k[plateau_idx_a]
cat(sprintf("\nGroup A — Hill plateau: k = %d  |  xi ≈ %.4f\n", k_plateau_a, xi_plateau_a))
ggplot(hill_a, aes(x = k, y = xi)) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  geom_line(aes(y = xi_smooth), colour = "orange", linewidth = 1) +
  geom_hline(yintercept = xi_plateau_a, linetype = "dashed", colour = "#e31a1c") +
  geom_vline(xintercept = k_plateau_a,  linetype = "dashed", colour = "grey40") +
  labs(title = "Hill Plot — Group A — Quantum Bore",
       subtitle = "Blue: raw | Orange: smoothed | Look for a flat plateau",
       x = "k (upper order statistics)", y = expression(hat(xi)[Hill](k))) +
  theme_minimal(base_size = 12)

# PLOT A6–A7 — GPD Stability (xi and sigma*)
u_cands_a <- quantile(x_a, seq(0.60, 0.95, by = 0.01))
stab_list_a <- lapply(u_cands_a, function(u) {
  exc <- x_a[x_a > u] - u
  if (length(exc) < 30) return(NULL)
  tryCatch({
    fit        <- ismev::gpd.fit(x_a, threshold = u, show = FALSE)
    sigma      <- fit$mle[1]; xi <- fit$mle[2]
    sigma_star <- sigma - xi * u
    data.frame(u = u, xi = xi, sigma = sigma, sigma_star = sigma_star,
               se_xi = fit$se[2], se_sigma = fit$se[1], n_excess = length(exc))
  }, error = function(e) NULL)
})
stab_a <- do.call(rbind, Filter(Negate(is.null), stab_list_a))
thresh_labs <- data.frame(
  u     = as.numeric(quantile(x_a, c(0.75, 0.85, 0.90))),
  label = c("p75", "p85", "p90"),
  col   = c("#e31a1c", "#ff7f00", "#33a02c")
)
ggplot(stab_a, aes(x = u, y = xi)) +
  geom_ribbon(aes(ymin = xi - 1.96 * se_xi, ymax = xi + 1.96 * se_xi),
              alpha = 0.20, fill = "#1f78b4") +
  geom_line(colour = "#1f78b4", linewidth = 1) +
  geom_vline(xintercept = thresh_labs$u, linetype = "dashed", colour = thresh_labs$col) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  scale_x_continuous(labels = comma) +
  labs(title = "GPD Stability — xi (Group A — Quantum Bore)",
       x = "Threshold u", y = expression(hat(xi)(u))) +
  theme_minimal(base_size = 12)
ggplot(stab_a, aes(x = u, y = sigma_star)) +
  geom_ribbon(aes(ymin = sigma_star - 1.96 * se_sigma, ymax = sigma_star + 1.96 * se_sigma),
              alpha = 0.20, fill = "#e31a1c") +
  geom_line(colour = "#e31a1c", linewidth = 1) +
  geom_vline(xintercept = thresh_labs$u, linetype = "dashed", colour = thresh_labs$col) +
  scale_x_continuous(labels = comma) +
  labs(title = "GPD Stability — sigma* (Group A — Quantum Bore)",
       x = "Threshold u", y = expression(sigma^"*"(u))) +
  theme_minimal(base_size = 12)

# Unconditional MLE fits for exploratory 4-panel (A8)
y_a     <- x_a / scale_k
mom_shape_a <- mean(y_a)^2 / var(y_a)
mom_rate_a  <- mean(y_a)   / var(y_a)
fit_ln_a    <- fitdist(y_a, "lnorm")
fit_gam_a   <- fitdist(y_a, "gamma",
                       start = list(shape = mom_shape_a, rate = mom_rate_a),
                       lower = c(1e-4, 1e-8))
fit_wei_a   <- fitdist(y_a, "weibull",
                       start = list(shape = 1, scale = mean(y_a)),
                       lower = c(1e-3, 1e-3))
fit_llogis_a <- tryCatch(
  fitdist(y_a, "llogis", start = list(shape = 2, rate = 1 / median(y_a)),
          lower = c(1e-3, 1e-8)), error = function(e) NULL)
fit_burr_a <- tryCatch(
  fitdist(y_a, "burr", start = list(shape1 = 2, shape2 = 1, rate = 1 / median(y_a)),
          lower = c(1e-3, 1e-3, 1e-8)), error = function(e) NULL)
fits_a <- Filter(Negate(is.null),
                 list(LogNormal = fit_ln_a, Gamma = fit_gam_a, Weibull = fit_wei_a,
                      LogLogistic = fit_llogis_a, Burr = fit_burr_a))
aic_tbl_a <- data.frame(Distribution = names(fits_a),
                        AIC = sapply(fits_a, function(f) f$aic),
                        BIC = sapply(fits_a, function(f) f$bic)) %>%
  mutate(delta_AIC = AIC - min(AIC)) %>% arrange(AIC)
cat("\n--- Group A: AIC / BIC (unconditional fits) ---\n")
print(aic_tbl_a)

# PLOT A8 — 4-panel diagnostic
par(mfrow = c(2, 2))
denscomp(fits_a, legendtext = names(fits_a),
         main = "Density ($000s) — Group A — Quantum Bore", xlab = "Claim Amount ($000s)")
qqcomp(fits_a, legendtext = names(fits_a), main = "Q-Q Comparison")
cdfcomp(fits_a, legendtext = names(fits_a), main = "CDF Comparison", xlab = "Claim ($000s)")
ppcomp(fits_a, legendtext = names(fits_a), main = "P-P Comparison")
par(mfrow = c(1, 1))

# PLOT A9 — Tail Q-Q (top 20%, LogNormal + Burr)
q_seq <- seq(0.80, 0.995, by = 0.001)
tail_a <- data.frame(Empirical = as.numeric(quantile(y_a, q_seq)))
tail_a$LogNormal <- qlnorm(q_seq, fit_ln_a$estimate["meanlog"], fit_ln_a$estimate["sdlog"])
if (!is.null(fit_burr_a))
  tail_a$Burr <- actuar::qburr(q_seq, shape1 = fit_burr_a$estimate["shape1"],
                               shape2 = fit_burr_a$estimate["shape2"],
                               rate = fit_burr_a$estimate["rate"])
tail_a %>%
  tidyr::pivot_longer(-Empirical, names_to = "Model", values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Tail Q-Q (top 20%, $000s) — Group A — Quantum Bore",
       x = "Theoretical ($000s)", y = "Empirical ($000s)") +
  theme_minimal(base_size = 12)

# PLOT A10 — GPD diagnostic (u_chosen for exploratory only)
u_chosen_a <- quantile(x_a, 0.95)
gpd_a      <- tryCatch(ismev::gpd.fit(x_a, threshold = u_chosen_a, show = FALSE), error = function(e) NULL)
if (!is.null(gpd_a)) {
  par(mfrow = c(2, 2))
  ismev::gpd.diag(gpd_a)
  mtext("GPD Diagnostics — Group A (u = p95, exploratory)", outer = TRUE, cex = 1.0, line = -1.5)
  par(mfrow = c(1, 1))
}

# PLOT A11 — Exceedance Q-Q vs GPD (exploratory)
if (!is.null(gpd_a)) {
  sigma_a <- gpd_a$mle[1]; xi_a <- gpd_a$mle[2]
  exc_a   <- sort(x_a[x_a > u_chosen_a] - u_chosen_a)
  n_exc_a <- length(exc_a)
  p_plot_a <- (1:n_exc_a) / (n_exc_a + 1)
  q_gpd_a  <- evd::qgpd(p_plot_a, loc = 0, scale = sigma_a, shape = xi_a)
  ggplot(data.frame(empirical = exc_a, theoretical = q_gpd_a), aes(x = theoretical, y = empirical)) +
    geom_point(alpha = 0.5, size = 1.5, colour = "#1f78b4") +
    geom_abline(slope = 1, intercept = 0, colour = "#e31a1c", linewidth = 1, linetype = "dashed") +
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    labs(title = "Q-Q: Exceedances vs Fitted GPD — Group A (exploratory)",
         x = "Theoretical GPD quantile", y = "Empirical exceedance") +
    theme_minimal(base_size = 12)
}

# ==============================================================================
# GROUP A — Model comparison on full sample + threshold choice + corrected final
# ==============================================================================

cat("\n======================================================================\n")
cat("  GROUP A — MODEL COMPARISON ON FULL SAMPLE\n")
cat("======================================================================\n\n")

model_cmp_A_full <- fit_candidate_models(
  x_a,
  dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
  scale_k = 1000
)

print(model_cmp_A_full$summary)

best_full_A <- choose_best_model(model_cmp_A_full$summary)
cat(sprintf("\nBest full-sample parametric model for Group A = %s\n", best_full_A$Distribution))

# ==============================================================================
# GROUP A — Compare bootstrap thresholds
# ==============================================================================

cat("\n======================================================================\n")
cat("  GROUP A — BOOTSTRAP THRESHOLD COMPARISON\n")
cat("======================================================================\n\n")

threshold_search_A <- compare_boot_thresholds(
  x_a,
  q_grid = c(0.99, 0.992, 0.993, 0.994, 0.995, 0.996),
  min_cat = 7,
  dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
  scale_k = 1000
)

threshold_cmp_A <- threshold_search_A$comparison
best_q_A        <- threshold_search_A$best

print(threshold_cmp_A)

cat("\nSelected q_boot for Group A:\n")
print(best_q_A)

# Optional plot for threshold comparison
threshold_cmp_A_long <- threshold_cmp_A %>%
  select(q_boot, n_cat, p_cat, cat_loss_share, body_qq_rmse, body_tail_qq_rmse) %>%
  tidyr::pivot_longer(-q_boot, names_to = "metric", values_to = "value")

ggplot(threshold_cmp_A_long, aes(x = factor(q_boot), y = value, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Group A — Comparison of Candidate Catastrophe Thresholds",
    x = "Bootstrap threshold quantile",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

# ==============================================================================
# GROUP A — Final distribution
# ==============================================================================

dist_A <- build_groupA_distribution(
  x_a,
  q_boot = best_q_A$q_boot,
  dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
  scale_k = 1000
)

cat("\n======================================================================\n")
cat("  GROUP A — FINAL DISTRIBUTION\n")
cat("======================================================================\n\n")

cat(sprintf("Final body model        : %s\n", dist_A$body_model))
cat(sprintf("Bootstrap threshold q   : %.3f\n", dist_A$threshold_q))
cat(sprintf("Bootstrap threshold u   : %s\n", comma(round(dist_A$threshold_u))))
cat(sprintf("Cat probability p_cat   : %.6f\n", dist_A$p_cat))
cat(sprintf("Body probability phi    : %.6f\n", dist_A$phi_body))
cat(sprintf("Number of cat obs       : %d\n", length(dist_A$x_cat)))

cat("\nBody parameter estimates:\n")
print(dist_A$body_fit$estimate)

cat("\nTop catastrophe values used for bootstrap tail:\n")
print(sort(dist_A$x_cat))

# ------------------------------------------------------------------------------
# Distribution formulas for Group A
# ------------------------------------------------------------------------------
cat("\nGroup A final CDF is:\n")
cat("For x <= u*:\n")
cat("  F_A(x) = phi * F_body(x) / F_body(u*)\n")
cat("For x > u*:\n")
cat("  F_A(x) = phi + (1-phi) * G_cat(x)\n")
cat("where G_cat is the empirical CDF of the catastrophe sample.\n")
cat("\nGroup A continuous PDF (on x <= u*):\n")
cat("  f_A(x) = phi * f_body(x) / F_body(u*)\n")
cat("For the bootstrap tail, probability mass (1-phi)/n_cat is placed on each catastrophe observation.\n")

# Example usage:
# dist_A$F(c(1e5, 3e5, 1e6))
# dist_A$Q(c(0.90, 0.95, 0.99))

# ==============================================================================
# ==============================================================================
#   GROUP B — OTHER EQUIPMENT TYPES (exploratory + final)
# ==============================================================================
# ==============================================================================

# Group B — Descriptive statistics
cat("\n======================================================================\n")
cat("  GROUP B — OTHER EQUIPMENT TYPES\n")
cat("======================================================================\n\n")
cat(sprintf("n           = %d\n", n_b))
cat(sprintf("mean        = %12.2f\n", mean(x_b)))
cat(sprintf("median      = %12.2f\n", median(x_b)))
cat(sprintf("sd          = %12.2f\n", sd(x_b)))
cat(sprintf("CV          = %12.4f\n", sd(x_b) / mean(x_b)))
cat(sprintf("log_mu      = %12.4f\n", mean(log(x_b))))
cat(sprintf("log_sigma   = %12.4f\n", sd(log(x_b))))
print(summary(x_b))
print(quantile(x_b, c(.50, .75, .90, .95, .99, .995, .999)))
conc_b <- sev_other %>%
  summarise(total = sum(claim_amount),
            top1  = sum(claim_amount[claim_amount >= quantile(claim_amount, .99)]) / total,
            top5  = sum(claim_amount[claim_amount >= quantile(claim_amount, .95)]) / total,
            .groups = "drop")
cat(sprintf("\nTop 1%% of claims: %.1f%% of total loss\n", conc_b$top1 * 100))
cat(sprintf("Top 5%% of claims: %.1f%% of total loss\n", conc_b$top5 * 100))
cat("\n--- Severity by solar_system (Group B) ---\n")
sev_other %>%
  group_by(solar_system) %>%
  summarise(n = n(), mean = mean(claim_amount), med = median(claim_amount),
            p95 = quantile(claim_amount, .95), p99 = quantile(claim_amount, .99),
            .groups = "drop") %>%
  arrange(desc(p99)) %>% print()

# PLOT B1–B2
hist(log(x_b), breaks = 60, main = "log(claim_amount) — Group B — Other Equipment Types", xlab = "")
ggplot(data.frame(x = x_b), aes(sample = log(x_b))) +
  stat_qq(alpha = 0.4, size = 0.9, colour = "#636363") +
  stat_qq_line(colour = "#d6604d", linewidth = 1) +
  labs(title = "Q-Q: log(Claim) vs Normal — Group B", x = "Theoretical Normal quantiles", y = "Sample quantiles") +
  theme_minimal(base_size = 12)

# PLOT B3–B4 — Log-log survival, Mean Excess
surv_b <- 1 - ecdf(xs_b)(xs_b)
plot(log(xs_b), log(pmax(surv_b, 1e-10)), xlab = "log(claim)", ylab = "log(P(X > t))",
     main = "Log-Log Survival — Group B")
u_grid_b <- quantile(xs_b, seq(0.10, 0.97, by = 0.005))
mep_b <- data.frame(
  u = as.numeric(u_grid_b),
  mean_excess = sapply(u_grid_b, function(u) mean(xs_b[xs_b > u] - u)),
  n_above = sapply(u_grid_b, function(u) sum(xs_b > u))
) %>% mutate(
  sd_excess = sapply(u, function(u) { exc <- xs_b[xs_b > u] - u; if (length(exc) < 2) NA else sd(exc) }),
  se = sd_excess / sqrt(n_above), ci_lo = mean_excess - 1.96 * se, ci_hi = mean_excess + 1.96 * se
)
ggplot(mep_b, aes(x = u, y = mean_excess)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "#7b2d8b") +
  geom_line(colour = "#7b2d8b", linewidth = 1) +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Mean Excess Plot — Group B", x = "Threshold u", y = "e(u)") +
  theme_minimal(base_size = 12)

# PLOT B5 — Hill
xs_desc_b <- sort(x_b, decreasing = TRUE)
k_seq_b  <- 5:floor(n_b / 5)
xi_hill_b <- sapply(k_seq_b, function(k) mean(log(xs_desc_b[1:k])) - log(xs_desc_b[k + 1]))
hill_b <- data.frame(k = k_seq_b, xi = xi_hill_b)
hill_b$xi_smooth <- zoo::rollmean(hill_b$xi, k = 30, fill = NA, align = "center")
ggplot(hill_b, aes(x = k, y = xi)) +
  geom_line(colour = "#1f78b4", linewidth = 0.8) +
  geom_line(aes(y = xi_smooth), colour = "orange", linewidth = 1) +
  labs(title = "Hill Plot — Group B", x = "k", y = expression(hat(xi)[Hill](k))) +
  theme_minimal(base_size = 12)

# PLOT B6–B7 — GPD stability (optional for B)
u_cands_b <- quantile(x_b, seq(0.60, 0.95, by = 0.01))
stab_list_b <- lapply(u_cands_b, function(u) {
  exc <- x_b[x_b > u] - u
  if (length(exc) < 30) return(NULL)
  tryCatch({
    fit <- ismev::gpd.fit(x_b, threshold = u, show = FALSE)
    data.frame(u = u, xi = fit$mle[2], sigma_star = fit$mle[1] - fit$mle[2] * u,
               se_xi = fit$se[2], se_sigma = fit$se[1], n_excess = length(exc))
  }, error = function(e) NULL)
})
stab_b <- do.call(rbind, Filter(Negate(is.null), stab_list_b))
if (nrow(stab_b) > 0) {
  ggplot(stab_b, aes(x = u, y = xi)) +
    geom_ribbon(aes(ymin = xi - 1.96 * se_xi, ymax = xi + 1.96 * se_xi), alpha = 0.2, fill = "#1f78b4") +
    geom_line(colour = "#1f78b4", linewidth = 1) + scale_x_continuous(labels = comma) +
    labs(title = "GPD Stability — xi (Group B)", x = "Threshold u", y = expression(hat(xi))) +
    theme_minimal(base_size = 12)
}

# Unconditional fits for 4-panel (B8)
y_b <- x_b / scale_k
mom_shape_b <- mean(y_b)^2 / var(y_b); mom_rate_b <- mean(y_b) / var(y_b)
fit_ln_b   <- fitdist(y_b, "lnorm")
fit_gam_b  <- fitdist(y_b, "gamma", start = list(shape = mom_shape_b, rate = mom_rate_b), lower = c(1e-4, 1e-8))
fit_wei_b  <- fitdist(y_b, "weibull", start = list(shape = 1, scale = mean(y_b)), lower = c(1e-3, 1e-3))
fit_llogis_b <- tryCatch(fitdist(y_b, "llogis", start = list(shape = 2, rate = 1/median(y_b)), lower = c(1e-3, 1e-8)), error = function(e) NULL)
fit_burr_b <- tryCatch(fitdist(y_b, "burr", start = list(shape1 = 2, shape2 = 1, rate = 1/median(y_b)), lower = c(1e-3, 1e-3, 1e-8)), error = function(e) NULL)
fits_b <- Filter(Negate(is.null), list(LogNormal = fit_ln_b, Gamma = fit_gam_b, Weibull = fit_wei_b, LogLogistic = fit_llogis_b, Burr = fit_burr_b))
cat("\n--- Group B: AIC / BIC ---\n")
print(data.frame(Distribution = names(fits_b), AIC = sapply(fits_b, function(f) f$aic), BIC = sapply(fits_b, function(f) f$bic)) %>% mutate(delta_AIC = AIC - min(AIC)))
par(mfrow = c(2, 2))
denscomp(fits_b, legendtext = names(fits_b), main = "Density ($000s) — Group B", xlab = "Claim ($000s)")
qqcomp(fits_b, legendtext = names(fits_b), main = "Q-Q"); cdfcomp(fits_b, legendtext = names(fits_b), main = "CDF", xlab = "Claim ($000s)")
ppcomp(fits_b, legendtext = names(fits_b), main = "P-P")
par(mfrow = c(1, 1))

# PLOT B9 — Tail Q-Q
tail_b <- data.frame(Empirical = as.numeric(quantile(y_b, q_seq)))
tail_b$LogNormal <- qlnorm(q_seq, fit_ln_b$estimate["meanlog"], fit_ln_b$estimate["sdlog"])
if (!is.null(fit_burr_b)) tail_b$Burr <- actuar::qburr(q_seq, shape1 = fit_burr_b$estimate["shape1"], shape2 = fit_burr_b$estimate["shape2"], rate = fit_burr_b$estimate["rate"])
tail_b %>% tidyr::pivot_longer(-Empirical, names_to = "Model", values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1.2, alpha = 0.7) + geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Tail Q-Q (top 20%, $000s) — Group B", x = "Theoretical ($000s)", y = "Empirical ($000s)") +
  theme_minimal(base_size = 12)

# ==============================================================================
# GROUP B — Best model and final distribution
# ==============================================================================
cat("\n======================================================================\n")
cat("  GROUP B — MODEL COMPARISON AND FINAL DISTRIBUTION\n")
cat("======================================================================\n\n")

dist_B <- build_groupB_distribution(
  x_b,
  dist_candidates = c("lnorm", "gamma", "weibull", "llogis", "burr"),
  scale_k = 1000
)

print(dist_B$compare)

cat(sprintf("\nBest Group B model = %s\n", dist_B$best_model))
cat("\nParameter estimates:\n")
print(dist_B$fit$estimate)

cat("\nGroup B final CDF is simply F_B(x) = F_best(x; theta_B)\n")
cat(sprintf("Best model for Group B is: %s\n", dist_B$best_model))

# Example usage:
# dist_B$F(c(1e5, 3e5, 1e6))
# dist_B$Q(c(0.90, 0.95, 0.99))

# ==============================================================================
# Optional: Compare empirical CDF vs final fitted CDF
# ==============================================================================

x_grid_A <- seq(min(x_a), quantile(x_a, 0.999), length.out = 800)
cdf_plot_A <- data.frame(
  x         = x_grid_A,
  Empirical = ecdf(x_a)(x_grid_A),
  Final     = dist_A$F(x_grid_A)
) %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "CDF")

ggplot(cdf_plot_A, aes(x = x, y = CDF, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  geom_vline(xintercept = dist_A$threshold_u, linetype = "dotted", colour = "grey40") +
  labs(title = "Group A — Empirical vs Final Fitted CDF",
       x = "Claim Amount (log scale)", y = "F(x)") +
  theme_minimal(base_size = 12)

# PLOT A12 — CDF: Empirical vs Spliced (corrected) vs LogNormal full
mu_ln_full_a <- log(exp(fit_ln_a$estimate["meanlog"]) * scale_k)
sig_ln_full_a <- fit_ln_a$estimate["sdlog"]
cdf_a_3 <- data.frame(
  x         = x_grid_A,
  Empirical = ecdf(x_a)(x_grid_A),
  Spliced  = dist_A$F(x_grid_A),
  LogNormal = plnorm(x_grid_A, mu_ln_full_a, sig_ln_full_a)
) %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "CDF")
ggplot(cdf_a_3, aes(x = x, y = CDF, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  scale_colour_manual(values = c(Empirical = "#333333", Spliced = "#e31a1c", LogNormal = "#1f78b4")) +
  geom_vline(xintercept = dist_A$threshold_u, linetype = "dotted", colour = "grey40") +
  labs(title = "CDF: Empirical vs Spliced (corrected) vs LogNormal — Group A",
       x = "Claim Amount (log scale)", y = "F(x)") +
  theme_minimal(base_size = 12)

# PLOT A13 — Tail Q-Q including Spliced (corrected)
tail_a$Spliced <- dist_A$Q(q_seq) / scale_k
tail_a %>%
  tidyr::pivot_longer(-Empirical, names_to = "Model", values_to = "Theoretical") %>%
  ggplot(aes(x = Theoretical, y = Empirical, colour = Model)) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "black") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Tail Q-Q incl. Spliced (corrected) — Group A — Quantum Bore",
       subtitle = "Spliced = truncated body + empirical bootstrap tail",
       x = "Theoretical ($000s)", y = "Empirical ($000s)") +
  theme_minimal(base_size = 12)

# SECTION A — Risk measures (corrected spliced model) and simulation
cat("\n--- Group A Risk Measures (corrected spliced: body + empirical tail) ---\n")
p_levels <- c(0.50, 0.75, 0.90, 0.95, 0.99, 0.995, 0.999)
cat(sprintf("%-8s  %14s  %14s\n", "Level", "VaR Spliced ($)", "Empirical ($)"))
for (p in p_levels) {
  var_splice <- dist_A$Q(p)
  var_emp    <- as.numeric(quantile(x_a, p))
  cat(sprintf("%-8s  %14s  %14s\n", paste0(100 * p, "%"), comma(round(var_splice)), comma(round(var_emp))))
}
# Corrected simulation: body truncated at u*, tail = bootstrap; no draw can exceed tail set
r_final_a <- function(n_samp, cap = Inf) {
  pmin(dist_A$Q(runif(n_samp)), cap)
}
cat("\nGroup A simulation: r_final_a(n, cap) uses corrected spliced (truncated body + empirical tail).\n")
cat("Usage: r_final_a(100000)  or  r_final_a(100000, cap = 2e6)\n")

x_grid_B <- seq(min(x_b), quantile(x_b, 0.999), length.out = 800)
cdf_plot_B <- data.frame(
  x         = x_grid_B,
  Empirical = ecdf(x_b)(x_grid_B),
  Final     = dist_B$F(x_grid_B)
) %>%
  tidyr::pivot_longer(-x, names_to = "Model", values_to = "CDF")

ggplot(cdf_plot_B, aes(x = x, y = CDF, colour = Model, linetype = Model)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = comma) +
  labs(title = "Group B — Empirical vs Final Fitted CDF",
       x = "Claim Amount (log scale)", y = "F(x)") +
  theme_minimal(base_size = 12)

# SECTION B — Risk measures and simulation
cat("\n--- Group B Risk Measures (best parametric) ---\n")
cat(sprintf("%-8s  %14s  %14s\n", "Level", "VaR Fitted ($)", "Empirical ($)"))
for (p in p_levels) {
  cat(sprintf("%-8s  %14s  %14s\n", paste0(100 * p, "%"),
              comma(round(dist_B$Q(p))), comma(round(quantile(x_b, p)))))
}
r_final_b <- function(n_samp, cap = Inf) pmin(dist_B$Q(runif(n_samp)), cap)
cat("\nGroup B simulation: r_final_b(n, cap). Usage: r_final_b(100000) or r_final_b(100000, cap = 2e6)\n")

# ==============================================================================
# CROSS-GROUP COMPARISON
# ==============================================================================
cat("\n\n", strrep("=", 70), "\n", sep = "")
cat("  CROSS-GROUP COMPARISON\n")
cat(strrep("=", 70), "\n\n")
p_cmp <- c(0.90, 0.95, 0.99, 0.995, 0.999)
cmp_df <- data.frame(
  Level  = paste0("p", 100 * p_cmp),
  VaR_A  = comma(round(dist_A$Q(p_cmp))),
  VaR_B  = comma(round(dist_B$Q(p_cmp))),
  Emp_A  = comma(round(quantile(x_a, p_cmp))),
  Emp_B  = comma(round(quantile(x_b, p_cmp)))
)
names(cmp_df) <- c("Level", "VaR A (QB)", "VaR B (Other)", "Emp A", "Emp B")
print(cmp_df)

# PLOT FINAL — Side-by-side density
plot_df <- bind_rows(
  sev_qb    %>% mutate(Group = "Quantum Bore"),
  sev_other %>% mutate(Group = "Other Equipment")
)
ggplot(plot_df, aes(x = claim_amount, fill = Group, colour = Group)) +
  geom_density(alpha = 0.25, adjust = 1.2, linewidth = 0.8) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(values  = c("Quantum Bore" = "#b2182b", "Other Equipment" = "#2166ac")) +
  scale_colour_manual(values = c("Quantum Bore" = "#b2182b", "Other Equipment" = "#2166ac")) +
  labs(title    = "Claim Severity: Quantum Bore vs Other Equipment (Final)",
       subtitle = "Group A: spliced (truncated body + empirical tail). Group B: best parametric.",
       x = "Claim Amount (log scale)", y = "Density", fill = "Group", colour = "Group") +
  theme_minimal(base_size = 12)

# ==============================================================================
# Final compact output objects
# ==============================================================================

final_model_summary <- tibble::tibble(
  Group = c("A: Quantum Bore", "B: Other Equipment"),
  Final_Model = c(
    paste0("Spliced(", dist_A$body_model, " body + empirical bootstrap tail)"),
    dist_B$best_model
  ),
  Threshold_q = c(dist_A$threshold_q, NA),
  Threshold_u = c(dist_A$threshold_u, NA),
  p_cat = c(dist_A$p_cat, NA)
)

print(final_model_summary)

# ==============================================================================
# MODEL SPECIFICATION (as used in code)
# ==============================================================================
#
# --- MODEL A (Quantum Bore) ---
#   Type: Spliced. Body = parametric (truncated at u*), tail = empirical bootstrap.
#   Body: dist_A$body_model (e.g. lnorm) with dist_A$body_fit$estimate (on scale_k).
#   Threshold: u* = dist_A$threshold_u. Body probability phi = dist_A$phi_body.
#   Tail: probability 1-phi on sample dist_A$x_cat (each point mass (1-phi)/n_cat).
#   CDF: F_A(x) = phi * F_body(x)/F_body(u*) for x<=u*; phi + (1-phi)*G_cat(x) for x>u*.
#   Simulate: r_final_a(n) or dist_A$Q(runif(n)).
#
# --- MODEL B (Other Equipment) ---
#   Type: Pure parametric. dist_B$best_model (e.g. lnorm) on all claims.
#   Parameters: dist_B$fit$estimate (fit on data/scale_k); scale_k = 1000.
#   CDF/PDF: dist_B$F(x), dist_B$f(x). Quantile: dist_B$Q(p).
#   Simulate: r_final_b(n) or dist_B$Q(runif(n)).
#
# ------------------------------------------------------------------------------

cat("\n")
cat(strrep("=", 72), "\n")
cat("  MODEL A — QUANTUM BORE: DISTRIBUTION & PARAMETERS\n")
cat(strrep("=", 72), "\n\n")

cat("Distribution type: Spliced\n")
cat("  - Body:  Parametric distribution (truncated at u*) fitted to claims <= u*\n")
cat("  - Tail:  Empirical bootstrap on observed claims > u*\n\n")

cat("Body component:\n")
cat("  - Distribution name: ", dist_A$body_model, "\n", sep = "")
cat("  - Parameters (body fit on scaled data; scale_k = ", scale_k, "):\n", sep = "")
print(dist_A$body_fit$estimate)
# On original scale (for LogNormal: meanlog_orig = meanlog + log(scale_k), sdlog unchanged)
if (dist_A$body_model == "lnorm") {
  est <- as.list(dist_A$body_fit$estimate)
  cat("  - On original $ scale: meanlog = ", est$meanlog + log(scale_k),
      ", sdlog = ", est$sdlog, "\n", sep = "")
}
cat("\nThreshold and tail:\n")
cat("  - u* (threshold, $)     : ", round(dist_A$threshold_u), "\n", sep = "")
cat("  - Threshold quantile     : ", dist_A$threshold_q, "\n", sep = "")
cat("  - phi (body probability) : ", dist_A$phi_body, "\n", sep = "")
cat("  - p_cat (tail probability): ", dist_A$p_cat, "\n", sep = "")
cat("  - n_cat (tail sample size): ", length(dist_A$x_cat), "\n", sep = "")
cat("  - Tail: discrete mass (1-phi)/n_cat at each of the n_cat observed values in dist_A$x_cat\n\n")

cat("CDF:\n")
cat("  F_A(x) = phi * F_body(x) / F_body(u*)   for x <= u*\n")
cat("  F_A(x) = phi + (1-phi) * G_cat(x)       for x > u*\n")
cat("  where G_cat = empirical CDF of tail sample (dist_A$x_cat)\n\n")

cat("PDF (body only; tail is discrete):\n")
cat("  f_A(x) = phi * f_body(x) / F_body(u*)   for 0 < x <= u*\n\n")

cat("Parameters needed to replicate Model A:\n")
cat("  body_model, body_fit (or body params), threshold_u = u*, phi_body = phi,\n")
cat("  x_cat = vector of tail claim amounts. In R: dist_A$body_fit, dist_A$threshold_u,\n")
cat("  dist_A$phi_body, dist_A$x_cat. Simulate: r_final_a(n) or dist_A$Q(runif(n)).\n\n")

cat(strrep("=", 72), "\n")
cat("  MODEL B — OTHER EQUIPMENT: DISTRIBUTION & PARAMETERS\n")
cat(strrep("=", 72), "\n\n")

cat("Distribution type: Pure parametric (single distribution on all claims)\n\n")

cat("Distribution name: ", dist_B$best_model, "\n", sep = "")
cat("Parameters (fit on scaled data; scale_k = ", scale_k, "):\n", sep = "")
print(dist_B$fit$estimate)
if (dist_B$best_model == "lnorm") {
  estB <- as.list(dist_B$fit$estimate)
  cat("On original $ scale: meanlog = ", estB$meanlog + log(scale_k),
      ", sdlog = ", estB$sdlog, "\n", sep = "")
}
cat("\nCDF: F_B(x) = F_(dist_B$best_model)(x; parameters above, on original scale)\n")
cat("PDF: f_B(x) = f_(dist_B$best_model)(x; parameters above)\n\n")

cat("Parameters needed to replicate Model B:\n")
cat("  best_model, fit (estimate + scale_k). In R: dist_B$fit, dist_B$best_model.\n")
cat("  CDF: dist_B$F(x). Quantile: dist_B$Q(p). Simulate: r_final_b(n) or dist_B$Q(runif(n)).\n\n")



# Save severity distributions for EF_new_old.R (Monte Carlo on new portfolio)
path_ef_sev_dists <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/ef_sev_dists.rds"
saveRDS(list(dist_A = dist_A, dist_B = dist_B, scale_k = scale_k), path_ef_sev_dists)
message("Saved severity distributions to ", path_ef_sev_dists)

cat("\nAll done.\n")
cat("Objects for Monte Carlo:\n")
cat("  Group A: r_final_a(n, cap), dist_A (F, Q, threshold_u, x_cat)\n")
cat("  Group B: r_final_b(n, cap), dist_B (F, Q, fit)\n")



# Best model for A -- Quantum Bore equipment: spliced: Lognormal + empirical bootstrap tail
# You can find information in dist_A.
# main body, lognormal, parameters: dist_A$body_fit; meanlog: 4.6918494; sdlog   0.5287625
# threshold for truncation: dist_A$threshold_u; 475108.6
# Bootstrap for tail losses: number is 8, dist_A$x_cat
# We randomly get any one value from [4237030  495711 2031845 1603831  549852 1574979 3237609  555392]
# CDF: dist_A$F(quantile)

# Best model for B: other equipments: Lognormal
# dist_B$fit: meanlog 4.1805129; sdlog   0.563082
# CDF: dist_B$F(quantile)

# For simulating losses: use:
# r_final_a(n): simulate n losses for Quantum Bore
# r_final_b(n): simulate n losses for other equipments

