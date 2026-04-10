# =============================================================================
# Cargo Loss — Historical model transfer to new solar systems
# Method chosen:
#   - Route B: historical prototype matching
#   - Exposure method 2: quantity × max volume (calibrated to historical scale)
#   - Strata: solar_system × container_type
#   - Frequency model: historical fitted cargo model
#   - Severity model: container-type-specific spliced Lognormal + GPD
# =============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(MASS)
library(fitdistrplus)
#install.packages("evd")
library(evd)
library(ggplot2)
library(scales)

set.seed(123)

# =============================================================================
# 0. LOAD HISTORICAL DATA AND MODEL
# -----------------------------------------------------------------------------
# Required:
#   cargo_freq_hist  : shipment-level historical frequency dataset
#   cargo_sev_hist   : historical severity dataset with positive claim amounts
#   final_model_cl: fitted cargo frequency model (e.g. quasipoisson GLM)
#
# You may either:
#   (A) already have them in environment
#   (B) load from .rds
# =============================================================================

if (!exists("cargo_freq_hist")) {
  cargo_freq_hist <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cl_freq_cleaned.rds")
}

if (!exists("cargo_sev_hist")) {
  cargo_sev_hist <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cl_sev_cleaned.rds")
}

if (!exists("final_model_cl")) {
  model_path <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/final_model_cl.rds"
  if (file.exists(model_path)) {
    final_model_cl <- readRDS(model_path)
    message("Loaded final_model_cl from ", model_path)
  } else {
    stop("final_model_cl not found. Run/load your historical cargo frequency model first.")
  }
}

# =============================================================================
# 1. BASIC HELPERS
# =============================================================================

Mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tvar_fn <- function(x, p = 0.95) {
  q <- as.numeric(quantile(x, p, na.rm = TRUE))
  mean(x[x >= q], na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Pick severity amount column automatically
# -----------------------------------------------------------------------------
loss_col_candidates <- c("loss_amount", "claim_amount", "severity", "paid_loss", "incurred_loss")
loss_col <- loss_col_candidates[loss_col_candidates %in% names(cargo_sev_hist)][1]

if (is.na(loss_col)) {
  stop("No severity amount column found in cargo_sev_hist. Add one of: ",
       paste(loss_col_candidates, collapse = ", "))
}

# -----------------------------------------------------------------------------
# Harmonise container names in historical data
# -----------------------------------------------------------------------------
cargo_freq_hist <- cargo_freq_hist %>%
  mutate(
    container_type = trimws(as.character(container_type)),
    cargo_type = trimws(as.character(cargo_type))
  )

cargo_sev_hist <- cargo_sev_hist %>%
  mutate(
    container_type = trimws(as.character(container_type))
  )

# =============================================================================
# 2. HISTORICAL LEVELS AND CALIBRATION
# =============================================================================

hist_container_levels <- sort(unique(as.character(cargo_freq_hist$container_type)))
hist_cargo_type_levels <- sort(unique(as.character(cargo_freq_hist$cargo_type)))

# -----------------------------------------------------------------------------
# Exposure calibration:
# historical model exposure is already in historical scale.
# For new data, we first build quantity × max_volume (kg-capacity proxy),
# then convert it back to historical exposure scale via historical kg/exposure.
# -----------------------------------------------------------------------------
hist_kg_per_exposure <- cargo_freq_hist %>%
  filter(!is.na(weight), !is.na(exposure), exposure > 0, weight > 0) %>%
  summarise(val = median(weight / exposure, na.rm = TRUE)) %>%
  pull(val)

if (length(hist_kg_per_exposure) == 0 || is.na(hist_kg_per_exposure) || hist_kg_per_exposure <= 0) {
  stop("Could not compute historical kg_per_exposure calibration.")
}

# =============================================================================
# 3. BUILD HISTORICAL PROTOTYPES BY CONTAINER TYPE
# -----------------------------------------------------------------------------
# Route B:
#   Use historical prototype profile by container_type
#   Then override system environment variables for new systems
# =============================================================================

container_proto_tbl <- cargo_freq_hist %>%
  group_by(container_type) %>%
  summarise(
    cargo_type_mode          = Mode_chr(cargo_type),
    cargo_value_med          = median(cargo_value, na.rm = TRUE),
    transit_duration_med     = median(transit_duration, na.rm = TRUE),
    pilot_experience_med     = median(pilot_experience, na.rm = TRUE),
    solar_radiation_hist_med = median(solar_radiation, na.rm = TRUE),
    debris_density_hist_med  = median(debris_density, na.rm = TRUE),
    .groups = "drop"
  )

# Optional check
print(container_proto_tbl, n = Inf)

# =============================================================================
# 4. FIT CONTAINER-TYPE SPECIFIC SEVERITY MODELS
# -----------------------------------------------------------------------------
# Base severity transfer logic:
#   - Historical severity is fitted by container_type
#   - New strata inherit same container-type family
#   - Scale by (new cargo value / historical reference cargo value)^alpha
#
# Model:
#   - body: Lognormal below threshold u
#   - tail: GPD on exceedances over u
#   - fallback: full Lognormal if tail sample too small
# =============================================================================

# -----------------------------------------------------------------------------
# We need severity rows to have container_type and claim amount.
# If cargo_value is not in severity data, we borrow median cargo_value from freq history.
# -----------------------------------------------------------------------------
sev_work <- cargo_sev_hist %>%
  mutate(
    container_type = trimws(as.character(container_type)),
    claim_amount = .data[[loss_col]]
  ) %>%
  filter(!is.na(container_type), !is.na(claim_amount), claim_amount > 0)

if (!("cargo_value" %in% names(sev_work))) {
  sev_work <- sev_work %>%
    left_join(
      cargo_freq_hist %>%
        group_by(container_type) %>%
        summarise(ref_cargo_value = median(cargo_value, na.rm = TRUE), .groups = "drop"),
      by = "container_type"
    ) %>%
    mutate(cargo_value = ref_cargo_value) %>%
    select(-ref_cargo_value)
}

# -----------------------------------------------------------------------------
# Truncated lognormal quantile sampler below threshold u
# -----------------------------------------------------------------------------
qtrunc_lnorm <- function(p, meanlog, sdlog, upper) {
  F_upper <- plnorm(upper, meanlog = meanlog, sdlog = sdlog)
  qlnorm(p * F_upper, meanlog = meanlog, sdlog = sdlog)
}

# -----------------------------------------------------------------------------
# GPD quantile
# -----------------------------------------------------------------------------
qgpd_custom <- function(p, xi, beta) {
  if (abs(xi) < 1e-8) {
    -beta * log(1 - p)
  } else {
    beta / xi * ((1 - p)^(-xi) - 1)
  }
}

# -----------------------------------------------------------------------------
# Fit spliced model for one container type
# -----------------------------------------------------------------------------
fit_spliced_lnorm_gpd <- function(x, ref_cargo_value, p_body = 0.90, min_tail_n = 25) {
  
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 30) {
    # fallback to lognormal
    fit_ln <- fitdist(x, "lnorm")
    return(list(
      model_type = "lnorm_only",
      meanlog = unname(fit_ln$estimate["meanlog"]),
      sdlog   = unname(fit_ln$estimate["sdlog"]),
      ref_cargo_value = ref_cargo_value,
      threshold = NA_real_,
      p_body = 1
    ))
  }
  
  u <- as.numeric(quantile(x, p_body, na.rm = TRUE))
  body_x <- x[x <= u]
  tail_x <- x[x > u]
  exceed <- tail_x - u
  
  # Fit body lognormal
  fit_ln <- fitdist(body_x, "lnorm")
  meanlog_hat <- unname(fit_ln$estimate["meanlog"])
  sdlog_hat   <- unname(fit_ln$estimate["sdlog"])
  
  # Tail fit
  if (length(exceed) < min_tail_n) {
    return(list(
      model_type = "lnorm_only",
      meanlog = meanlog_hat,
      sdlog   = sdlog_hat,
      ref_cargo_value = ref_cargo_value,
      threshold = u,
      p_body = 1
    ))
  }
  
  gpd_fit <- tryCatch(
    evir::gpd(x, threshold = u, method = "ml"),
    error = function(e) NULL
  )
  
  if (is.null(gpd_fit)) {
    return(list(
      model_type = "lnorm_only",
      meanlog = meanlog_hat,
      sdlog   = sdlog_hat,
      ref_cargo_value = ref_cargo_value,
      threshold = u,
      p_body = 1
    ))
  }
  
  xi_hat   <- unname(gpd_fit$par.ests["xi"])
  beta_hat <- unname(gpd_fit$par.ests["beta"])
  
  list(
    model_type = "spliced_lnorm_gpd",
    meanlog = meanlog_hat,
    sdlog   = sdlog_hat,
    threshold = u,
    xi = xi_hat,
    beta = beta_hat,
    p_body = mean(x <= u),
    ref_cargo_value = ref_cargo_value
  )
}

# -----------------------------------------------------------------------------
# Random generator from fitted severity object
# -----------------------------------------------------------------------------
rsev_from_fit <- function(fit_obj, n, cargo_value_new, alpha_scale = 0.70) {
  
  if (n <= 0) return(numeric(0))
  
  if (is.null(fit_obj)) stop("Severity fit object is NULL.")
  if (is.na(cargo_value_new) || cargo_value_new <= 0) cargo_value_new <- fit_obj$ref_cargo_value
  
  scale_mult <- (cargo_value_new / fit_obj$ref_cargo_value)^alpha_scale
  
  if (fit_obj$model_type == "lnorm_only") {
    draws <- rlnorm(n, meanlog = fit_obj$meanlog, sdlog = fit_obj$sdlog)
    return(draws * scale_mult)
  }
  
  u <- runif(n)
  out <- numeric(n)
  
  body_idx <- which(u <= fit_obj$p_body)
  tail_idx <- which(u > fit_obj$p_body)
  
  if (length(body_idx) > 0) {
    p_body_adj <- u[body_idx] / fit_obj$p_body
    out[body_idx] <- qtrunc_lnorm(
      p = p_body_adj,
      meanlog = fit_obj$meanlog,
      sdlog = fit_obj$sdlog,
      upper = fit_obj$threshold
    )
  }
  
  if (length(tail_idx) > 0) {
    p_tail_adj <- (u[tail_idx] - fit_obj$p_body) / (1 - fit_obj$p_body)
    out[tail_idx] <- fit_obj$threshold + qgpd_custom(
      p = p_tail_adj,
      xi = fit_obj$xi,
      beta = fit_obj$beta
    )
  }
  
  out * scale_mult
}

# -----------------------------------------------------------------------------
# clean container_type 
# -----------------------------------------------------------------------------
library(dplyr)
library(stringr)

# 只清理 container_type
clean_container_type_v2 <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  
  # 去控制字符
  x <- str_replace_all(x, "[[:cntrl:]]", " ")
  
  # 合并空格、去前后空格
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  
  # 去掉末尾这种 "_1234"
  x <- str_replace(x, "_[0-9]+$", "")
  
  # 再次去前后空格
  x <- str_trim(x)
  
  # 标准化名称
  x <- case_when(
    str_detect(x, regex("^deepspace\\s*haul\\s*box$", ignore_case = TRUE)) ~ "DeepSpace Haulbox",
    str_detect(x, regex("^deepspace\\s*haulbox$", ignore_case = TRUE)) ~ "DeepSpace Haulbox",
    
    str_detect(x, regex("^dockarc\\s*freight\\s*case$", ignore_case = TRUE)) ~ "DockArc Freight Case",
    
    str_detect(x, regex("^hard\\s*seal\\s*transit\\s*crate$", ignore_case = TRUE)) ~ "HardSeal Transit Crate",
    str_detect(x, regex("^hardseal\\s*transit\\s*crate$", ignore_case = TRUE)) ~ "HardSeal Transit Crate",
    
    str_detect(x, regex("^longhaul\\s*value\\s*canister$", ignore_case = TRUE)) ~ "LongHaul Vault Canister",
    str_detect(x, regex("^longhaul\\s*vault\\s*canister$", ignore_case = TRUE)) ~ "LongHaul Vault Canister",
    
    str_detect(x, regex("^quantum\\s*crate\\s*module$", ignore_case = TRUE)) ~ "QuantumCrate Module",
    str_detect(x, regex("^quantumcrate\\s*module$", ignore_case = TRUE)) ~ "QuantumCrate Module",
    
    TRUE ~ x
  )
  
  x
}

# 应用到两个数据集
cargo_freq_hist <- cargo_freq_hist %>%
  mutate(container_type = clean_container_type_v2(container_type))

cargo_sev_hist <- cargo_sev_hist %>%
  mutate(container_type = clean_container_type_v2(container_type))

# 检查结果
sort(unique(cargo_freq_hist$container_type))
sort(unique(cargo_sev_hist$container_type))

sev_work <- cargo_sev_hist %>%
  mutate(
    container_type = trimws(as.character(container_type)),
    claim_amount   = as.numeric(claim_amount),
    cargo_value    = as.numeric(cargo_value)
  ) %>%
  filter(
    !is.na(container_type),
    container_type != "",
    !is.na(claim_amount),
    claim_amount > 0
  ) %>%
  select(container_type, claim_amount, cargo_value)

# -----------------------------------------------------------------------------
# Build severity fit table by container_type
# -----------------------------------------------------------------------------

sev_split <- sev_work %>%
  group_by(container_type) %>%
  group_split()

severity_fit_tbl <- purrr::map_dfr(sev_split, function(df_one) {
  
  ct <- unique(df_one$container_type)[1]
  ref_val <- median(df_one$cargo_value, na.rm = TRUE)
  
  fit_obj <- fit_spliced_lnorm_gpd(
    x = df_one$claim_amount,
    ref_cargo_value = ref_val,
    p_body = 0.90,
    min_tail_n = 25
  )
  
  tibble(
    container_type = ct,
    ref_cargo_value = ref_val,
    sev_fit = list(fit_obj),
    n_claims = nrow(df_one)
  )
})
# =============================================================================
# 5. ENTER NEW CARGO VESSEL INVENTORY
# -----------------------------------------------------------------------------
# New portfolio unit = solar_system × container_type_raw
# =============================================================================

new_cargo_raw <- tibble::tribble(
  ~solar_system,        ~container_type_raw,        ~max_volume, ~quantity,
  "Helionis Cluster",   "DeepSpace Haul Box",          25000,         58,
  "Helionis Cluster",   "DockArc Freight Case",        50000,        116,
  "Helionis Cluster",   "HardSEal Transit Crate",     100000,        580,
  "Helionis Cluster",   "LongHaul Value Canister",    150000,        232,
  "Helionis Cluster",   "Quantum Crate Module",       250000,        174,
  
  "Bayesia System",     "DeepSpace Haul Box",          25000,         56,
  "Bayesia System",     "DockArc Freight Case",        50000,        113,
  "Bayesia System",     "HardSEal Transit Crate",     100000,        564,
  "Bayesia System",     "LongHaul Value Canister",    150000,        226,
  "Bayesia System",     "Quantum Crate Module",       250000,        169,
  
  "Oryn Delta",         "DeepSpace Haul Box",          25000,         39,
  "Oryn Delta",         "DockArc Freight Case",        50000,         77,
  "Oryn Delta",         "HardSEal Transit Crate",     100000,        387,
  "Oryn Delta",         "LongHaul Value Canister",    150000,        155,
  "Oryn Delta",         "Quantum Crate Module",       250000,        116
)

# =============================================================================
# 6. NAME MAP: NEW -> HISTORICAL CONTAINER LEVELS
# =============================================================================

name_map_cargo <- tibble::tribble(
  ~container_type_raw,         ~container_type,
  "DeepSpace Haul Box",        "DeepSpace Haulbox",
  "DockArc Freight Case",      "DockArc Freight Case",
  "HardSEal Transit Crate",    "HardSeal Transit Crate",
  "LongHaul Value Canister",   "LongHaul Vault Canister",
  "Quantum Crate Module",      "QuantumCrate Module"
)

# =============================================================================
# 7. NEW SYSTEM ENVIRONMENT TABLE
# -----------------------------------------------------------------------------
# Derived from encyclopedia entries
# These are system-level overrides for environmental predictors
# =============================================================================

system_env_tbl <- tibble::tribble(
  ~solar_system,         ~route_risk, ~solar_radiation, ~debris_density, ~distance_from_earth_au, ~transit_mult, ~pilot_mult,
  "Helionis Cluster",       4.0,           0.14,             0.32,               120,                 1.00,          1.03,
  "Bayesia System",         3.0,           0.30,             0.16,               175,                 1.15,          1.00,
  "Oryn Delta",             5.0,           0.22,             0.35,               240,                 1.35,          0.96
)

# =============================================================================
# 8. PROTOTYPE MATCHING TABLE
# -----------------------------------------------------------------------------
# Historical prototype by container_type, to be inherited by new portfolio
# cargo_type, cargo_value, transit_duration, pilot_experience come from history
# system environment variables are then overridden by system_env_tbl
# =============================================================================

# hard checks
if (!all(name_map_cargo$container_type %in% hist_container_levels)) {
  missing_levels <- setdiff(name_map_cargo$container_type, hist_container_levels)
  stop("Mapped container_type not found in historical data: ",
       paste(missing_levels, collapse = ", "))
}

# -----------------------------------------------------------------------------
# Build new prediction dataframe
# -----------------------------------------------------------------------------
new_pred_df <- new_cargo_raw %>%
  left_join(name_map_cargo, by = "container_type_raw") %>%
  left_join(container_proto_tbl, by = "container_type") %>%
  left_join(system_env_tbl, by = "solar_system") %>%
  mutate(
    solar_system_original = solar_system,
    
    # Exposure method 2:
    # first capacity proxy = quantity × max_volume (kg)
    capacity_kg = quantity * max_volume,
    
    # then calibrate back to historical model exposure scale
    exposure = capacity_kg / hist_kg_per_exposure,
    
    # Route B prototype matching + system override
    cargo_type = cargo_type_mode,
    cargo_value = cargo_value_med,
    transit_duration = transit_duration_med * transit_mult,
    pilot_experience = pilot_experience_med * pilot_mult
  ) %>%
  select(
    solar_system_original,
    container_type,
    quantity,
    max_volume,
    capacity_kg,
    exposure,
    cargo_type,
    cargo_value,
    route_risk,
    solar_radiation,
    debris_density,
    transit_duration,
    pilot_experience
  )

# -----------------------------------------------------------------------------
# Validation checks
# -----------------------------------------------------------------------------
stopifnot(!any(is.na(new_pred_df$solar_system_original)))
stopifnot(!any(is.na(new_pred_df$container_type)))
stopifnot(!any(is.na(new_pred_df$cargo_type)))
stopifnot(!any(is.na(new_pred_df$cargo_value)))
stopifnot(!any(is.na(new_pred_df$route_risk)))
stopifnot(!any(is.na(new_pred_df$solar_radiation)))
stopifnot(!any(is.na(new_pred_df$debris_density)))
stopifnot(!any(is.na(new_pred_df$transit_duration)))
stopifnot(!any(is.na(new_pred_df$pilot_experience)))
stopifnot(!any(is.na(new_pred_df$exposure)))
stopifnot(all(new_pred_df$exposure > 0))

cat("\n====================================================\n")
cat("New cargo synthetic shipment strata (Route B)\n")
cat("====================================================\n")
print(new_pred_df, n = Inf)

# =============================================================================
# 9. PREPARE NEWDATA FOR HISTORICAL FREQUENCY MODEL
# -----------------------------------------------------------------------------
# Historical model predictors:
#   route_risk + solar_radiation + debris_density + cargo_type +
#   container_type + cargo_value + transit_duration + pilot_experience
#
# If the fitted model also uses exposure/offset, pass exposure here.
# =============================================================================
hist_route_risk_levels <- levels(as.factor(cargo_freq_hist$route_risk))

pred_for_model <- new_pred_df %>%
  transmute(
    route_risk       = factor(as.character(route_risk), levels = hist_route_risk_levels),
    solar_radiation  = solar_radiation,
    debris_density   = debris_density,
    cargo_type       = factor(cargo_type, levels = hist_cargo_type_levels),
    container_type   = factor(container_type, levels = hist_container_levels),
    cargo_value      = cargo_value,
    transit_duration = transit_duration,
    pilot_experience = pilot_experience,
    exposure         = exposure
  )

# -----------------------------------------------------------------------------
# Predict expected claim counts
# -----------------------------------------------------------------------------
new_pred_df <- new_pred_df %>%
  mutate(
    lambda_glm = as.numeric(predict(final_model_cl, newdata = pred_for_model, type = "response"))
  )

# =============================================================================
# 10. DISPERSION HANDLING FOR QUASI-POISSON MC
# -----------------------------------------------------------------------------
# For quasipoisson, exact simulation distribution is not defined.
# We approximate frequency uncertainty by moment-matched Negative Binomial:
#
#   Var(N) = phi * mu
#
# NB variance:
#   Var(N) = mu + mu^2 / size
#
# => size_i = mu_i / (phi - 1), if phi > 1
# If phi <= 1, revert to Poisson
# =============================================================================

phi_hat <- tryCatch(summary(final_model_cl)$dispersion, error = function(e) NA_real_)

if (is.na(phi_hat)) phi_hat <- 1

cat("\n====================================================\n")
cat("Frequency dispersion used in MC\n")
cat("====================================================\n")
cat("phi_hat =", phi_hat, "\n")

# =============================================================================
# 11. ATTACH SEVERITY FITS TO NEW STRATA
# =============================================================================

new_pred_df <- new_pred_df %>%
  left_join(severity_fit_tbl, by = "container_type")

if (any(sapply(new_pred_df$sev_fit, is.null))) {
  missing_sev <- new_pred_df %>%
    filter(sapply(sev_fit, is.null)) %>%
    distinct(container_type) %>%
    pull(container_type)
  stop("Missing severity model for container_type: ", paste(missing_sev, collapse = ", "))
}

# =============================================================================
# 12. ONE MONTE CARLO SIMULATION
# -----------------------------------------------------------------------------
# Frequency: quasi-Poisson approximated by NB if phi_hat > 1, else Poisson
# Severity: container-type-specific spliced severity, scaled by cargo value
# =============================================================================

simulate_one_cargo <- function(input_df, phi_hat = 1) {
  
  sim_df <- input_df
  
  # -------------------------
  # Frequency simulation
  # -------------------------
  if (phi_hat > 1.0001) {
    size_vec <- pmax(sim_df$lambda_glm / (phi_hat - 1), 1e-8)
    sim_df$N_sim <- rnbinom(
      n = nrow(sim_df),
      mu = sim_df$lambda_glm,
      size = size_vec
    )
  } else {
    sim_df$N_sim <- rpois(
      n = nrow(sim_df),
      lambda = sim_df$lambda_glm
    )
  }
  
  # -------------------------
  # Severity + aggregate loss
  # -------------------------
  sim_df$agg_loss <- pmap_dbl(
    list(sim_df$sev_fit, sim_df$N_sim, sim_df$cargo_value),
    function(sev_fit, n_claims, cargo_value) {
      if (n_claims <= 0) return(0)
      sev_draws <- rsev_from_fit(
        fit_obj = sev_fit,
        n = n_claims,
        cargo_value_new = cargo_value,
        alpha_scale = 0.70
      )
      sum(sev_draws)
    }
  )
  
  sim_df
}

# =============================================================================
# 13. RUN MONTE CARLO
# =============================================================================

n_sim <- 10000

cargo_sim_results_list <- vector("list", n_sim)

for (i in seq_len(n_sim)) {
  cargo_sim_results_list[[i]] <- simulate_one_cargo(new_pred_df, phi_hat = phi_hat) %>%
    mutate(sim = i)
}

cargo_sim_results <- bind_rows(cargo_sim_results_list)

# =============================================================================
# 14. AGGREGATE RESULTS
# =============================================================================

agg_by_strata <- cargo_sim_results %>%
  group_by(sim, solar_system_original, container_type) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    exposure    = sum(exposure),
    quantity    = sum(quantity),
    .groups = "drop"
  )

agg_by_system <- cargo_sim_results %>%
  group_by(sim, solar_system_original) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  )

agg_total <- cargo_sim_results %>%
  group_by(sim) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  )

# =============================================================================
# 15. SUMMARY STATISTICS
# =============================================================================

summary_by_strata <- agg_by_strata %>%
  group_by(solar_system_original, container_type) %>%
  summarise(
    mean_loss = mean(agg_loss),
    sd_loss   = sd(agg_loss),
    p95_loss  = quantile(agg_loss, 0.95),
    p99_loss  = quantile(agg_loss, 0.99),
    tvar95    = tvar_fn(agg_loss, 0.95),
    mean_N    = mean(claim_count),
    p95_N     = quantile(claim_count, 0.95),
    exposure  = first(exposure),
    quantity  = first(quantity),
    .groups = "drop"
  ) %>%
  mutate(
    mean_loss_per_unit     = mean_loss / quantity,
    mean_loss_per_exposure = mean_loss / exposure
  )

summary_by_system <- agg_by_system %>%
  group_by(solar_system_original) %>%
  summarise(
    mean_loss = mean(agg_loss),
    sd_loss   = sd(agg_loss),
    p95_loss  = quantile(agg_loss, 0.95),
    p99_loss  = quantile(agg_loss, 0.99),
    tvar95    = tvar_fn(agg_loss, 0.95),
    mean_N    = mean(claim_count),
    p95_N     = quantile(claim_count, 0.95),
    .groups = "drop"
  )

summary_total <- agg_total %>%
  summarise(
    mean_loss = mean(agg_loss),
    sd_loss   = sd(agg_loss),
    p95_loss  = quantile(agg_loss, 0.95),
    p99_loss  = quantile(agg_loss, 0.99),
    tvar95    = tvar_fn(agg_loss, 0.95),
    mean_N    = mean(claim_count),
    p95_N     = quantile(claim_count, 0.95)
  )

# =============================================================================
# 16. PRINT MAIN OUTPUTS
# =============================================================================

cat("\n====================================================\n")
cat("Deterministic expected frequency by cargo stratum\n")
cat("====================================================\n")
print(
  new_pred_df %>%
    select(
      solar_system_original, container_type, quantity, max_volume, exposure,
      cargo_type, cargo_value, route_risk, solar_radiation, debris_density,
      transit_duration, pilot_experience, lambda_glm
    ),
  n = Inf
)

cat("\n====================================================\n")
cat("Cargo Monte Carlo summary by solar_system × container_type\n")
cat("====================================================\n")
print(summary_by_strata, n = Inf)

cat("\n====================================================\n")
cat("Cargo Monte Carlo summary by solar_system\n")
cat("====================================================\n")
print(summary_by_system, n = Inf)

cat("\n====================================================\n")
cat("Cargo Monte Carlo summary for total portfolio\n")
cat("====================================================\n")
print(summary_total)

# =============================================================================
# 17. OPTIONAL PLOTS
# =============================================================================

# -----------------------------------------------------------------------------
# 17. Cargo aggregate loss distribution plots (readable, consistent style)
# Style: density + fill overlay, title centered & bold, legend on top
# -----------------------------------------------------------------------------

# 17.1 Total aggregate loss distribution
xlim_total <- quantile(agg_total$agg_loss, 0.99, na.rm = TRUE)

p_total <- ggplot(agg_total, aes(x = agg_loss)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 60,
    alpha = 0.4,
    fill = "steelblue",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_density(linewidth = 0.9, colour = "steelblue") +
  coord_cartesian(xlim = c(0, xlim_total)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "MC Distribution of Total Cargo Aggregate Loss",
    x = "Total Aggregate Loss",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

print(p_total)

# 17.2 Aggregate loss by solar system (overlay style like EF)
system_long <- agg_by_system %>%
  transmute(
    sim,
    SolarSystem = solar_system_original,
    loss = agg_loss
  )

xlim_system <- quantile(system_long$loss, 0.99, na.rm = TRUE)

p_system <- ggplot(
  system_long,
  aes(x = loss, color = SolarSystem, fill = SolarSystem)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  coord_cartesian(xlim = c(0, xlim_system)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Cargo Aggregate Loss Distribution by Solar System",
    x = "Aggregate Loss",
    y = "Density",
    colour = "Solar System",
    fill = "Solar System"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_system)

# 17.3 Aggregate loss by container type (same overlay style)
agg_by_container <- cargo_sim_results %>%
  group_by(sim, container_type) %>%
  summarise(
    agg_loss = sum(agg_loss),
    .groups = "drop"
  )

container_order <- c(
  "QuantumCrate Module",
  setdiff(sort(unique(as.character(agg_by_container$container_type))), "QuantumCrate Module")
)

agg_by_container <- agg_by_container %>%
  mutate(container_type = factor(as.character(container_type), levels = container_order))

container_long <- agg_by_container %>%
  transmute(
    sim,
    ContainerType = container_type,
    loss = agg_loss
  )

xlim_container <- quantile(container_long$loss, 0.99, na.rm = TRUE)

p_container <- ggplot(
  container_long,
  aes(x = loss, color = ContainerType, fill = ContainerType)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  coord_cartesian(xlim = c(0, xlim_container)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Cargo Aggregate Loss Distribution by Container Type",
    x = "Aggregate Loss",
    y = "Density",
    colour = "Container Type",
    fill = "Container Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_container)


# =============================================================================
# 18. EXPORTS simulation
# =============================================================================
# =============================================================================
# Cargo report: set output folder and export CSV + PNG
# =============================================================================

library(dplyr)
library(scales)
library(ggplot2)

# -----------------------------------------------------------------------------
# 0. Set output folder (change path if needed)
# =============================================================================
out_dir <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/cargo_report_output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# =============================================================================
# 1. Export CSV tables
# =============================================================================

# 1.1 Prediction / transfer input (drop list columns so write.csv works)
write.csv(
  new_pred_df %>% select(where(~ !is.list(.))),
  file.path(out_dir, "cargo_pred_df.csv"),
  row.names = FALSE
)

# 1.2 Summary tables
write.csv(
  summary_by_strata,
  file.path(out_dir, "cargo_summary_by_strata.csv"),
  row.names = FALSE
)

write.csv(
  summary_by_system,
  file.path(out_dir, "cargo_summary_by_system.csv"),
  row.names = FALSE
)

write.csv(
  summary_total,
  file.path(out_dir, "cargo_summary_total.csv"),
  row.names = FALSE
)

# 1.3 Simulation / aggregate outputs
write.csv(
  agg_by_strata,
  file.path(out_dir, "cargo_agg_by_strata_by_sim.csv"),
  row.names = FALSE
)

write.csv(
  agg_by_system,
  file.path(out_dir, "cargo_agg_by_system_by_sim.csv"),
  row.names = FALSE
)

write.csv(
  agg_total,
  file.path(out_dir, "cargo_agg_total_by_sim.csv"),
  row.names = FALSE
)

write.csv(
  cargo_sim_results %>% select(where(~ !is.list(.))),
  file.path(out_dir, "cargo_sim_results_all.csv"),
  row.names = FALSE
)

# =============================================================================
# 2. Export PNG figures (requires p_total, p_system, p_container from plotting section)
# =============================================================================

ggsave(
  filename = file.path(out_dir, "fig_cargo_total_loss.png"),
  plot = p_total,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "fig_cargo_loss_by_system.png"),
  plot = p_system,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "fig_cargo_loss_by_container.png"),
  plot = p_container,
  width = 8,
  height = 5,
  dpi = 300
)

# =============================================================================
# 3. Print confirmation
# =============================================================================

cat("\n====================================================\n")
cat("Cargo report outputs exported to:\n")
cat(out_dir, "\n")
cat("====================================================\n")

cat("\nFiles exported:\n")
cat("- cargo_pred_df.csv\n")
cat("- cargo_summary_by_strata.csv\n")
cat("- cargo_summary_by_system.csv\n")
cat("- cargo_summary_total.csv\n")
cat("- cargo_agg_by_strata_by_sim.csv\n")
cat("- cargo_agg_by_system_by_sim.csv\n")
cat("- cargo_agg_total_by_sim.csv\n")
cat("- cargo_sim_results_all.csv\n")
cat("- fig_cargo_total_loss.png\n")
cat("- fig_cargo_loss_by_system.png\n")
cat("- fig_cargo_loss_by_container.png\n")
