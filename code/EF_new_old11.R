# =============================================================================
# Equipment Failure (EF) — Transfer historical frequency model to new data
# (new -> old: use final_model_ef + freq_ef to predict on new portfolio)
# =============================================================================
# Prerequisite: Run "Equipment failure freq.R" so that final_model_ef and
#               train/test exist, OR load freq_ef and a saved final_model_ef.
# =============================================================================

library(dplyr)
library(tidyr)
library(tibble)

set.seed(123)

# -----------------------------------------------------------------------------
# 0. Load historical data and model
# -----------------------------------------------------------------------------
# Option A: If you have already run the frequency script in this session:
#   final_model_ef, freq_ef, train are already in the environment.
# Option B: Load cleaned data and refit or load saved model.
freq_ef <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/ef_freq_cleaned.rds")

# final_model_ef 来自路径 "C:/Users/laptop/Desktop/UNSW study/ACTL5100"，名为 "Equipment failure freq.R"
# 若环境中没有 final_model_ef：可先运行 "Equipment failure freq.R"，或从保存的 .rds 加载：
if (!exists("final_model_ef")) {
  model_path <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/final_model_ef.rds"
  if (file.exists(model_path)) {
    final_model_ef <- readRDS(model_path)
    message("Loaded final_model_ef from ", model_path)
  } else {
    stop("final_model_ef not found. Run 'Equipment failure freq.R' first or save/load the model.")
  }
}

# =============================================================================
# Equipment Failure (EF) — Transfer historical frequency model to new portfolio
# Final design chosen:
#
#   Strata level:
#       solar_system × equipment_type
#
#   Historical frequency model:
#       final_model_ef
#
#   Historical frequency dataset:
#       freq_ef
#
#   Severity simulation:
#       - Quantum Bore -> Group A: r_final_a()
#       - Other equipment -> Group B: ef_sev_dists$dist_B$Q(runif(n))
#
#   Important transfer assumptions:
#       1. Helionis Cluster is INCLUDED as part of the new portfolio
#       2. New solar systems cannot be directly used in predict(),
#          because they are not historical factor levels
#       3. Therefore, all strata are predicted using Helionis Cluster
#          as the historical baseline solar_system
#       4. Only equipment-level residual risk adjustment is retained
#       5. equipment_age is set at solar_system × equipment_type level
#       6. exposure = machine_count × average exposure ratio per machine
#       7. Monte Carlo simulation runs = 10,000
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

set.seed(123)

# =============================================================================
# 0. REQUIRED OBJECTS ALREADY IN ENVIRONMENT
# -----------------------------------------------------------------------------
# Required objects:
#   final_model_ef
#   freq_ef
#   r_final_a
#   ef_sev_dists
# =============================================================================


# =============================================================================
# 1. EXTRACT HISTORICAL MODEL INFORMATION
# =============================================================================

hist_equipment_levels <- levels(freq_ef$equipment_type)
hist_system_levels    <- levels(freq_ef$solar_system)

usage_by_type <- freq_ef %>%
  group_by(equipment_type) %>%
  summarise(
    usage_int_hist_mean = mean(usage_int, na.rm = TRUE),
    .groups = "drop"
  )

usage_overall_mean <- mean(freq_ef$usage_int, na.rm = TRUE)
max_age_hist <- max(freq_ef$equipment_age, na.rm = TRUE)

baseline_system_for_prediction <- "Helionis Cluster"

if (!(baseline_system_for_prediction %in% hist_system_levels)) {
  stop("Helionis Cluster is not found in historical solar_system levels.")
}


# =============================================================================
# 2. ENTER NEW PORTFOLIO DATA FROM THE INVENTORY TABLE
# -----------------------------------------------------------------------------
# IMPORTANT:
# Equipment names below are written exactly as shown in your new-data sheet.
# You may need to map them to the exact historical factor levels later.
# =============================================================================

new_counts_raw <- tibble::tribble(
  ~solar_system,        ~equipment_type_raw,        ~machine_count,
  "Helionis Cluster",   "Quantum Bores",                 300,
  "Helionis Cluster",   "Graviton Extractors",           240,
  "Helionis Cluster",   "Fexstram Carriers",             150,
  "Helionis Cluster",   "ReglAggregators",                300,
  "Helionis Cluster",   "Flux Riders",                  1500,
  "Helionis Cluster",   "Ion Pulverizers",                90,
  
  "Bayesian System",    "Quantum Bores",                 150,
  "Bayesian System",    "Graviton Extractors",           120,
  "Bayesian System",    "Fexstram Carriers",              75,
  "Bayesian System",    "ReglAggregators",                150,
  "Bayesian System",    "Flux Riders",                   750,
  "Bayesian System",    "Ion Pulverizers",                45,
  
  "Oryn Delta",         "Quantum Bores",                 100,
  "Oryn Delta",         "Graviton Extractors",            80,
  "Oryn Delta",         "Fexstram Carriers",              50,
  "Oryn Delta",         "ReglAggregators",                100,
  "Oryn Delta",         "Flux Riders",                   500,
  "Oryn Delta",         "Ion Pulverizers",                30
)


# =============================================================================
# 2A. SERVICE YEAR DISTRIBUTION TABLE
# -----------------------------------------------------------------------------
# We will convert the service-year band distribution into expected age
# for each solar_system × equipment_type stratum.
# =============================================================================

age_dist_raw <- tibble::tribble(
  ~solar_system,        ~age_band, ~equipment_type_raw,       ~count,
  
  "Helionis Cluster",   "<5",      "Quantum Bores",             30,
  "Helionis Cluster",   "<5",      "Graviton Extractors",       24,
  "Helionis Cluster",   "<5",      "Fexstram Carriers",         15,
  "Helionis Cluster",   "<5",      "ReglAggregators",            30,
  "Helionis Cluster",   "<5",      "Flux Riders",              150,
  "Helionis Cluster",   "<5",      "Ion Pulverizers",            9,
  
  "Helionis Cluster",   "5-9",     "Quantum Bores",             45,
  "Helionis Cluster",   "5-9",     "Graviton Extractors",       36,
  "Helionis Cluster",   "5-9",     "Fexstram Carriers",         23,
  "Helionis Cluster",   "5-9",     "ReglAggregators",            45,
  "Helionis Cluster",   "5-9",     "Flux Riders",              225,
  "Helionis Cluster",   "5-9",     "Ion Pulverizers",           14,
  
  "Helionis Cluster",   "10-14",   "Quantum Bores",            180,
  "Helionis Cluster",   "10-14",   "Graviton Extractors",      144,
  "Helionis Cluster",   "10-14",   "Fexstram Carriers",         89,
  "Helionis Cluster",   "10-14",   "ReglAggregators",           180,
  "Helionis Cluster",   "10-14",   "Flux Riders",              900,
  "Helionis Cluster",   "10-14",   "Ion Pulverizers",           53,
  
  "Helionis Cluster",   "15-19",   "Quantum Bores",             30,
  "Helionis Cluster",   "15-19",   "Graviton Extractors",       24,
  "Helionis Cluster",   "15-19",   "Fexstram Carriers",         15,
  "Helionis Cluster",   "15-19",   "ReglAggregators",            30,
  "Helionis Cluster",   "15-19",   "Flux Riders",              150,
  "Helionis Cluster",   "15-19",   "Ion Pulverizers",            9,
  
  "Helionis Cluster",   "20+",     "Quantum Bores",             15,
  "Helionis Cluster",   "20+",     "Graviton Extractors",       12,
  "Helionis Cluster",   "20+",     "Fexstram Carriers",          8,
  "Helionis Cluster",   "20+",     "ReglAggregators",            15,
  "Helionis Cluster",   "20+",     "Flux Riders",               75,
  "Helionis Cluster",   "20+",     "Ion Pulverizers",            5,
  
  "Bayesian System",    "<5",      "Quantum Bores",             45,
  "Bayesian System",    "<5",      "Graviton Extractors",       36,
  "Bayesian System",    "<5",      "Fexstram Carriers",         23,
  "Bayesian System",    "<5",      "ReglAggregators",            45,
  "Bayesian System",    "<5",      "Flux Riders",              225,
  "Bayesian System",    "<5",      "Ion Pulverizers",           14,
  
  "Bayesian System",    "5-9",     "Quantum Bores",             38,
  "Bayesian System",    "5-9",     "Graviton Extractors",       30,
  "Bayesian System",    "5-9",     "Fexstram Carriers",         19,
  "Bayesian System",    "5-9",     "ReglAggregators",            38,
  "Bayesian System",    "5-9",     "Flux Riders",              187,
  "Bayesian System",    "5-9",     "Ion Pulverizers",           11,
  
  "Bayesian System",    "10-14",   "Quantum Bores",             59,
  "Bayesian System",    "10-14",   "Graviton Extractors",       48,
  "Bayesian System",    "10-14",   "Fexstram Carriers",         29,
  "Bayesian System",    "10-14",   "ReglAggregators",            59,
  "Bayesian System",    "10-14",   "Flux Riders",              300,
  "Bayesian System",    "10-14",   "Ion Pulverizers",           18,
  
  "Bayesian System",    "15-19",   "Quantum Bores",              8,
  "Bayesian System",    "15-19",   "Graviton Extractors",        6,
  "Bayesian System",    "15-19",   "Fexstram Carriers",          4,
  "Bayesian System",    "15-19",   "ReglAggregators",             8,
  "Bayesian System",    "15-19",   "Flux Riders",               38,
  "Bayesian System",    "15-19",   "Ion Pulverizers",            2,
  
  "Bayesian System",    "20+",     "Quantum Bores",              0,
  "Bayesian System",    "20+",     "Graviton Extractors",        0,
  "Bayesian System",    "20+",     "Fexstram Carriers",          0,
  "Bayesian System",    "20+",     "ReglAggregators",             0,
  "Bayesian System",    "20+",     "Flux Riders",                0,
  "Bayesian System",    "20+",     "Ion Pulverizers",            0,
  
  "Oryn Delta",         "<5",      "Quantum Bores",             75,
  "Oryn Delta",         "<5",      "Graviton Extractors",       60,
  "Oryn Delta",         "<5",      "Fexstram Carriers",         37,
  "Oryn Delta",         "<5",      "ReglAggregators",            75,
  "Oryn Delta",         "<5",      "Flux Riders",              375,
  "Oryn Delta",         "<5",      "Ion Pulverizers",           22,
  
  "Oryn Delta",         "5-9",     "Quantum Bores",             15,
  "Oryn Delta",         "5-9",     "Graviton Extractors",       12,
  "Oryn Delta",         "5-9",     "Fexstram Carriers",          8,
  "Oryn Delta",         "5-9",     "ReglAggregators",            15,
  "Oryn Delta",         "5-9",     "Flux Riders",               75,
  "Oryn Delta",         "5-9",     "Ion Pulverizers",            5,
  
  "Oryn Delta",         "10-14",   "Quantum Bores",             10,
  "Oryn Delta",         "10-14",   "Graviton Extractors",        8,
  "Oryn Delta",         "10-14",   "Fexstram Carriers",          5,
  "Oryn Delta",         "10-14",   "ReglAggregators",            10,
  "Oryn Delta",         "10-14",   "Flux Riders",               50,
  "Oryn Delta",         "10-14",   "Ion Pulverizers",            3,
  
  "Oryn Delta",         "15-19",   "Quantum Bores",              0,
  "Oryn Delta",         "15-19",   "Graviton Extractors",        0,
  "Oryn Delta",         "15-19",   "Fexstram Carriers",          0,
  "Oryn Delta",         "15-19",   "ReglAggregators",             0,
  "Oryn Delta",         "15-19",   "Flux Riders",                0,
  "Oryn Delta",         "15-19",   "Ion Pulverizers",            0,
  
  "Oryn Delta",         "20+",     "Quantum Bores",              0,
  "Oryn Delta",         "20+",     "Graviton Extractors",        0,
  "Oryn Delta",         "20+",     "Fexstram Carriers",          0,
  "Oryn Delta",         "20+",     "ReglAggregators",             0,
  "Oryn Delta",         "20+",     "Flux Riders",                0,
  "Oryn Delta",         "20+",     "Ion Pulverizers",            0
)

age_midpoint <- function(x) {
  dplyr::case_when(
    x == "<5"    ~ 2.5,
    x == "5-9"   ~ 7,
    x == "10-14" ~ 12,
    x == "15-19" ~ 17,
    x == "20+"   ~ 22,
    TRUE ~ NA_real_
  )
}

expected_age_tbl <- age_dist_raw %>%
  mutate(age_mid = age_midpoint(age_band)) %>%
  group_by(solar_system, equipment_type_raw) %>%
  summarise(
    equipment_age = sum(count * age_mid) / sum(count),
    .groups = "drop"
  )


# =============================================================================
# 2B. USAGE AND MAINTENANCE TABLE
# -----------------------------------------------------------------------------
# % in Operation is used to construct exposure:
#   exposure = machine_count × avg_exposure_ratio_per_machine
# =============================================================================

ops_maint_tbl <- tibble::tribble(
  ~solar_system,        ~equipment_type_raw,       ~avg_exposure_ratio_per_machine, ~maintenance_int,
  "Helionis Cluster",   "Quantum Bores",                     0.95,                     750,
  "Helionis Cluster",   "Graviton Extractors",               0.95,                     750,
  "Helionis Cluster",   "Fexstram Carriers",                 0.90,                     375,
  "Helionis Cluster",   "ReglAggregators",                    0.80,                    1500,
  "Helionis Cluster",   "Flux Riders",                       0.80,                    1500,
  "Helionis Cluster",   "Ion Pulverizers",                   0.50,                    1000,
  
  "Bayesian System",    "Quantum Bores",                     0.80,                     600,
  "Bayesian System",    "Graviton Extractors",               0.80,                     600,
  "Bayesian System",    "Fexstram Carriers",                 0.75,                     400,
  "Bayesian System",    "ReglAggregators",                    0.75,                    1000,
  "Bayesian System",    "Flux Riders",                       0.80,                    1000,
  "Bayesian System",    "Ion Pulverizers",                   0.60,                     750,
  
  "Oryn Delta",         "Quantum Bores",                     0.75,                     500,
  "Oryn Delta",         "Graviton Extractors",               0.75,                     500,
  "Oryn Delta",         "Fexstram Carriers",                 0.70,                     250,
  "Oryn Delta",         "ReglAggregators",                    0.70,                     300,
  "Oryn Delta",         "Flux Riders",                       0.75,                     300,
  "Oryn Delta",         "Ion Pulverizers",                   0.50,                     500
)



# =============================================================================
# 2C. RISK TABLE
# =============================================================================

eq_risk_tbl <- tibble::tribble(
  ~equipment_type_raw,       ~risk_helionis, ~risk_bayesian, ~risk_oryn,
  "Quantum Bores",                0.69,          0.77,         0.93,
  "Graviton Extractors",          0.48,          0.57,         0.73,
  "Fexstram Carriers",            0.67,          0.71,         0.78,
  "ReglAggregators",               0.24,          0.26,         0.35,
  "Flux Riders",                  0.24,          0.21,         0.20,
  "Ion Pulverizers",              0.64,          0.66,         0.75
)

name_map <- tibble::tribble(
  ~equipment_type_raw,       ~equipment_type,
  "Quantum Bores",           "Quantum Bore",
  "Graviton Extractors",     "Graviton Extractor",
  "Fexstram Carriers",       "FexStram Carrier",
  "ReglAggregators",          "ReglAggregators",
  "Flux Riders",             "Flux Rider",
  "Ion Pulverizers",         "Ion Pulverizer"
)
# =============================================================================
# 3. EQUIPMENT-LEVEL RESIDUAL RISK ADJUSTMENT
# -----------------------------------------------------------------------------
# Use the raw risk table defined above (eq_risk_tbl) and map raw names to the
# historical equipment_type names used in the fitted frequency model.
# =============================================================================

eq_multiplier_tbl <- eq_risk_tbl %>%
  left_join(name_map, by = "equipment_type_raw") %>%
  mutate(
    equipment_type = trimws(equipment_type)
  ) %>%
  transmute(
    equipment_type,
    helionis_mult = risk_helionis / risk_helionis,
    bayesian_mult = risk_bayesian / risk_helionis,
    oryn_mult     = risk_oryn / risk_helionis
  ) %>%
  pivot_longer(
    cols = c(helionis_mult, bayesian_mult, oryn_mult),
    names_to = "system_key",
    values_to = "eq_multiplier"
  ) %>%
  mutate(
    solar_system = case_when(
      system_key == "helionis_mult" ~ "Helionis Cluster",
      system_key == "bayesian_mult" ~ "Bayesian System",
      system_key == "oryn_mult"     ~ "Oryn Delta"
    ),
    solar_system = trimws(solar_system)
  ) %>%
  dplyr::select(solar_system, equipment_type, eq_multiplier)
# =============================================================================
# 4. BUILD PREDICTION DATASET
# -----------------------------------------------------------------------------
# IMPORTANT:
#   solar_system_original = real portfolio system name
#   solar_system_model    = baseline historical system used for predict()
#
# Because Bayesian / Oryn are not in historical model factor levels,
# predict() must use a historical level. We choose Helionis Cluster.
# =============================================================================

# -----------------------------------------------------------------------------
# Build the base new-portfolio table first
# This step only combines:
#   - machine counts
#   - expected age
#   - operation/exposure ratio
#   - maintenance interval
#   - mapped historical equipment name
#
# Do NOT create usage_int or eq_multiplier here yet,
# because those need additional joins in the next step.
# -----------------------------------------------------------------------------

new_portfolio_raw <- new_counts_raw %>%
  left_join(expected_age_tbl, by = c("solar_system", "equipment_type_raw")) %>%
  left_join(ops_maint_tbl,    by = c("solar_system", "equipment_type_raw")) %>%
  left_join(name_map,         by = "equipment_type_raw") %>%
  mutate(
    solar_system_original = solar_system,
    solar_system_model    = baseline_system_for_prediction
  ) %>%
  dplyr::select(
    solar_system_original,
    solar_system_model,
    equipment_type_raw,
    equipment_type,
    machine_count,
    avg_exposure_ratio_per_machine,
    equipment_age,
    maintenance_int
  )

# -----------------------------------------------------------------------------
# Optional check: inspect whether any equipment type failed to map
# If any equipment_type is NA here, name_map needs fixing.
# -----------------------------------------------------------------------------
print(new_portfolio_raw, n = Inf)

# -----------------------------------------------------------------------------
# Build final prediction dataframe
# This step joins:
#   - historical usage assumption by equipment type
#   - equipment-level residual multiplier
#
# Then construct:
#   - exposure = machine_count × avg_exposure_ratio_per_machine
#   - usage_int = historical mean usage by type, fallback to overall mean
# -----------------------------------------------------------------------------

pred_df <- new_portfolio_raw %>%
  left_join(usage_by_type, by = "equipment_type") %>%
  left_join(eq_multiplier_tbl,
            by = c("solar_system_original" = "solar_system",
                   "equipment_type" = "equipment_type")) %>%
  mutate(
    exposure  = machine_count * avg_exposure_ratio_per_machine,
    usage_int = dplyr::coalesce(usage_int_hist_mean, usage_overall_mean)
  ) %>%
  select(
    solar_system_original,
    solar_system_model,
    equipment_type,
    machine_count,
    avg_exposure_ratio_per_machine,
    exposure,
    equipment_age,
    maintenance_int,
    usage_int,
    eq_multiplier
  )
# -----------------------------------------------------------------------------
# Validation checks
# -----------------------------------------------------------------------------

stopifnot(!any(is.na(pred_df$solar_system_model)))
stopifnot(!any(is.na(pred_df$equipment_type)))
stopifnot(!any(is.na(pred_df$equipment_age)))
stopifnot(!any(is.na(pred_df$maintenance_int)))
stopifnot(!any(is.na(pred_df$usage_int)))
stopifnot(!any(is.na(pred_df$eq_multiplier)))
stopifnot(!any(is.na(pred_df$exposure)))
stopifnot(all(pred_df$exposure > 0))


# =============================================================================
# 5. PREDICT EXPECTED CLAIM COUNTS USING HISTORICAL FREQUENCY MODEL
# -----------------------------------------------------------------------------
# Since final_model_ef was trained on historical solar_system levels only,
# we use solar_system_model = Helionis Cluster for all rows.
#
# Then we externally adjust using eq_multiplier.
# =============================================================================

pred_for_model <- pred_df %>%
  transmute(
    equipment_type = factor(equipment_type, levels = hist_equipment_levels),
    solar_system   = factor(solar_system_model, levels = hist_system_levels),
    equipment_age  = equipment_age,
    maintenance_int = maintenance_int,
    usage_int      = usage_int,
    exposure       = exposure
  )

pred_df <- pred_df %>%
  mutate(
    lambda_glm = predict(final_model_ef, newdata = pred_for_model, type = "response"),
    lambda_adj = lambda_glm * eq_multiplier
  )

# -----------------------------------------------------------------------------
# Deterministic frequency check
# -----------------------------------------------------------------------------

freq_check <- pred_df %>%
  group_by(solar_system_original, equipment_type) %>%
  summarise(
    machine_count         = sum(machine_count),
    exposure              = sum(exposure),
    expected_claims_glm   = sum(lambda_glm),
    expected_claims_adj   = sum(lambda_adj),
    .groups = "drop"
  )

print(freq_check)


# =============================================================================
# 5B. LOAD SEVERITY DISTRIBUTIONS (if not already in environment)
# -----------------------------------------------------------------------------
# Required for r_sev_ef(): ef_sev_dists (from "Equipment failure sev.R" save).
# =============================================================================

if (!exists("ef_sev_dists")) {
  ef_sev_dists_path <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/ef_sev_dists.rds"
  if (!file.exists(ef_sev_dists_path)) {
    stop("ef_sev_dists.rds not found. Run 'Equipment failure sev.R' first to create it.")
  }
  ef_sev_dists <- readRDS(ef_sev_dists_path)
  message("Loaded ef_sev_dists from ", ef_sev_dists_path)
}

if (!exists("r_final_a")) {
  r_final_a <- function(n_samp, cap = Inf) {
    if (n_samp <= 0) return(numeric(0))
    pmin(ef_sev_dists$dist_A$Q(stats::runif(n_samp)), cap)
  }
}

# =============================================================================
# 6. DEFINE SEVERITY RANDOM GENERATOR
# =============================================================================

r_sev_ef <- function(eq_type, n) {
  
  if (n <= 0) return(numeric(0))
  
  eq_type <- trimws(as.character(eq_type))
  
  if (eq_type == "Quantum Bore") {
    
    # Group A severity
    sev <- r_final_a(n)
    
  } else {
    
    # Group B severity
    sev <- ef_sev_dists$dist_B$Q(stats::runif(n))
    
  }
  
  return(sev)
}

# =============================================================================
# 7. DEFINE ONE MONTE CARLO SIMULATION
# =============================================================================

simulate_one_ef <- function(input_df) {
  
  sim_df <- input_df
  
  sim_df$N_sim <- rpois(
    n = nrow(sim_df),
    lambda = sim_df$lambda_adj
  )
  
  sim_df$agg_loss <- map2_dbl(
    sim_df$equipment_type,
    sim_df$N_sim,
    ~{
      eq_type  <- as.character(.x)
      n_claims <- .y
      
      if (n_claims == 0) return(0)
      
      sev_draws <- r_sev_ef(eq_type = eq_type, n = n_claims  )
      sum(sev_draws)
    }
  )
  
  sim_df
}


# =============================================================================
# 8. RUN 10,000 MONTE CARLO SIMULATIONS
# =============================================================================

n_sim <- 10000

sim_results_list <- vector("list", n_sim)

for (i in seq_len(n_sim)) {
  sim_results_list[[i]] <- simulate_one_ef(pred_df ) %>%
    mutate(sim = i)
}

sim_results <- bind_rows(sim_results_list)



# =============================================================================
# 9. AGGREGATE SIMULATION RESULTS
# -----------------------------------------------------------------------------
# IMPORTANT:
# Use solar_system_original for final reporting,
# because solar_system_model is only the baseline proxy used inside predict().
# =============================================================================

agg_by_strata <- sim_results %>%
  group_by(sim, solar_system_original, equipment_type) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  )

agg_by_system <- sim_results %>%
  group_by(sim, solar_system_original) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  )

agg_total <- sim_results %>%
  group_by(sim) %>%
  summarise(
    agg_loss    = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  )


# =============================================================================
# 10. SUMMARY STATISTICS
# =============================================================================

tvar_fn <- function(x, p = 0.95) {
  q <- as.numeric(quantile(x, p, na.rm = TRUE))
  mean(x[x >= q], na.rm = TRUE)
}

summary_by_strata <- agg_by_strata %>%
  group_by(solar_system_original, equipment_type) %>%
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
# 11. OPTIONAL PRICING-STYLE METRICS
# =============================================================================

base_info <- pred_df %>%
  group_by(solar_system_original, equipment_type) %>%
  summarise(
    machine_count = sum(machine_count),
    exposure      = sum(exposure),
    .groups = "drop"
  )

summary_by_strata <- summary_by_strata %>%
  left_join(base_info, by = c("solar_system_original", "equipment_type")) %>%
  mutate(
    mean_loss_per_machine  = mean_loss / machine_count,
    mean_loss_per_exposure = mean_loss / exposure
  )


# =============================================================================
# 12. PRINT MAIN OUTPUTS
# =============================================================================

cat("\n====================================================\n")
cat("Deterministic expected frequency by stratum\n")
cat("====================================================\n")
print(freq_check)

cat("\n====================================================\n")
cat("Monte Carlo summary by solar_system × equipment_type\n")
cat("====================================================\n")
print(summary_by_strata)

cat("\n====================================================\n")
cat("Monte Carlo summary by solar_system\n")
cat("====================================================\n")
print(summary_by_system)

cat("\n====================================================\n")
cat("Monte Carlo summary for total portfolio\n")
cat("====================================================\n")
print(summary_total)


#-------------------------------------------------------------
#Check Quantum Bore simulation
#------------------------------------------------------------

qb_test <- r_final_a(1000)
cat("Mean severity =", mean(qb_test), "\n")
cat("Min severity =", min(qb_test), "\n")
cat("Max severity =", max(qb_test), "\n")
cat("Zero draws =", sum(qb_test == 0), "\n")

qb_summary_by_system <- summary_by_strata %>%
  filter(equipment_type == "Quantum Bore")

qb_summary_total <- sim_results %>%
  filter(trimws(as.character(equipment_type)) == "Quantum Bore") %>%
  group_by(sim) %>%
  summarise(
    agg_loss = sum(agg_loss),
    claim_count = sum(N_sim),
    .groups = "drop"
  ) %>%
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
# 11. OPTIONAL EXPORTS
# =============================================================================

# write.csv(qb_summary_by_system, "qb_summary_by_system.csv", row.names = FALSE)
# write.csv(qb_summary_total, "qb_summary_total.csv", row.names = FALSE)
# write.csv(qb_sim_results, "qb_sim_results_all.csv", row.names = FALSE)

#---------------------------
#plots
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

# =============================================================================
# 11A. Total portfolio loss
# =============================================================================
df_total <- data.frame(loss = agg_total$agg_loss)

p_total <- ggplot(df_total, aes(x = loss)) +
  geom_histogram(
    bins = 80,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  labs(
    title = "Simulated annual EF loss (Total portfolio)",
    x = "Loss",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p_total)


# =============================================================================
# 11B. By equipment type
# =============================================================================
equipment_long <- sim_results %>%
  group_by(sim, equipment_type) %>%
  summarise(
    loss = sum(agg_loss),
    .groups = "drop"
  ) %>%
  mutate(
    equipment_type = as.character(equipment_type)
  )

p_equipment_overlay <- ggplot(
  equipment_long,
  aes(x = loss, color = equipment_type, fill = equipment_type)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  labs(
    title = "Simulated annual EF loss by equipment type",
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_equipment_overlay)


# =============================================================================
# 11C. By solar system
# =============================================================================
system_long <- agg_by_system %>%
  transmute(
    sim,
    SolarSystem = solar_system_original,
    loss = agg_loss
  )

p_system_overlay <- ggplot(
  system_long,
  aes(x = loss, color = SolarSystem, fill = SolarSystem)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  labs(
    title = "Simulated annual EF loss by solar system",
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_system_overlay)

# =============================================================================
# 11D. By strata (solar_system × equipment_type)
# =============================================================================
strata_long <- agg_by_strata %>%
  mutate(
    SolarSystem = solar_system_original,
    EquipmentType = as.character(equipment_type),
    loss = agg_loss
  )

p_strata_overlay <- ggplot(
  strata_long,
  aes(x = loss, color = EquipmentType, fill = EquipmentType)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_wrap(~ SolarSystem, scales = "free_y", ncol = 2) +
  labs(
    title = "Simulated annual EF loss by stratum (Solar System × Equipment Type)",
    x = "Aggregate Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_strata_overlay)

# =============================================================================
# 12. EXPORT RESULTS
# =============================================================================

out_dir <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/ef_report_output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 数值四舍五入，便于报告
round_df <- function(df, digits = 2) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

# -----------------------------------------------------------------------------
# 12A. Tables -> CSV (summary + prediction + simulation aggregates)
# -----------------------------------------------------------------------------
write.csv(round_df(summary_total, 2),
          file.path(out_dir, "ef_summary_total.csv"),
          row.names = FALSE)

write.csv(round_df(summary_by_system, 2),
          file.path(out_dir, "ef_summary_by_system.csv"),
          row.names = FALSE)

write.csv(round_df(summary_by_strata, 2),
          file.path(out_dir, "ef_summary_by_strata.csv"),
          row.names = FALSE)

write.csv(round_df(freq_check, 2),
          file.path(out_dir, "ef_freq_check.csv"),
          row.names = FALSE)

write.csv(
  round_df(pred_df %>% select(where(~ !is.list(.))), 4),
  file.path(out_dir, "ef_pred_df.csv"),
  row.names = FALSE
)

# Simulation outputs: per-sim aggregate by total, system, strata; and raw sim_results
write.csv(round_df(agg_total, 2),
          file.path(out_dir, "ef_agg_total_by_sim.csv"),
          row.names = FALSE)

write.csv(round_df(agg_by_system, 2),
          file.path(out_dir, "ef_agg_by_system_by_sim.csv"),
          row.names = FALSE)

write.csv(round_df(agg_by_strata, 2),
          file.path(out_dir, "ef_agg_by_strata_by_sim.csv"),
          row.names = FALSE)

write.csv(
  round_df(sim_results %>% select(where(~ !is.list(.))), 2),
  file.path(out_dir, "ef_sim_results_all.csv"),
  row.names = FALSE
)

# -----------------------------------------------------------------------------
# 12B. Figures -> PNG
# -----------------------------------------------------------------------------
fig_width  <- 7
fig_height <- 4.5
dpi        <- 300

ggsave(
  file.path(out_dir, "fig_ef_total_loss.png"),
  p_total,
  width = fig_width,
  height = fig_height,
  dpi = dpi,
  bg = "white"
)

ggsave(
  file.path(out_dir, "fig_ef_loss_by_equipment.png"),
  p_equipment_overlay,
  width = fig_width,
  height = fig_height,
  dpi = dpi,
  bg = "white"
)

ggsave(
  file.path(out_dir, "fig_ef_loss_by_system.png"),
  p_system_overlay,
  width = fig_width,
  height = fig_height,
  dpi = dpi,
  bg = "white"
)

ggsave(
  file.path(out_dir, "fig_ef_loss_by_strata.png"),
  p_strata_overlay,
  width = 9,
  height = 6,
  dpi = dpi,
  bg = "white"
)

message("Exported to: ", normalizePath(out_dir))
list.files(out_dir)