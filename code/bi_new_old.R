# =============================================================================
# Business Interruption (BI)
# Transfer from historical model to new solar systems using proxy-only method
# No detailed new portfolio data available
# =============================================================================

rm(list = setdiff(ls(), c("freq_bi", "sev_bi", "final_model_bi")))

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(MASS)

set.seed(123)

# =============================================================================
# 0. Output folder
# =============================================================================
output_dir <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/bi_report_output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# 1. Basic checks
# =============================================================================
if (!exists("freq_bi")) stop("freq_bi not found in environment.")
if (!exists("sev_bi")) stop("sev_bi not found in environment.")
if (!exists("final_model_bi")) stop("final_model_bi not found in environment.")

required_freq_cols <- c("solar_system", "production_load", "energy_backup_score", "exposure", "claim_count")
missing_freq_cols <- setdiff(required_freq_cols, names(freq_bi))
if (length(missing_freq_cols) > 0) {
  stop("freq_bi is missing columns: ", paste(missing_freq_cols, collapse = ", "))
}

sev_amount_col <- c("claim_amount", "loss_amount", "severity", "amount")
sev_amount_col <- sev_amount_col[sev_amount_col %in% names(sev_bi)]

if (length(sev_amount_col) == 0) {
  stop("sev_bi must contain one of these columns: claim_amount / loss_amount / severity / amount")
}
sev_amount_col <- sev_amount_col[1]

# =============================================================================
# 2. Helper functions
# =============================================================================
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# =============================================================================
# 3. Historical factor levels from fitted model
# =============================================================================
model_solar_levels <- final_model_bi$xlevels$solar_system
model_backup_levels <- final_model_bi$xlevels$energy_backup_score

if (is.null(model_solar_levels)) {
  model_solar_levels <- levels(freq_bi$solar_system)
}
if (is.null(model_backup_levels)) {
  model_backup_levels <- sort(unique(as.character(freq_bi$energy_backup_score)))
}

freq_bi <- freq_bi %>%
  dplyr::mutate(
    solar_system = factor(as.character(solar_system), levels = model_solar_levels),
    energy_backup_score = factor(as.character(energy_backup_score), levels = as.character(model_backup_levels))
  )

# =============================================================================
# 4. Proxy mapping for new solar systems
# =============================================================================
bi_proxy_table <- tibble::tribble(
  ~solar_system_new,   ~solar_system_proxy,    ~weight, ~proxy_reason,
  "Helionis Cluster",  "Helionis Cluster",     1.00,    "Direct historical system in model",
  "Bayesia System",    "Zeta",                 0.70,    "Radiation spikes + operational resilience, closest to Zeta",
  "Bayesia System",    "Epsilon",              0.30,    "Some additional harsh-radiation / temperature stress overlay",
  "Oryn Delta",        "Zeta",                 0.65,    "Operational instability / navigation complexity closer to Zeta",
  "Oryn Delta",        "Helionis Cluster",     0.35,    "Retains some communication / cluster-fragmentation similarity"
)

# =============================================================================
# 5. Exposure assumptions using equipment counts
# =============================================================================
hist_exposure_by_system <- freq_bi %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    total_exposure_hist = sum(exposure, na.rm = TRUE),
    avg_exposure_per_record = mean(exposure, na.rm = TRUE),
    n_records = dplyr::n(),
    .groups = "drop"
  )

bi_equipment_exposure <- tibble::tibble(
  solar_system_new = c("Helionis Cluster", "Bayesia System", "Oryn Delta"),
  equipment_total = c(2580, 1290, 860)
)

helionis_hist_exposure <- freq_bi %>%
  dplyr::filter(as.character(solar_system) == "Helionis Cluster") %>%
  dplyr::summarise(total_exposure = sum(exposure, na.rm = TRUE)) %>%
  dplyr::pull(total_exposure)

helionis_equipment_total <- bi_equipment_exposure %>%
  dplyr::filter(solar_system_new == "Helionis Cluster") %>%
  dplyr::pull(equipment_total)

bi_exposure_table <- bi_equipment_exposure %>%
  dplyr::mutate(
    total_exposure_assumed = helionis_hist_exposure * equipment_total / helionis_equipment_total
  ) %>%
  dplyr::select(solar_system_new, total_exposure_assumed, equipment_total)

print(hist_exposure_by_system)
print(bi_exposure_table)

# =============================================================================
# 6. Historical proxy statistics
# =============================================================================
hist_proxy_stats <- freq_bi %>%
  dplyr::group_by(solar_system) %>%
  dplyr::summarise(
    production_load_mean = mean(production_load, na.rm = TRUE),
    energy_backup_score_mode = as.character(get_mode(as.character(energy_backup_score))),
    exposure_mean = mean(exposure, na.rm = TRUE),
    exposure_total = sum(exposure, na.rm = TRUE),
    claim_rate = sum(claim_count, na.rm = TRUE) / sum(exposure, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 7. Build assumed input table
# =============================================================================
bi_assumption_table <- bi_proxy_table %>%
  dplyr::left_join(
    bi_exposure_table,
    by = "solar_system_new"
  ) %>%
  dplyr::left_join(
    hist_proxy_stats %>%
      dplyr::rename(
        solar_system_proxy = solar_system,
        production_load_assumed = production_load_mean,
        energy_backup_score_assumed = energy_backup_score_mode,
        proxy_claim_rate = claim_rate
      ),
    by = "solar_system_proxy"
  ) %>%
  dplyr::mutate(
    exposure_assumed = total_exposure_assumed * weight,
    solar_system_proxy = factor(as.character(solar_system_proxy), levels = model_solar_levels),
    energy_backup_score_assumed = factor(
      as.character(energy_backup_score_assumed),
      levels = as.character(model_backup_levels)
    )
  )

if (any(is.na(bi_assumption_table$solar_system_proxy))) {
  stop("Some proxy solar systems are not in the fitted model levels.")
}
if (any(is.na(bi_assumption_table$energy_backup_score_assumed))) {
  stop("Some assumed energy_backup_score values are not in the fitted model levels.")
}

# =============================================================================
# 8. Predict frequency on proxy rows using historical NB model
# =============================================================================
bi_assumption_table$mu_hat_proxy <- predict(
  final_model_bi,
  newdata = bi_assumption_table %>%
    dplyr::transmute(
      solar_system = solar_system_proxy,
      production_load = production_load_assumed,
      energy_backup_score = energy_backup_score_assumed,
      exposure = exposure_assumed
    ),
  type = "response"
)

bi_pred_summary <- bi_assumption_table %>%
  dplyr::group_by(solar_system_new) %>%
  dplyr::summarise(
    total_exposure_assumed = sum(exposure_assumed, na.rm = TRUE),
    expected_claim_count = sum(mu_hat_proxy, na.rm = TRUE),
    implied_claim_rate = expected_claim_count / total_exposure_assumed,
    .groups = "drop"
  )

# =============================================================================
# 9. Severity fit (lognormal, no spliced)
# =============================================================================
sev_vec <- sev_bi[[sev_amount_col]]
sev_vec <- sev_vec[is.finite(sev_vec) & !is.na(sev_vec) & sev_vec > 0]

if (length(sev_vec) == 0) {
  stop("No positive severity values found in sev_bi.")
}

meanlog_bi <- mean(log(sev_vec))
sdlog_bi   <- sd(log(sev_vec))

bi_sev_table <- tibble::tibble(
  severity_distribution = "Lognormal",
  claim_amount_column = sev_amount_col,
  meanlog = meanlog_bi,
  sdlog = sdlog_bi
)

# =============================================================================
# 10. Theta settings
# =============================================================================
theta_bi_fitted <- final_model_bi$theta
theta_bi_1 <- 1
theta_bi_5 <- 5

cat("theta_bi_fitted =", theta_bi_fitted, "\n")
cat("theta_bi_1 =", theta_bi_1, "\n")
cat("theta_bi_5 =", theta_bi_5, "\n")

bi_theta_table <- tibble::tibble(
  theta_scenario = c("theta_1", "theta_5", "theta_fitted"),
  theta_used = c(theta_bi_1, theta_bi_5, theta_bi_fitted),
  note = c(
    "Moderated dispersion scenario",
    "Milder dispersion scenario",
    "Record-level fitted NB theta"
  )
)

# =============================================================================
# 11. Monte Carlo simulation
# =============================================================================
n_sim <- 10000

simulate_bi_system <- function(system_name, mu_total, theta, meanlog, sdlog, n_sim = 10000) {
  claim_count_sim <- rnbinom(n_sim, mu = mu_total, size = theta)
  agg_loss_sim <- numeric(n_sim)
  
  for (i in seq_len(n_sim)) {
    n_i <- claim_count_sim[i]
    if (n_i > 0) {
      agg_loss_sim[i] <- sum(rlnorm(n_i, meanlog = meanlog, sdlog = sdlog))
    } else {
      agg_loss_sim[i] <- 0
    }
  }
  
  tibble::tibble(
    sim = seq_len(n_sim),
    solar_system = system_name,
    claim_count = claim_count_sim,
    agg_loss = agg_loss_sim
  )
}

run_bi_mc <- function(theta_value, scenario_name, bi_pred_summary, meanlog_bi, sdlog_bi, n_sim = 10000) {
  
  bi_agg_by_system_by_sim <- purrr::map_dfr(
    seq_len(nrow(bi_pred_summary)),
    function(i) {
      simulate_bi_system(
        system_name = bi_pred_summary$solar_system_new[i],
        mu_total    = bi_pred_summary$expected_claim_count[i],
        theta       = theta_value,
        meanlog     = meanlog_bi,
        sdlog       = sdlog_bi,
        n_sim       = n_sim
      )
    }
  ) %>%
    dplyr::mutate(
      theta_scenario = scenario_name,
      theta_used = theta_value
    )
  
  bi_agg_total_by_sim <- bi_agg_by_system_by_sim %>%
    dplyr::group_by(theta_scenario, theta_used, sim) %>%
    dplyr::summarise(
      claim_count = sum(claim_count),
      agg_loss = sum(agg_loss),
      .groups = "drop"
    )
  
  list(
    by_system = bi_agg_by_system_by_sim,
    total = bi_agg_total_by_sim
  )
}

bi_mc_theta1 <- run_bi_mc(
  theta_value   = theta_bi_1,
  scenario_name = "theta_1",
  bi_pred_summary = bi_pred_summary,
  meanlog_bi = meanlog_bi,
  sdlog_bi = sdlog_bi,
  n_sim = n_sim
)

bi_mc_theta5 <- run_bi_mc(
  theta_value   = theta_bi_5,
  scenario_name = "theta_5",
  bi_pred_summary = bi_pred_summary,
  meanlog_bi = meanlog_bi,
  sdlog_bi = sdlog_bi,
  n_sim = n_sim
)

bi_mc_fitted <- run_bi_mc(
  theta_value   = theta_bi_fitted,
  scenario_name = "theta_fitted",
  bi_pred_summary = bi_pred_summary,
  meanlog_bi = meanlog_bi,
  sdlog_bi = sdlog_bi,
  n_sim = n_sim
)

bi_agg_by_system_by_sim_all <- dplyr::bind_rows(
  bi_mc_theta1$by_system,
  bi_mc_theta5$by_system,
  bi_mc_fitted$by_system
)

bi_agg_total_by_sim_all <- dplyr::bind_rows(
  bi_mc_theta1$total,
  bi_mc_theta5$total,
  bi_mc_fitted$total
)

# =============================================================================
# 12. Summaries
# =============================================================================
bi_summary_system <- bi_agg_by_system_by_sim_all %>%
  dplyr::group_by(theta_scenario, theta_used, solar_system) %>%
  dplyr::summarise(
    mean_claim_count = mean(claim_count),
    sd_claim_count   = sd(claim_count),
    p95_claim_count  = as.numeric(quantile(claim_count, 0.95)),
    p99_claim_count  = as.numeric(quantile(claim_count, 0.99)),
    mean_agg_loss    = mean(agg_loss),
    sd_agg_loss      = sd(agg_loss),
    p50_agg_loss     = as.numeric(quantile(agg_loss, 0.50)),
    p75_agg_loss     = as.numeric(quantile(agg_loss, 0.75)),
    p90_agg_loss     = as.numeric(quantile(agg_loss, 0.90)),
    p95_agg_loss     = as.numeric(quantile(agg_loss, 0.95)),
    p99_agg_loss     = as.numeric(quantile(agg_loss, 0.99)),
    max_agg_loss     = max(agg_loss),
    zero_prop_claim  = mean(claim_count == 0),
    .groups = "drop"
  )

bi_summary_total <- bi_agg_total_by_sim_all %>%
  dplyr::group_by(theta_scenario, theta_used) %>%
  dplyr::summarise(
    mean_claim_count = mean(claim_count),
    sd_claim_count   = sd(claim_count),
    p95_claim_count  = as.numeric(quantile(claim_count, 0.95)),
    p99_claim_count  = as.numeric(quantile(claim_count, 0.99)),
    mean_agg_loss    = mean(agg_loss),
    sd_agg_loss      = sd(agg_loss),
    p50_agg_loss     = as.numeric(quantile(agg_loss, 0.50)),
    p75_agg_loss     = as.numeric(quantile(agg_loss, 0.75)),
    p90_agg_loss     = as.numeric(quantile(agg_loss, 0.90)),
    p95_agg_loss     = as.numeric(quantile(agg_loss, 0.95)),
    p99_agg_loss     = as.numeric(quantile(agg_loss, 0.99)),
    max_agg_loss     = max(agg_loss),
    zero_prop_claim  = mean(claim_count == 0),
    .groups = "drop"
  )

cat("\n====================================================\n")
cat("BI transfer assumptions table\n")
cat("====================================================\n")
print(bi_assumption_table, n = Inf)

cat("\n====================================================\n")
cat("BI expected frequency summary for new solar systems\n")
cat("====================================================\n")
print(bi_pred_summary, n = Inf)

cat("\n====================================================\n")
cat("BI Monte Carlo summary by solar system\n")
cat("====================================================\n")
print(bi_summary_system, n = Inf)

cat("\n====================================================\n")
cat("BI Monte Carlo summary for total portfolio\n")
cat("====================================================\n")
print(bi_summary_total, n = Inf)

# =============================================================================
# 13. Select scenario for plots and single-scenario exports
# =============================================================================
selected_theta_scenario <- "theta_1"
# 可改成 "theta_5" 或 "theta_fitted"

bi_agg_by_system_by_sim <- bi_agg_by_system_by_sim_all %>%
  dplyr::filter(theta_scenario == selected_theta_scenario)

bi_agg_total_by_sim <- bi_agg_total_by_sim_all %>%
  dplyr::filter(theta_scenario == selected_theta_scenario)

# =============================================================================
# 14. Plots
# =============================================================================
xlim_system <- quantile(bi_agg_by_system_by_sim$agg_loss, 0.99, na.rm = TRUE)
xlim_total  <- quantile(bi_agg_total_by_sim$agg_loss, 0.99, na.rm = TRUE)
xlim_claim  <- quantile(bi_agg_by_system_by_sim$claim_count, 0.99, na.rm = TRUE)

df_total <- bi_agg_total_by_sim %>%
  dplyr::transmute(loss = agg_loss)

p_bi_total_loss <- ggplot(df_total, aes(x = loss)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white", alpha = 0.8) +
  coord_cartesian(xlim = c(0, xlim_total)) +
  labs(
    title = paste0("Simulated annual BI loss (Total portfolio) - ", selected_theta_scenario),
    x = "Loss",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

p_bi_total_loss_log <- ggplot(df_total, aes(x = loss)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white", alpha = 0.8) +
  scale_x_log10() +
  labs(
    title = paste0("Simulated annual BI loss (Total portfolio, log scale) - ", selected_theta_scenario),
    x = "Loss (log scale)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

system_long <- bi_agg_by_system_by_sim %>%
  dplyr::transmute(
    sim_id = sim,
    SolarSystem = solar_system,
    loss = agg_loss
  )

p_bi_loss_by_system <- ggplot(system_long, aes(x = loss, color = SolarSystem, fill = SolarSystem)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  coord_cartesian(xlim = c(0, xlim_system)) +
  labs(
    title = paste0("Simulated annual BI loss by solar system - ", selected_theta_scenario),
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

p_bi_loss_by_system_log <- ggplot(system_long, aes(x = loss, color = SolarSystem, fill = SolarSystem)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  scale_x_log10() +
  labs(
    title = paste0("Simulated annual BI loss by solar system (log scale) - ", selected_theta_scenario),
    x = "Loss (log scale)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

claim_long <- bi_agg_by_system_by_sim %>%
  dplyr::transmute(
    sim_id = sim,
    SolarSystem = solar_system,
    claim_count = claim_count
  )

p_bi_claim_by_system <- ggplot(claim_long, aes(x = claim_count, color = SolarSystem)) +
  geom_freqpoly(linewidth = 0.8, bins = 80) +
  coord_cartesian(xlim = c(0, xlim_claim)) +
  labs(
    title = paste0("Simulated annual BI claim count by solar system - ", selected_theta_scenario),
    x = "Claim count",
    y = "Frequency",
    color = "SolarSystem"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

ggsave(
  filename = file.path(output_dir, paste0("fig_bi_total_loss_", selected_theta_scenario, ".png")),
  plot = p_bi_total_loss,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("fig_bi_total_loss_log_", selected_theta_scenario, ".png")),
  plot = p_bi_total_loss_log,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("fig_bi_loss_by_system_", selected_theta_scenario, ".png")),
  plot = p_bi_loss_by_system,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("fig_bi_loss_by_system_log_", selected_theta_scenario, ".png")),
  plot = p_bi_loss_by_system_log,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, paste0("fig_bi_claim_by_system_", selected_theta_scenario, ".png")),
  plot = p_bi_claim_by_system,
  width = 9,
  height = 6,
  dpi = 300
)

try(print(p_bi_total_loss), silent = TRUE)
try(print(p_bi_total_loss_log), silent = TRUE)
try(print(p_bi_loss_by_system), silent = TRUE)
try(print(p_bi_loss_by_system_log), silent = TRUE)
try(print(p_bi_claim_by_system), silent = TRUE)

# =============================================================================
# 15. Export CSV files
# =============================================================================
safe_write_csv <- function(df, path) {
  tryCatch(
    {
      write.csv(df, path, row.names = FALSE)
      message("Saved: ", path)
    },
    error = function(e) {
      message("Failed to save: ", path, " | ", e$message)
    }
  )
}

safe_write_csv(bi_proxy_table,
               file.path(output_dir, "bi_proxy_table.csv"))

safe_write_csv(bi_exposure_table,
               file.path(output_dir, "bi_exposure_table.csv"))

safe_write_csv(hist_proxy_stats,
               file.path(output_dir, "bi_hist_proxy_stats.csv"))

safe_write_csv(bi_assumption_table,
               file.path(output_dir, "bi_assumption_table.csv"))

safe_write_csv(bi_pred_summary,
               file.path(output_dir, "bi_pred_summary.csv"))

safe_write_csv(bi_sev_table,
               file.path(output_dir, "bi_severity_table.csv"))

safe_write_csv(bi_theta_table,
               file.path(output_dir, "bi_theta_table.csv"))

safe_write_csv(bi_agg_by_system_by_sim_all,
               file.path(output_dir, "bi_agg_by_system_by_sim_all.csv"))

safe_write_csv(bi_agg_total_by_sim_all,
               file.path(output_dir, "bi_agg_total_by_sim_all.csv"))

safe_write_csv(bi_summary_system,
               file.path(output_dir, "bi_summary_system_by_theta.csv"))

safe_write_csv(bi_summary_total,
               file.path(output_dir, "bi_summary_total_by_theta.csv"))

safe_write_csv(bi_agg_by_system_by_sim,
               file.path(output_dir, paste0("bi_agg_by_system_by_sim_", selected_theta_scenario, ".csv")))

safe_write_csv(bi_agg_total_by_sim,
               file.path(output_dir, paste0("bi_agg_total_by_sim_", selected_theta_scenario, ".csv")))

# =============================================================================
# 16. Final message
# =============================================================================
cat("\n====================================================\n")
cat("Selected plotting/export scenario:\n")
cat(selected_theta_scenario, "\n")
cat("All BI outputs have been saved to:\n")
cat(output_dir, "\n")
cat("====================================================\n")