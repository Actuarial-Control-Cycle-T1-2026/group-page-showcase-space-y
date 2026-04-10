# =========================================================
# Step 0. INPUTS
# 这一部分放：
# - 新数据（personnel / equipment / usage）
# - old freq_wcuency transfer inputs
# - WC sev_wcerity distribution parameters
# - sev_wcerity multipliers
# =========================================================

#install.packages("dplyr")
#install.packages("evd")
library(dplyr)
library(tidyr)
library(tibble)


set.seed(123)

freq_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_freq_cleaned.rds")
sev_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_sev_cleaned.rds")
# final_model_wc 来自脚本 "Works compensation freq.R"
# 路径：C:\Users\laptop\Desktop\UNSW study\ACTL5100；文件名：Works compensation freq.R
# 若环境中没有 final_model_wc：可先运行 "Works compensation freq.R" 得到模型，或在彼脚本中
# saveRDS(final_model, "final_model_wc.rds") 后在此处加载：
if (!exists("final_model_wc")) {
  model_path <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/final_model_wc.rds"
  if (file.exists(model_path)) {
    final_model_wc <- readRDS(model_path)
    message("Loaded final_model_wc from ", model_path)
  } else {
    stop("final_model_wc not found. Run 'Works compensation freq.R' first, or save the model there with saveRDS(final_model, 'final_model_wc.rds') and place it in ACTL5100.")
  }
}

# ---------------------------------------------------------
# 0A. Personnel Summary
# 数学意思：
# 这是 WC exposure reconstruction 的原始 role-level workforce data
# ---------------------------------------------------------

personnel_tbl <- tibble::tribble(
  ~Department, ~Role, ~Employees, ~FullTime, ~Contract, ~AvgSalary, ~AvgAge, ~ContractMix,
  "Management", "Executive", 12, 12, 0, 500000, 55, 0.000,
  "Management", "Vice President", 54, 54, 0, 250000, 48, 0.000,
  "Management", "Director", 111, 111, 0, 150000, 41, 0.000,
  "Administration", "HR", 426, 426, 0, 80000, 40, 0.000,
  "Administration", "IT", 568, 426, 142, 85000, 35, 0.250,
  "Administration", "Legal", 142, 85, 57, 110000, 45, 0.401,
  "Administration", "Finance & Accounting", 426, 398, 28, 100000, 38, 0.066,
  "Environmental & Safety", "Environmental Scientists", 711, 569, 142, 120000, 39, 0.200,
  "Environmental & Safety", "Safety Officer", 2132, 2132, 0, 80000, 30, 0.000,
  "Environmental & Safety", "Medical Personnel", 1421, 1137, 284, 130000, 34, 0.200,
  "Exploration Operations", "Geologist", 152, 152, 0, 120000, 35, 0.000,
  "Exploration Operations", "Scientist", 211, 169, 42, 120000, 38, 0.199,
  "Exploration Operations", "Field Technician", 842, 421, 421, 55000, 29, 0.500,
  "Extraction Operations", "Drilling Operators", 2526, 1684, 842, 60000, 36, 0.333,
  "Extraction Operations", "Maintenance", 10316, 8253, 2063, 65000, 32, 0.200,
  "Extraction Operations", "Engineers", 2684, 2684, 0, 95000, 35, 0.000,
  "Extraction Operations", "Freight Operators", 3553, 2842, 711, 60000, 30, 0.200,
  "Extraction Operations", "Robotics Technician", 711, 711, 0, 75000, 29, 0.000,
  "Spacecraft Operations", "Navigation Officers", 2842, 2842, 0, 85000, 56, 0.000,
  "Spacecraft Operations", "Maintenance (Spacecraft)", 1137, 1137, 0, 65000, 31, 0.000,
  "Spacecraft Operations", "Security Personnel", 2132, 1421, 711, 55000, 27, 0.333,
  "Spacecraft Operations", "Steward", 568, 568, 0, 65000, 42, 0.000,
  "Spacecraft Operations", "Galleyhand", 2132, 0, 2132, 40000, 35, 1.000
)

# ---------------------------------------------------------
# 0B. Role mapping: detailed role -> WC bucket
# 数学意思：
# 把新 role 映射到 4 个 WC modelling strata
# ---------------------------------------------------------

role_map_wc <- tibble::tribble(
  ~Role, ~occ_bucket,
  "Executive", "Office/Admin",
  "Vice President", "Office/Admin",
  "Director", "Office/Admin",
  "HR", "Office/Admin",
  "IT", "Technical",
  "Legal", "Office/Admin",
  "Finance & Accounting", "Office/Admin",
  "Environmental Scientists", "Technical",
  "Safety Officer", "Field Operations",
  "Medical Personnel", "Technical",
  "Geologist", "Technical",
  "Scientist", "Technical",
  "Field Technician", "Field Operations",
  "Drilling Operators", "Heavy Operations",
  "Maintenance", "Field Operations",
  "Engineers", "Technical",
  "Freight Operators", "Heavy Operations",
  "Robotics Technician", "Technical",
  "Navigation Officers", "Technical",
  "Maintenance (Spacecraft)", "Field Operations",
  "Security Personnel", "Field Operations",
  "Steward", "Office/Admin",
  "Galleyhand", "Heavy Operations"
)

# ---------------------------------------------------------
# 0C. Mining Equipment counts
# 数学意思：
# 用于构造 system operational scale proxy
# ---------------------------------------------------------

equip_count_wide <- tibble::tribble(
  ~EquipmentType, ~Helionis_Cluster, ~Bayesia_System, ~Oryn_Delta,
  "Quantum Bores", 300, 150, 100,
  "Graviton Extractors", 240, 120, 80,
  "Fexstram Carriers", 150, 75, 50,
  "ReglAggregators", 300, 150, 100,
  "Flux Riders", 1500, 750, 500,
  "Ion Pulverizers", 90, 45, 30
)

# ---------------------------------------------------------
# 0D. Equipment Usage / Maintenance Schedule
# 数学意思：
# WC system proxy 只先使用 % in operation
# ---------------------------------------------------------

equip_usage_wide <- tibble::tribble(
  ~EquipmentType, ~Helionis_op, ~Helionis_maint, ~Bayesia_op, ~Bayesia_maint, ~Oryn_op, ~Oryn_maint,
  "Quantum Bores", 0.95, 750, 0.80, 600, 0.75, 500,
  "Graviton Extractors", 0.95, 750, 0.80, 600, 0.75, 500,
  "Fexstram Carriers", 0.90, 375, 0.75, 400, 0.70, 250,
  "ReglAggregators", 0.80, 1500, 0.75, 1000, 0.70, 300,
  "Flux Riders", 0.80, 1500, 0.80, 1000, 0.75, 300,
  "Ion Pulverizers", 0.50, 1000, 0.60, 750, 0.50, 500
)

# ---------------------------------------------------------
# 0E. Judgment-based solar system risk factors for WC freq_wcuency
# 数学意思：
# system effect 只放在 freq_wcuency side
# 这个设定是qualitative的，可以找文献支持/ 是主观决定的，但不要直接这么说
# ---------------------------------------------------------

wc_system_factor <- tibble::tribble(
  ~SolarSystem, ~F_s,
  "Helionis Cluster", 1.00,
  "Bayesia System", 1.20,
  "Oryn Delta", 1.45
)

# =========================================================
# 0F. Derive WC bucket relativities from old model using observed covariate mix
# =========================================================

library(dplyr)
library(tibble)

# 0) old occupation -> new WC bucket mapping
# 请按你的旧数据 occupation 名字检查并补全

old_occ_map <- tibble::tribble(
  ~occupation,              ~occ_bucket,
  "Administrator",          "Office/Admin",
  "Executive",              "Office/Admin",
  "Manager",                "Office/Admin",
  "Engineer",               "Technical",
  "Scientist",              "Technical",
  "Technology Officer",     "Technical",
  "Maintenance Staff",      "Field Operations",
  "Planetary Operations",   "Field Operations",
  "Safety Officer",         "Field Operations",
  "Spacecraft Operator",    "Field Operations",
  "Drill Operator",         "Heavy Operations"
)


# 1) 用旧 freq_wcuency model 对 old training data 每条记录算 fitted mean 和 fitted rate
#
# 同时假设 old data 叫 freq_wc，暴露列叫 exposure

train_pred <- freq_wc %>%
  dplyr::mutate(
    fitted_mu = predict(final_model_wc, newdata = freq_wc, type = "response"),
    fitted_lambda = fitted_mu / exposure
  )

# 3) 在 observed covariate mix 下，聚合成 bucket-level fitted claim rate
#
# 数学上：
# bucket_rate_o = sum_i fitted_mu_i / sum_i exposure_i

wc_bucket_rate <- train_pred %>%
  left_join(old_occ_map, by = "occupation") %>%
  filter(!is.na(occ_bucket)) %>%
  group_by(occ_bucket) %>%
  summarise(
    bucket_exposure = sum(exposure, na.rm = TRUE),
    bucket_mu       = sum(fitted_mu, na.rm = TRUE),
    bucket_rate     = bucket_mu / bucket_exposure,
    .groups = "drop"
  )

print(wc_bucket_rate)

# 4) 选择 reference bucket
# 这里以 Techical 为基准

ref_rate <- wc_bucket_rate %>%
  filter(occ_bucket == "Technical") %>%
  pull(bucket_rate)

lambda_wc_base_old <- ref_rate


# 5) 计算 relativity
# relativity_o = bucket_rate_o / ref_rate

wc_relativity <- wc_bucket_rate %>%
  mutate(
    relativity = bucket_rate / ref_rate
  ) %>%
  dplyr::select(occ_bucket, relativity)

print(wc_relativity)

# 6) 可选：一起看 bucket rate，方便写报告

wc_bucket_rate_table <- wc_bucket_rate %>%
  mutate(
    relativity = bucket_rate / ref_rate
  ) %>%
  arrange(desc(bucket_rate))

print(wc_bucket_rate_table)

# 7) 现在这两个对象就是你后面 transfer 要直接用的
print(lambda_wc_base_old)
print(wc_relativity)
# ---------------------------------------------------------
# 0G. freq_wcuency dispersion parameter for WC
# 数学意思：
# N_o ~ NB(mu_o, theta_wc)
#
# 如果你最后不用 NB 而继续用 Poisson，可以把这一段改掉
# ---------------------------------------------------------

#theta_wc <- 12

# ---------------------------------------------------------
# 0H. WC sev_wcerity: Spliced distribution (Lognormal body + GPD tail)
# Body: LN(mu_body, sd_body) on (0, u_chosen], mass phi_u
# Tail:  GPD(loc=0, scale=sigma_hat, shape=xi_hat) for excess above u_chosen
# CDF:   F(q) = phi_u * [LN(q)/LN(u)] for q<=u; phi_u + (1-phi_u)*GPD(q-u) for q>u
# ---------------------------------------------------------

# Body (Lognormal)
wc_u_chosen  <- 16373.2
wc_mu_body   <- 7.491752
wc_sd_body   <- 0.934789

# Tail (GPD)
wc_sigma_gpd <- 44348.65   # GPD scale
wc_xi_gpd    <- -0.1617661 # GPD shape

# Body probability: P(X <= u_chosen). Often set = Lognormal CDF at u_chosen.
# 若你有单独拟合的 phi_u，可在这里直接赋值替换下面这行  这是个啥？
phi_u <- plnorm(wc_u_chosen, wc_mu_body, wc_sd_body)

# ---------------------------------------------------------
# 0I. sev_wcerity multipliers by WC bucket
# 用 old sev_wcerity data 算 bucket-level relative sev_wcerity；
# spliced 下用作「金额倍数」: 该 bucket 损失 = 基准 spliced 抽样 × sev_wc_mult
# ---------------------------------------------------------

sev_wc_data <- sev_wc %>%
  filter(claim_amount > 0) %>%
  left_join(old_occ_map, by = "occupation")

unmapped_occ <- sev_wc_data %>%
  filter(is.na(occ_bucket)) %>%
  distinct(occupation)
print(unmapped_occ)

bucket_sev_wc <- sev_wc_data %>%
  group_by(occ_bucket) %>%
  summarise(
    median_sev_wcerity = median(claim_amount, na.rm = TRUE),
    n_claims = n(),
    .groups = "drop"
  )

ref_sev_wc <- bucket_sev_wc %>%
  filter(occ_bucket == "Technical") %>%
  pull(median_sev_wcerity)

wc_sev_wc_mult <- bucket_sev_wc %>%
  mutate(sev_wc_mult = median_sev_wcerity / ref_sev_wc) %>%
  select(occ_bucket, sev_wc_mult)

print(bucket_sev_wc)
print(wc_sev_wc_mult)

# A tibble: 4 × 3
#occ_bucket       median_sev_wcerity n_claims
#<chr>                      <dbl>    <int>
#1 Field Operations           1616.      458
#2 Heavy Operations           1723.      633
#3 Office/Admin               2394.      295
#4 Technical                  2561.      474
#> print(wc_sev_wc_mult)
# A tibble: 4 × 2
# occ_bucket       sev_wc_mult
# <chr>               <dbl>
# 1 Field Operations    0.631
# 2 Heavy Operations    0.673
# 3 Office/Admin        0.935
# 4 Technical           1   

# =========================================================
# Step 0H'. Mean exposure time by employment type (from historical data)
# 用 historical data 的 exposure (0–1) 与 employment_type 计算：
# Full-time 与 Contract 的平均 exposure，供 new data 折算有效暴露
# =========================================================

# 确保 historical 数据中有 employment_type 与 exposure 列（列名若不同请修改）
stopifnot("employment_type" %in% names(freq_wc), "exposure" %in% names(freq_wc))

freq_wc_emp <- freq_wc %>%
  dplyr::filter(!is.na(exposure), exposure > 0, exposure <= 1)

avg_exposure_by_emp <- freq_wc_emp %>%
  dplyr::group_by(employment_type = as.character(trimws(employment_type))) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mean_exposure = mean(exposure, na.rm = TRUE),
    .groups = "drop"
  )

# 取 Full-time 与 Contract 的均值（列名/取值若不同请按实际修改）
avg_exposure_fulltime <- avg_exposure_by_emp %>%
  dplyr::filter(tolower(employment_type) %in% c("full-time", "fulltime")) %>%
  dplyr::pull(mean_exposure)
avg_exposure_fulltime <- if (length(avg_exposure_fulltime) == 0) 1 else avg_exposure_fulltime[1]

avg_exposure_contract <- avg_exposure_by_emp %>%
  dplyr::filter(tolower(employment_type) %in% c("contract")) %>%
  dplyr::pull(mean_exposure)
avg_exposure_contract <- if (length(avg_exposure_contract) == 0) 0.5 else avg_exposure_contract[1]

print(avg_exposure_by_emp)
message("Used: avg_exposure_fulltime = ", round(avg_exposure_fulltime, 4),
        ", avg_exposure_contract = ", round(avg_exposure_contract, 4))

# =========================================================
# Step 1. Reconstruct WC exposure from personnel table
# 数学意思：
# exposure = 人头；exposure_eff = 有效暴露（与旧数据单位一致）
# exposure_eff = FullTime * avg(Full-time) + Contract * avg(Contract)，来自 historical 均值
# =========================================================

wc_exposure <- personnel_tbl %>%
  dplyr::left_join(role_map_wc, by = "Role") %>%
  dplyr::group_by(occ_bucket) %>%
  dplyr::summarise(
    exposure     = sum(Employees, na.rm = TRUE),
    full_time    = sum(FullTime, na.rm = TRUE),
    contract     = sum(Contract, na.rm = TRUE),
    exposure_eff = sum(FullTime, na.rm = TRUE) * avg_exposure_fulltime + sum(Contract, na.rm = TRUE) * avg_exposure_contract,
    contract_mix = sum(Contract, na.rm = TRUE) / sum(Employees, na.rm = TRUE),
    weighted_avg_age = sum(Employees * AvgAge, na.rm = TRUE) / sum(Employees, na.rm = TRUE),
    weighted_avg_salary = sum(Employees * AvgSalary, na.rm = TRUE) / sum(Employees, na.rm = TRUE),
    .groups = "drop"
  )

print(wc_exposure)

# 检查有没有 role 没被映射
unmapped_roles <- personnel_tbl %>%
  dplyr::left_join(role_map_wc, by = "Role") %>%
  dplyr::filter(is.na(occ_bucket))

if (nrow(unmapped_roles) > 0) {
  print(unmapped_roles)
  stop("Some roles are not mapped into WC occupation buckets.")
}

# =========================================================
# Step 2. Build system operational scale proxy from equipment data
#
# 数学意思：
# W_s = sum_e count_{s,e} * op_ratio_{s,e}
#
# 因为没有 workforce by solar system，
# 用 active equipment units 作为 WC 在 system 维度的 proxy
# =========================================================

# ---- 2A. Convert equipment counts into long format
equip_count_long <- equip_count_wide %>%
  tidyr::pivot_longer(
    cols = -EquipmentType,
    names_to = "system_code",
    values_to = "Count"
  ) %>%
  dplyr::mutate(
    SolarSystem = dplyr::case_when(
      system_code == "Helionis_Cluster" ~ "Helionis Cluster",
      system_code == "Bayesia_System" ~ "Bayesia System",
      system_code == "Oryn_Delta" ~ "Oryn Delta"
    )
  ) %>%
  dplyr::select(EquipmentType, SolarSystem, Count)

# ---- 2B. Convert equipment operation ratios into long format
equip_usage_long <- equip_usage_wide %>%
  dplyr::select(EquipmentType, Helionis_op, Bayesia_op, Oryn_op) %>%
  tidyr::pivot_longer(
    cols = -EquipmentType,
    names_to = "op_code",
    values_to = "OpRatio"
  ) %>%
  dplyr::mutate(
    SolarSystem = dplyr::case_when(
      op_code == "Helionis_op" ~ "Helionis Cluster",
      op_code == "Bayesia_op" ~ "Bayesia System",
      op_code == "Oryn_op" ~ "Oryn Delta"
    )
  ) %>%
  dplyr::select(EquipmentType, SolarSystem, OpRatio)

# ---- 2C. Combine equipment counts and operation ratios
equip_proxy <- equip_count_long %>%
  dplyr::left_join(equip_usage_long, by = c("EquipmentType", "SolarSystem")) %>%
  dplyr::mutate(
    active_units = Count * OpRatio
  )

print(equip_proxy)

# ---- 2D. Aggregate operational scale by solar system
wc_system_scale <- equip_proxy %>%
  dplyr::group_by(SolarSystem) %>%
  dplyr::summarise(
    W_s = sum(active_units, na.rm = TRUE),
    .groups = "drop"
  )

print(wc_system_scale)

# =========================================================
# Step 3. Convert system scale into system weights
#
# 数学意思：
# w_s = W_s / sum_s W_s
# F_solar_WC = sum_s w_s * F_s
# =========================================================

wc_system_weight <- wc_system_scale %>%
  dplyr::mutate(
    w_s = W_s / sum(W_s)
  ) %>%
  dplyr::left_join(wc_system_factor, by = "SolarSystem")

print(wc_system_weight)

F_solar_WC <- wc_system_weight %>%
  dplyr::summarise(
    F_solar_WC = sum(w_s * F_s, na.rm = TRUE)
  ) %>%
  dplyr::pull(F_solar_WC)

print(F_solar_WC)

# =========================================================
# Step 4. Transfer old WC freq_wcuency to new systems
#
# 数学意思：
# lambda_o_base = lambda_wc_base_old * relativity_o
# lambda_o_new  = lambda_o_base * F_solar_WC
# mu_o          = E_o * lambda_o_new
# =========================================================

wc_transfer <- wc_exposure %>%
  dplyr::left_join(wc_relativity, by = "occ_bucket") %>%
  dplyr::mutate(
    relativity  = dplyr::coalesce(relativity, 1),
    lambda_base = lambda_wc_base_old * relativity,
    F_solar_WC  = F_solar_WC,
    lambda_new  = lambda_base * F_solar_WC,
    mu          = exposure_eff * lambda_new   # 用有效暴露，与旧模型「每单位暴露的索赔率」一致
  )

print(wc_transfer)

# =========================================================
# Step 5. Attach sev_wcerity multiplier to WC strata (spliced: apply as amount multiplier)
# =========================================================

wc_transfer <- wc_transfer %>%
  dplyr::left_join(wc_sev_wc_mult, by = "occ_bucket") %>%
  dplyr::mutate(
    sev_wc_mult = ifelse(is.na(sev_wc_mult), 1, sev_wc_mult)
    # 不再有 sev_wc_scale_new；spliced 用 sev_wc_mult 乘在抽样金额上
  )

print(wc_transfer)

# =========================================================
# Step 6. Spliced sev_wcerity: CDF and random generator
# Body: Lognormal(mu_body, sd_body) up to u_chosen (mass phi_u)
# Tail: GPD(loc=0, scale=sigma, shape=xi) for excess above u_chosen
# =========================================================

# 若未安装 evd： install.packages("evd")
library(evd)

# ----- CDF (for reference / checks)
F_spliced <- function(q, u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  ifelse(
    q <= u_chosen,
    phi_u * plnorm(q, mu_body, sd_body) / F_body_at_u,
    phi_u + (1 - phi_u) * pgpd(q - u_chosen, loc = 0, scale = sigma_gpd, shape = xi_gpd)
  )
}

# ----- Quantile (inverse CDF) for sampling
# u in (0,1): if u <= phi_u then from body (rescaled LN), else from tail (GPD excess then + u_chosen)
q_spliced <- function(u, u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd) {
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  ifelse(
    u <= phi_u,
    qlnorm(u / phi_u * F_body_at_u, mu_body, sd_body),
    u_chosen + qgpd((u - phi_u) / (1 - phi_u), loc = 0, scale = sigma_gpd, shape = xi_gpd)
  )
}

# ----- Random draw from baseline spliced (single sev_wcerity). Bucket adjustment via sev_wc_mult applied in Step 7.
r_spliced_one <- function(u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd) {
  u <- runif(1)
  as.numeric(q_spliced(u, u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd))
}

# ----- Vectorized: n draws from baseline spliced
r_wc_sev_wcerity_spliced <- function(n, u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd) {
  u <- runif(n)
  u <- pmin(pmax(u, .Machine$double.eps), 1 - .Machine$double.eps)
  F_body_at_u <- plnorm(u_chosen, mu_body, sd_body)
  body_idx <- u <= phi_u
  x <- numeric(n)
  x[body_idx]  <- qlnorm(u[body_idx] / phi_u * F_body_at_u, mu_body, sd_body)
  x[!body_idx] <- u_chosen + qgpd((u[!body_idx] - phi_u) / (1 - phi_u), loc = 0, scale = sigma_gpd, shape = xi_gpd)
  x
}

# =========================================================
# Step 7. One-year Monte Carlo simulation for annual aggregate WC loss
#
# 数学意思：
# P_o ~ Poi(mu_o)
# X_ok ~ WC sev_wcerity distribution(shape1, shape2, scale_o)
# S_o = sum_{k=1}^{N_o} X_ok
# S_WC = sum_o S_o
# =========================================================

simulate_wc_one_year_split <- function(par_tbl, system_tbl,
                                       u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd) {
  
  bucket_names  <- par_tbl$occ_bucket
  system_names  <- system_tbl$SolarSystem
  
  bucket_losses <- setNames(rep(0, length(bucket_names)), bucket_names)
  system_losses <- setNames(rep(0, length(system_names)), system_names)
  total_loss    <- 0
  bucket_system_rows <- list()
  
  for (i in 1:nrow(par_tbl)) {
    for (j in 1:nrow(system_tbl)) {
      mu_o   <- par_tbl$mu[i]
      w_s    <- system_tbl$w_s[j]
      mu_so  <- mu_o * w_s
      n_so   <- rpois(1, lambda = mu_so)
      
      if (n_so == 0) {
        loss_so <- 0
      } else {
        sev_wc_so <- r_wc_sev_wcerity_spliced(n_so, u_chosen, mu_body, sd_body, phi_u, sigma_gpd, xi_gpd)
        loss_so <- sum(sev_wc_so) * par_tbl$sev_wc_mult[i]
      }
      
      bucket_losses[i] <- bucket_losses[i] + loss_so
      system_losses[j] <- system_losses[j] + loss_so
      total_loss       <- total_loss + loss_so
      bucket_system_rows[[length(bucket_system_rows) + 1L]] <- tibble::tibble(
        occ_bucket = bucket_names[i],
        SolarSystem = system_names[j],
        loss = loss_so
      )
    }
  }
  
  bucket_system_losses <- dplyr::bind_rows(bucket_system_rows)
  
  list(
    bucket_losses = bucket_losses,
    system_losses = system_losses,
    total_loss = total_loss,
    bucket_system_losses = bucket_system_losses
  )
}
# =========================================================
# Step 8. Run Monte Carlo simulations
# =========================================================

set.seed(123)

n_sim <- 10000

wc_sim_results <- replicate(
  n = n_sim,
  expr = simulate_wc_one_year_split(
    par_tbl    = wc_transfer,
    system_tbl = wc_system_weight,
    u_chosen   = wc_u_chosen,
    mu_body    = wc_mu_body,
    sd_body    = wc_sd_body,
    phi_u      = phi_u,
    sigma_gpd  = wc_sigma_gpd,
    xi_gpd     = wc_xi_gpd
  ),
  simplify = FALSE
)

#提取 total aggregate loss
S_wc <- sapply(wc_sim_results, function(x) x$total_loss)
#提取按 occupation bucket 的 aggregate loss
bucket_loss_mat <- do.call(
  rbind,
  lapply(wc_sim_results, function(x) x$bucket_losses)
)

bucket_loss_mat <- as.data.frame(bucket_loss_mat)
print(head(bucket_loss_mat))
#提取按 solar system 的 aggregate loss
system_loss_mat <- do.call(
  rbind,
  lapply(wc_sim_results, function(x) x$system_losses)
)

system_loss_mat <- as.data.frame(system_loss_mat)
print(head(system_loss_mat))

# 提取按 occ_bucket × SolarSystem 的 aggregate loss（每 sim 每个 cell 一行）
agg_by_bucket_system <- dplyr::bind_rows(
  lapply(seq_along(wc_sim_results), function(s) {
    wc_sim_results[[s]]$bucket_system_losses %>% dplyr::mutate(sim = s)
  })
)

#分别做summary
#1) portfolio total loss summary
calc_tvar <- function(x, p = 0.95) {
  var_p <- as.numeric(stats::quantile(x, p, na.rm = TRUE))
  mean(x[x >= var_p], na.rm = TRUE)
}

wc_summary_total <- tibble::tibble(
  metric = c("Mean", "SD", "VaR_95", "TVaR_95", "VaR_99", "TVaR_99", "Max"),
  value = c(
    mean(S_wc, na.rm = TRUE),
    stats::sd(S_wc, na.rm = TRUE),
    as.numeric(stats::quantile(S_wc, 0.95, na.rm = TRUE)),
    calc_tvar(S_wc, 0.95),
    as.numeric(stats::quantile(S_wc, 0.99, na.rm = TRUE)),
    calc_tvar(S_wc, 0.99),
    max(S_wc, na.rm = TRUE)
  )
)

print(wc_summary_total)

# 2) occupation bucket summary
wc_summary_bucket <- tibble::tibble(
  occ_bucket = names(bucket_loss_mat),
  Mean = sapply(bucket_loss_mat, mean, na.rm = TRUE),
  SD = sapply(bucket_loss_mat, stats::sd, na.rm = TRUE),
  VaR_95 = sapply(bucket_loss_mat, function(x) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE))),
  TVaR_95 = sapply(bucket_loss_mat, function(x) calc_tvar(x, 0.95)),
  VaR_99 = sapply(bucket_loss_mat, function(x) as.numeric(stats::quantile(x, 0.99, na.rm = TRUE))),
  TVaR_99 = sapply(bucket_loss_mat, function(x) calc_tvar(x, 0.99)),
  Max = sapply(bucket_loss_mat, max, na.rm = TRUE)
)

print(wc_summary_bucket)

# 3) solar system summary
wc_summary_system <- tibble::tibble(
  SolarSystem = names(system_loss_mat),
  Mean = sapply(system_loss_mat, mean, na.rm = TRUE),
  SD = sapply(system_loss_mat, stats::sd, na.rm = TRUE),
  VaR_95 = sapply(system_loss_mat, function(x) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE))),
  TVaR_95 = sapply(system_loss_mat, function(x) calc_tvar(x, 0.95)),
  VaR_99 = sapply(system_loss_mat, function(x) as.numeric(stats::quantile(x, 0.99, na.rm = TRUE))),
  TVaR_99 = sapply(system_loss_mat, function(x) calc_tvar(x, 0.99)),
  Max = sapply(system_loss_mat, max, na.rm = TRUE)
)

print(wc_summary_system)
# =========================================================
# Step 9. Summary outputs
# =========================================================

calc_tvar <- function(x, p = 0.95) {
  var_p <- as.numeric(stats::quantile(x, p, na.rm = TRUE))
  mean(x[x >= var_p], na.rm = TRUE)
}

# ---- 9A. Total WC portfolio summary
wc_summary_total <- tibble::tibble(
  metric = c("Mean", "SD", "VaR_95", "TVaR_95", "VaR_99", "TVaR_99", "Max"),
  value = c(
    mean(S_wc, na.rm = TRUE),
    stats::sd(S_wc, na.rm = TRUE),
    as.numeric(stats::quantile(S_wc, 0.95, na.rm = TRUE)),
    calc_tvar(S_wc, 0.95),
    as.numeric(stats::quantile(S_wc, 0.99, na.rm = TRUE)),
    calc_tvar(S_wc, 0.99),
    max(S_wc, na.rm = TRUE)
  )
)

print(wc_summary_total)

# ---- 9B. Summary by occupation bucket
wc_summary_bucket <- tibble::tibble(
  occ_bucket = names(bucket_loss_mat),
  Mean = sapply(bucket_loss_mat, mean, na.rm = TRUE),
  SD = sapply(bucket_loss_mat, stats::sd, na.rm = TRUE),
  VaR_95 = sapply(bucket_loss_mat, function(x) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE))),
  TVaR_95 = sapply(bucket_loss_mat, function(x) calc_tvar(x, 0.95)),
  VaR_99 = sapply(bucket_loss_mat, function(x) as.numeric(stats::quantile(x, 0.99, na.rm = TRUE))),
  TVaR_99 = sapply(bucket_loss_mat, function(x) calc_tvar(x, 0.99)),
  Max = sapply(bucket_loss_mat, max, na.rm = TRUE)
) %>%
  dplyr::mutate(
    MeanShare = Mean / sum(Mean, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(Mean))

print(wc_summary_bucket)

# ---- 9C. Summary by solar system
wc_summary_system <- tibble::tibble(
  SolarSystem = names(system_loss_mat),
  Mean = sapply(system_loss_mat, mean, na.rm = TRUE),
  SD = sapply(system_loss_mat, stats::sd, na.rm = TRUE),
  VaR_95 = sapply(system_loss_mat, function(x) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE))),
  TVaR_95 = sapply(system_loss_mat, function(x) calc_tvar(x, 0.95)),
  VaR_99 = sapply(system_loss_mat, function(x) as.numeric(stats::quantile(x, 0.99, na.rm = TRUE))),
  TVaR_99 = sapply(system_loss_mat, function(x) calc_tvar(x, 0.99)),
  Max = sapply(system_loss_mat, max, na.rm = TRUE)
) %>%
  dplyr::mutate(
    MeanShare = Mean / sum(Mean, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(Mean))

print(wc_summary_system)

# ---- 9D. Summary by occ_bucket × SolarSystem (strata)
wc_summary_bucket_system <- agg_by_bucket_system %>%
  dplyr::group_by(occ_bucket, SolarSystem) %>%
  dplyr::summarise(
    Mean = mean(loss, na.rm = TRUE),
    SD = stats::sd(loss, na.rm = TRUE),
    VaR_95 = as.numeric(stats::quantile(loss, 0.95, na.rm = TRUE)),
    TVaR_95 = calc_tvar(loss, 0.95),
    VaR_99 = as.numeric(stats::quantile(loss, 0.99, na.rm = TRUE)),
    TVaR_99 = calc_tvar(loss, 0.99),
    Max = max(loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(MeanShare = Mean / sum(Mean, na.rm = TRUE)) %>%
  dplyr::arrange(SolarSystem, dplyr::desc(Mean))

print(wc_summary_bucket_system)

# =========================================================
# Step 10. Tables most useful for report writing
# =========================================================

# ---- 10A. WC strata exposure and transferred parameters
wc_exposure_table <- wc_transfer %>%
  dplyr::select(
    occ_bucket,
    exposure,
    exposure_eff,
    full_time,
    contract,
    contract_mix,
    weighted_avg_age,
    weighted_avg_salary,
    relativity,
    lambda_base,
    F_solar_WC,
    lambda_new,
    mu,
    sev_wc_mult
  ) %>%
  dplyr::mutate(
    exposure_share     = exposure / sum(exposure, na.rm = TRUE),
    exposure_eff_share = exposure_eff / sum(exposure_eff, na.rm = TRUE),
    expected_claim_share = mu / sum(mu, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(exposure))

print(wc_exposure_table)

# ---- 10B. Solar system proxy table
wc_proxy_table <- wc_system_weight %>%
  dplyr::select(SolarSystem, W_s, w_s, F_s) %>%
  dplyr::mutate(
    proxy_share = W_s / sum(W_s, na.rm = TRUE)
  ) %>%
  dplyr::arrange(dplyr::desc(W_s))

print(wc_proxy_table)

# ---- 10C. Equipment-level proxy detail
equip_proxy_table <- equip_proxy %>%
  dplyr::select(EquipmentType, SolarSystem, Count, OpRatio, active_units) %>%
  dplyr::arrange(SolarSystem, dplyr::desc(active_units))

print(equip_proxy_table)

# ---- 10D. Optional: simulation-level total loss table
wc_total_loss_table <- tibble::tibble(
  sim_id = 1:length(S_wc),
  total_wc_loss = S_wc
)

print(head(wc_total_loss_table))

# ---- 10E. Optional: simulation-by-bucket table
wc_bucket_loss_table <- bucket_loss_mat %>%
  dplyr::mutate(sim_id = 1:n()) %>%
  dplyr::select(sim_id, dplyr::everything())

print(head(wc_bucket_loss_table))

# ---- 10F. Optional: simulation-by-system table
wc_system_loss_table <- system_loss_mat %>%
  dplyr::mutate(sim_id = 1:n()) %>%
  dplyr::select(sim_id, dplyr::everything())

print(head(wc_system_loss_table))

# =========================================================
# Step 11. Plots: simulated losses (x = losses, y = freq_wcuency)
# =========================================================

library(ggplot2)
library(tidyr)

# ---- 11A. Total portfolio loss
df_total <- data.frame(loss = S_wc)
p_total <- ggplot(df_total, aes(x = loss)) +
  geom_histogram(bins = 80, fill = "steelblue", color = "white", alpha = 0.8) +
  labs(title = "Simulated annual WC loss (Total portfolio)", x = "Loss", y = "freq_wcuency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_total)

# ---- 11B'. By occupation bucket: 四条曲线在同一坐标轴
library(ggplot2)
library(tidyr)

bucket_long <- bucket_loss_mat %>%
  tibble::rownames_to_column("sim_id") %>%
  tidyr::pivot_longer(cols = -sim_id, names_to = "occ_bucket", values_to = "loss")

p_bucket_overlay <- ggplot(bucket_long, aes(x = loss, color = occ_bucket, fill = occ_bucket)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  labs(
    title = "Simulated annual WC loss by occupation bucket",
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_bucket_overlay)

# ---- 11C'. By solar system: 三条曲线在同一坐标轴
library(ggplot2)
library(tidyr)

system_long <- system_loss_mat %>%
  tibble::rownames_to_column("sim_id") %>%
  tidyr::pivot_longer(cols = -sim_id, names_to = "SolarSystem", values_to = "loss")

p_system_overlay <- ggplot(system_long, aes(x = loss, color = SolarSystem, fill = SolarSystem)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  labs(
    title = "Simulated annual WC loss by solar system",
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_system_overlay)

# ---- 11D. By occ_bucket × SolarSystem (strata facet)
bucket_system_long <- agg_by_bucket_system %>%
  dplyr::mutate(
    occ_bucket = as.character(occ_bucket),
    SolarSystem = as.character(SolarSystem)
  )

p_bucket_system_overlay <- ggplot(
  bucket_system_long,
  aes(x = loss, color = occ_bucket, fill = occ_bucket)
) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  facet_wrap(~ SolarSystem, scales = "free_y", ncol = 2) +
  labs(
    title = "Simulated annual WC loss by occupation bucket × solar system",
    x = "Loss",
    y = "Density",
    colour = "Occupation bucket",
    fill = "Occupation bucket"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

print(p_bucket_system_overlay)

# =========================================================
# Step 12. Export results for report (tables + figures)
# =========================================================

out_dir <- "C:/Users/laptop/Desktop/UNSW study/ACTL5100/wc_report_output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 数值四舍五入，便于报告
round_df <- function(df, digits = 2) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

# ---- 12A. 表格 → CSV（可直接插入 Word/Excel 或上传）
write.csv(round_df(wc_summary_total, 2),      file.path(out_dir, "wc_summary_total.csv"),      row.names = FALSE)
write.csv(round_df(wc_summary_bucket, 2),    file.path(out_dir, "wc_summary_bucket.csv"),    row.names = FALSE)
write.csv(round_df(wc_summary_system, 2),    file.path(out_dir, "wc_summary_system.csv"),    row.names = FALSE)
write.csv(round_df(wc_summary_bucket_system, 2), file.path(out_dir, "wc_summary_bucket_system.csv"), row.names = FALSE)
write.csv(round_df(wc_exposure_table, 4),   file.path(out_dir, "wc_exposure_table.csv"),   row.names = FALSE)
write.csv(round_df(wc_proxy_table, 4),       file.path(out_dir, "wc_proxy_table.csv"),       row.names = FALSE)
write.csv(round_df(equip_proxy_table, 2),    file.path(out_dir, "equip_proxy_table.csv"),    row.names = FALSE)

# ---- 12B. Simulation outputs → CSV (per-sim total, by bucket, by system)
wc_agg_total_df <- tibble::tibble(sim = seq_len(length(S_wc)), total_loss = S_wc)
write.csv(round_df(wc_agg_total_df, 2), file.path(out_dir, "wc_agg_total_by_sim.csv"), row.names = FALSE)

wc_agg_bucket_df <- dplyr::bind_cols(tibble::tibble(sim = seq_len(nrow(bucket_loss_mat))), bucket_loss_mat)
write.csv(round_df(wc_agg_bucket_df, 2), file.path(out_dir, "wc_agg_by_bucket_by_sim.csv"), row.names = FALSE)

wc_agg_system_df <- dplyr::bind_cols(tibble::tibble(sim = seq_len(nrow(system_loss_mat))), system_loss_mat)
write.csv(round_df(wc_agg_system_df, 2), file.path(out_dir, "wc_agg_by_system_by_sim.csv"), row.names = FALSE)

write.csv(round_df(agg_by_bucket_system, 2), file.path(out_dir, "wc_agg_by_bucket_system_by_sim.csv"), row.names = FALSE)

# ---- 12C. 图片 → PNG（适合 report 上传）
fig_width <- 7
fig_height <- 4.5
dpi <- 300

ggsave(file.path(out_dir, "fig_wc_total_loss.png"),     p_total,         width = fig_width, height = fig_height, dpi = dpi, bg = "white")
ggsave(file.path(out_dir, "fig_wc_loss_by_bucket.png"), p_bucket_overlay, width = fig_width, height = fig_height, dpi = dpi, bg = "white")
ggsave(file.path(out_dir, "fig_wc_loss_by_system.png"), p_system_overlay, width = fig_width, height = fig_height, dpi = dpi, bg = "white")
ggsave(file.path(out_dir, "fig_wc_loss_by_bucket_system.png"), p_bucket_system_overlay, width = 9, height = 6, dpi = dpi, bg = "white")

message("Exported to: ", normalizePath(out_dir))
list.files(out_dir)