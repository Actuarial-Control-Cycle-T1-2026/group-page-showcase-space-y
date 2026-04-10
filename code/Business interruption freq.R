# -----------------------------------------------------------------------------
# 0. Install and load required packages
# -----------------------------------------------------------------------------
pkgs <- c(
  "readxl",          # Read xlsx
  "dplyr",           # Data manipulation
  "ggplot2",         # Visualisation
  "MASS",            # glm.nb (negative binomial)
  "pscl",            # zeroinfl (zero-inflated models)
  "fitdistrplus",    # Distribution fitting & comparison
  "actuar",          # Actuarial distributions (Burr, Pareto, etc.)
  "insurancerating", # Insurance actuarial tools
  "car",             # VIF (variance inflation factor)
  "lmtest",          # Likelihood ratio test
  "ggfortify",       # GLM diagnostic plots
  "patchwork",       # Plot composition
  "scales",          # Axis formatting
  "gridExtra",       # Plot arrangement
  "glmnet",          # LASSO / Ridge
  "zoo",
  "ismev",
  "evd",
  "tidyr"
)

# Auto-install missing packages
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

invisible(lapply(pkgs, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 1. Import dataset
# -----------------------------------------------------------------------------

freq_raw <- readRDS("D:/Dropbox/Dropbox/Private document/SOA case study/Dataset/bi_freq_cleaned.rds")
sev_raw  <- readRDS("D:/Dropbox/Dropbox/Private document/SOA case study/Dataset/bi_sev_cleaned.rds")


# -----------------------------------------------------------------------------
# Step 1: Data structure + validity checks (always first)
# -----------------------------------------------------------------------------

# 1) Variable types and overall overview
str(freq_raw)
summary(freq_raw$exposure)
summary(freq_raw$claim_count)

# 2) claim_count must be: non-negative integers
cat("claim_count < 0:", sum(freq_raw$claim_count < 0, na.rm = TRUE), "\n")
cat("claim_count non-integer:", sum(freq_raw$claim_count %% 1 != 0, na.rm = TRUE), "\n")

# 3) exposure must be: positive (offset uses log(exposure))
cat("exposure <= 0:", sum(freq_raw$exposure <= 0, na.rm = TRUE), "\n")
cat("exposure missing:", sum(is.na(freq_raw$exposure)), "\n")

# 4) Missing value summary (per column)
na_count <- sapply(freq_raw, function(x) sum(is.na(x)))
na_count[na_count > 0]

# 5) Uniqueness check: is each row a unique exposure unit?
cat("policy_id unique:", nrow(freq_raw) == n_distinct(freq_raw$policy_id), "\n")
cat("(policy_id, station_id) unique:",
    nrow(freq_raw) == n_distinct(paste(freq_raw$policy_id, freq_raw$station_id)), "\n")

# 6) If duplicates exist, inspect them
freq_raw %>%
  count(policy_id, station_id, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  head(20)


# -----------------------------------------------------------------------------
# Step 2: Remove rows with NA values
# -----------------------------------------------------------------------------

freq <- freq_raw %>% drop_na()

cat("Rows before:", nrow(freq_raw), "\n")
cat("Rows after:",  nrow(freq),     "\n")
cat("Rows removed:", nrow(freq_raw) - nrow(freq), "\n")


# -----------------------------------------------------------------------------
# Step 3: Exploratory frequency analysis
# -----------------------------------------------------------------------------

# 3.1 Overall claim frequency (claims per unit of exposure)
total_claims <- sum(freq$claim_count)
total_expo   <- sum(freq$exposure)
cat("Total claims:", total_claims, "\n")
cat("Total exposure:", total_expo, "\n")
cat("Overall claim rate per exposure:", total_claims / total_expo, "\n")
# Overall claim rate per exposure: 0.2013915

# 3.2 Zero proportion
cat("Zero proportion:", mean(freq$claim_count == 0), "\n")

# 3.3 Mean and variance of claim_count
cat("Mean count:", mean(freq$claim_count), "\n")
cat("Var  count:", var(freq$claim_count),  "\n")

# Crude overdispersion ratio (var/mean >> 1.2 → overdispersed → NB warranted)
var(freq$claim_count) / mean(freq$claim_count)
# 1.723667 >> 1.2  →  Negative Binomial warranted


# -----------------------------------------------------------------------------
# Step 4: Confirm overdispersion — Poisson baseline + formal LRT vs NB
# -----------------------------------------------------------------------------

# 4.0 Poisson baseline (diagnostic only — expected to be rejected)
m_pois0 <- glm(
  claim_count ~ offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq
)

phi0 <- sum(residuals(m_pois0, type = "pearson")^2) / df.residual(m_pois0)
cat("Pearson dispersion (Poisson null):", phi0, "\n")
# phi0 ≈ 1.97  >>  1.2  →  Poisson inadequate

# 4.1 Fit NB null model
m_nb0 <- glm.nb(
  claim_count ~ offset(log(exposure)),
  data = freq
)

summary(m_nb0)
cat("NB theta (null):", m_nb0$theta, "\n")
# Small theta (< 5) confirms meaningful overdispersion

# Formal LRT: Poisson vs NB
# H0: theta → ∞ (reduces to Poisson); reject if p < 0.05
lrt_pois_nb <- 2 * (logLik(m_nb0) - logLik(m_pois0))
cat("LRT statistic (Poisson vs NB):", as.numeric(lrt_pois_nb), "\n")
cat("p-value:", pchisq(as.numeric(lrt_pois_nb), df = 1, lower.tail = FALSE), "\n")
# p << 0.05  →  NB significantly better; proceed with NB throughout

exp(coef(m_nb0))   # Baseline claim rate under NB


# 4.2 Add solar_system — assess its contribution under NB
# 4.2.1 Group-level claim rates by solar_system
freq %>%
  group_by(solar_system) %>%
  summarise(
    n         = n(),
    expo      = sum(exposure),
    claims    = sum(claim_count),
    rate      = claims / expo,
    zero_prop = mean(claim_count == 0)
  ) %>%
  arrange(desc(rate))

# 4.2.2 NB GLM with solar_system
m_nb1 <- glm.nb(
  claim_count ~ solar_system + offset(log(exposure)),
  data = freq
)

summary(m_nb1)
cat("NB theta (solar_system):", m_nb1$theta, "\n")

# LRT: NB null vs NB + solar_system
lrtest(m_nb0, m_nb1)
# Significant p → solar_system improves fit


# 4.3 Domain-knowledge model — must-keep predictors
#
#   Must-keep parameters:
#     - solar_system        (environmental / regional operating risk)
#     - production_load     (operational intensity driving BI exposure)
#     - energy_backup_score (resilience factor — higher score, lower BI risk)

m_nb2 <- glm.nb(
  claim_count ~ solar_system + production_load + energy_backup_score +
    offset(log(exposure)),
  data = freq
)

summary(m_nb2)
cat("NB theta (domain model):", m_nb2$theta, "\n")

exp(coef(m_nb2))   # Multiplicative rate relativities

# LRT: NB null vs domain model
lrtest(m_nb0, m_nb2)


# =============================================================================
# 4.4 Data-driven variable selection (with train / test split)
# =============================================================================

# -----------------------------------------------------------------------------
# 4.4.0 Split data at policy_id level (avoids leakage)
# -----------------------------------------------------------------------------

set.seed(123)

pol_sum <- freq %>%
  group_by(policy_id) %>%
  summarise(
    solar_system = first(solar_system),
    exp_pol      = sum(exposure),
    clm_pol      = sum(claim_count),
    .groups      = "drop"
  )

# Stratify by solar_system; cut at 80% cumulative exposure
pol_split <- pol_sum %>%
  group_by(solar_system) %>%
  mutate(u = runif(n())) %>%
  arrange(u, .by_group = TRUE) %>%
  mutate(
    cum_exp  = cumsum(exp_pol),
    tot_exp  = sum(exp_pol),
    in_train = cum_exp <= 0.8 * tot_exp
  ) %>%
  ungroup()

train_ids <- pol_split %>% filter(in_train) %>% pull(policy_id)

train <- freq %>% filter( policy_id %in% train_ids)
test  <- freq %>% filter(!policy_id %in% train_ids)

cat("Rows train/test:", nrow(train), nrow(test), "\n")
cat("Claim rate train:", sum(train$claim_count) / sum(train$exposure), "\n")
cat("Claim rate test :", sum(test$claim_count)  / sum(test$exposure),  "\n")

# Align factor levels between train and test
fix_levels <- function(df, ref) {
  facs <- names(ref)[sapply(ref, is.factor)]
  for (nm in facs) df[[nm]] <- factor(df[[nm]], levels = levels(ref[[nm]]))
  df
}
train <- fix_levels(train, freq)
test  <- fix_levels(test,  freq)


# -----------------------------------------------------------------------------
# 4.4.1 LASSO + CV on TRAIN — using "negbin" family
# -----------------------------------------------------------------------------

mm_formula <- ~ solar_system +
  production_load +
  energy_backup_score +
  supply_chain_index +
  avg_crew_exp +
  maintenance_freq +
  safety_compliance

X_train   <- model.matrix(mm_formula, data = train)[, -1]
y_train   <- train$claim_count
off_train <- log(train$exposure)

# penalty.factor: 0 = unpenalised (must-keep), 1 = LASSO-penalised
penalty <- rep(1, ncol(X_train))
must_keep <- c("solar_system", "production_load", "energy_backup_score")
penalty[grep(paste(must_keep, collapse = "|"), colnames(X_train))] <- 0

table(penalty)
head(colnames(X_train)[penalty == 0], 20)

set.seed(123)
cvfit <- cv.glmnet(
  x              = X_train,
  y              = y_train,
  family         = "poisson", 
  offset         = off_train,
  alpha          = 1,
  penalty.factor = penalty,
  nfolds         = 10,
  type.measure   = "deviance"
)

plot(cvfit)
cat("lambda.1se:", cvfit$lambda.1se, "\n")
cat("lambda.min:", cvfit$lambda.min, "\n")

# Extract selected variables (non-zero coefficients)
b_1se <- coef(cvfit, s = "lambda.1se")
b_min <- coef(cvfit, s = "lambda.min")

sel_1se <- rownames(b_1se)[as.numeric(b_1se) != 0]
sel_min <- rownames(b_min)[as.numeric(b_min) != 0]

cat("Selected (lambda.1se):\n"); print(sel_1se); cat("Count:", length(sel_1se), "\n")
cat("Selected (lambda.min):\n"); print(sel_min); cat("Count:", length(sel_min), "\n")


# -----------------------------------------------------------------------------
# 4.4.2 Refit unpenalised NB GLM on TRAIN for interpretability
# -----------------------------------------------------------------------------

# Model A: must-keep parameters only
m_A_train <- glm.nb(
  claim_count ~ solar_system + production_load + energy_backup_score +
    offset(log(exposure)),
  data = train
)

# Model B: must-keep + LASSO-selected additional predictors
# (update extra variables based on sel_1se / sel_min results above)
m_B_train <- glm.nb(
  claim_count ~ solar_system + production_load + energy_backup_score +
    supply_chain_index + safety_compliance +
    offset(log(exposure)),
  data = train
)

AIC(m_A_train, m_B_train)
# ΔAIC > 2 favours the model with lower AIC

# Pearson dispersion for each NB model (should be ≈ 1.0 if NB fits well)
phiA <- sum(residuals(m_A_train, type = "pearson")^2) / df.residual(m_A_train)
phiB <- sum(residuals(m_B_train, type = "pearson")^2) / df.residual(m_B_train)
c(phiA = phiA, phiB = phiB)
# Both near 1.0 → NB has absorbed the overdispersion

cat("NB theta Model A:", m_A_train$theta, "\n")
cat("NB theta Model B:", m_B_train$theta, "\n")

summary(m_A_train)
summary(m_B_train)

exp(coef(m_A_train))   # Rate relativities — Model A
exp(coef(m_B_train))   # Rate relativities — Model B

# LRT between Model A and Model B (nested)
lrtest(m_A_train, m_B_train)


# -----------------------------------------------------------------------------
# 4.4.3 Evaluate on TEST (out-of-sample)
# -----------------------------------------------------------------------------

# Helper: Poisson deviance (consistent scoring metric across models)
pois_dev <- function(y, mu) {
  mu   <- pmax(mu, 1e-12)
  term <- ifelse(y == 0, 0, y * log(y / mu))
  2 * sum(term - (y - mu))
}

# Predicted expected claim counts on test set
test$pred_A_claims <- predict(m_A_train, newdata = test, type = "response")
test$pred_B_claims <- predict(m_B_train, newdata = test, type = "response")

# Overall calibration on test
actual_rate_test <- sum(test$claim_count)   / sum(test$exposure)
predA_rate_test  <- sum(test$pred_A_claims) / sum(test$exposure)
predB_rate_test  <- sum(test$pred_B_claims) / sum(test$exposure)

c(actual_rate_test = actual_rate_test,
  predA_rate_test  = predA_rate_test,
  predB_rate_test  = predB_rate_test)

# Deviance on test (lower is better)
devA_test <- pois_dev(test$claim_count, test$pred_A_claims)
devB_test <- pois_dev(test$claim_count, test$pred_B_claims)
c(devA_test = devA_test, devB_test = devB_test)

# Select final model based on test deviance
final_model <- if (devB_test < devA_test) m_B_train else m_A_train
final_name  <- if (devB_test < devA_test) "Model B (enriched)" else "Model A (parsimonious)"
cat("Selected final model:", final_name, "\n")

# Select model A
# solar_system + production_load + energy_backup_score


# -----------------------------------------------------------------------------
# 4.5 Calibration plot on TEST (Predicted vs Actual claim rate)
# -----------------------------------------------------------------------------

test$pred_claims <- predict(final_model, newdata = test, type = "response")
test$pred_rate   <- test$pred_claims / test$exposure
test$actual_rate <- test$claim_count / test$exposure

test <- test %>% mutate(risk_decile = ntile(pred_rate, 10))

calibration_test <- test %>%
  group_by(risk_decile) %>%
  summarise(
    exposure         = sum(exposure),
    actual_claims    = sum(claim_count),
    predicted_claims = sum(pred_claims)
  ) %>%
  mutate(
    actual_rate    = actual_claims    / exposure,
    predicted_rate = predicted_claims / exposure
  )


print(calibration_test)

ggplot(calibration_test, aes(x = predicted_rate, y = actual_rate)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  labs(
    title = paste0("TEST Calibration Plot: Predicted vs Actual Claim Rates\n(",
                   final_name, " — Negative Binomial)"),
    x = "Predicted Claim Rate",
    y = "Actual Claim Rate"
  ) +
  theme_minimal()


# =============================================================================
# Full pipeline summary
# =============================================================================
#
#  Data import
#  ↓
#  Validity checks (claim_count, exposure, NAs, uniqueness)
#  ↓
#  Exploratory analysis (zero proportion, var/mean = 1.72 >> 1.2)
#  ↓
#  Poisson baseline → Pearson dispersion phi ≈ 1.97 → Poisson rejected
#  ↓
#  Formal LRT (Poisson vs NB) → NB confirmed
#  ↓
#  NB domain-knowledge model (solar_system, production_load, energy_backup_score)
#  ↓
#  Train / test split (stratified by solar_system, 80/20 on exposure)
#  ↓
#  LASSO + CV with negbin family (must-keep parameters unpenalised)
#  ↓
#  Refit unpenalised NB GLMs (Model A vs Model B)
#  ↓
#  AIC + Pearson dispersion + LRT comparison
#  ↓
#  Out-of-sample test evaluation (calibration + deviance)
#  ↓
#  Final model selection + calibration plot
