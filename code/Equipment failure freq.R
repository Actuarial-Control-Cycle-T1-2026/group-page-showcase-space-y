# =============================================================================
# Equipment Failure - frequency Model
# Mirrors the Workers' Compensation frequency pipeline
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Install and load required packages
# -----------------------------------------------------------------------------
pkgs <- c(
  "readxl",        # read xlsx
  "dplyr",         # data manipulation
  "ggplot2",       # visualisation
  "MASS",          # glm.nb (negative binomial)
  "pscl",          # zeroinfl (zero-inflated models)
  "fitdistrplus",  # distribution fitting
  "actuar",        # actuarial distributions
  "car",           # VIF
  "lmtest",        # likelihood ratio test
  "patchwork",     # plot arrangement
  "scales",        # axis formatting
  "gridExtra",     # plot arrangement
  "glmnet",        # LASSO / elastic net
  "zoo"
)

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 1. Import cleaned dataset
# -----------------------------------------------------------------------------

freq_ef <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/ef_freq_cleaned.rds") 

# -----------------------------------------------------------------------------
# Step 1: Data structure & validity checks  (always do this first)
# -----------------------------------------------------------------------------

# 1) Variable types and overall summary
str(freq_ef)
summary(freq_ef$exposure)
summary(freq_ef$claim_count)

# 2) claim_count must be: non-negative integers
cat("claim_count < 0   :", sum(freq_ef$claim_count < 0,    na.rm = TRUE), "\n")
cat("claim_count non-int:", sum(freq_ef$claim_count %% 1 != 0, na.rm = TRUE), "\n")

# 3) exposure must be: strictly positive (used as log-offset)
cat("exposure <= 0 :", sum(freq_ef$exposure <= 0, na.rm = TRUE), "\n")
cat("exposure NA   :", sum(is.na(freq_ef$exposure)), "\n")

# 4) Missing value overview (per column)
na_count <- sapply(freq_ef, function(x) sum(is.na(x)))
na_count[na_count > 0]

# 5) Uniqueness check
cat("policy_id unique          :", nrow(freq_ef) == n_distinct(freq_ef$policy_id), "\n")
cat("(policy_id, equipment_id) :",
    nrow(freq_ef) == n_distinct(paste(freq_ef$policy_id, freq_ef$equipment_id)), "\n")

# 6) Show any duplicate key combinations
freq_ef %>%
  count(policy_id, equipment_id, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  head(20)


# -----------------------------------------------------------------------------
# Step 2: Assume no further cleaning needed — proceed with full dataset
# -----------------------------------------------------------------------------
freq_ef <- freq_ef


# -----------------------------------------------------------------------------
# Step 3: Exploratory descriptive statistics
# -----------------------------------------------------------------------------

# 3.1 Overall claim rate per unit of exposure
total_claims <- sum(freq_ef$claim_count)
total_expo   <- sum(freq_ef$exposure)
cat("Total claims        :", total_claims, "\n")
cat("Total exposure      :", total_expo, "\n")
cat("Overall claim rate  :", total_claims / total_expo, "\n")

# Overall claim rate  : 0.1549301 

# 3.2 Zero-claim proportion (typically very high in equipment data)
cat("Zero proportion:", mean(freq_ef$claim_count == 0), "\n")

#Zero proportion: 0.9267074 

# 3.3 Mean and variance of claim_count (crude overdispersion check)
cat("Mean count:", mean(freq_ef$claim_count), "\n")
cat("Var  count:", var(freq_ef$claim_count), "\n")

# Mean count: 0.07740598 
#Var  count: 0.08007252


# Variance-to-mean ratio — guides choice between Poisson and NB
vm_ratio <- var(freq_ef$claim_count) / mean(freq_ef$claim_count)
cat("Var/Mean ratio:", vm_ratio, "\n")
# If ratio ≈ 1 → Poisson adequate
# If ratio >> 1 → consider Negative Binomial

# Var/Mean ratio: 1.034449 

# 3.4 Claim rate by equipment_type (key domain variable)
freq_ef %>%
  group_by(equipment_type) %>%
  summarise(
    n          = n(),
    expo       = sum(exposure),
    claims     = sum(claim_count),
    rate       = claims / expo,
    zero_prop  = mean(claim_count == 0)
  ) %>%
  arrange(desc(rate))

# 3.5 Claim rate by solar_system
freq_ef %>%
  group_by(solar_system) %>%
  summarise(
    n          = n(),
    expo       = sum(exposure),
    claims     = sum(claim_count),
    rate       = claims / expo,
    zero_prop  = mean(claim_count == 0)
  ) %>%
  arrange(desc(rate))

# Epsilon has the least claim rate

# -----------------------------------------------------------------------------
# Step 4: Fit Poisson baseline — compute Pearson dispersion
# -----------------------------------------------------------------------------

# 4.1 Intercept-only Poisson (null model with offset)
m_pois0 <- glm(
  claim_count ~ offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq_ef
)

summary(m_pois0)

phi0 <- sum(residuals(m_pois0, type = "pearson")^2) / df.residual(m_pois0)
cat("Pearson dispersion phi0:", phi0, "\n")
# phi0 < 1.2 → Poisson is adequate; phi0 >> 1.2 → consider NB

# Possion is good

# Baseline claim rate
exp(coef(m_pois0))


# 4.2 Add equipment_type as a single-factor model
m_pois1 <- glm(
  claim_count ~ equipment_type + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq_ef
)

summary(m_pois1)

phi1 <- sum(residuals(m_pois1, type = "pearson")^2) / df.residual(m_pois1)
cat("Pearson dispersion phi1:", phi1, "\n")
# Pearson dispersion phi1: 1.018018 

# 4.3 Domain-knowledge model (must-keep predictors)
# Rationale:
#   - equipment_type : directly drives failure risk (analogous to occupation in WC)
#   - solar_system   : captures environmental/operational context
#   - equipment_age  : older equipment → higher failure rate
#
m_pois2 <- glm(
  claim_count ~ equipment_type + solar_system + equipment_age + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = freq_ef
)

summary(m_pois2)

# All significant!!!

phi2 <- sum(residuals(m_pois2, type = "pearson")^2) / df.residual(m_pois2)
cat("Pearson dispersion phi2:", phi2, "\n")
# Pearson dispersion phi2: 1.00432 

# =============================================================================
# 4.4 Data-driven variable selection (train / test split + LASSO)
# =============================================================================

# ------------------------------------------------------------
# 4.4.0 Split by policy_id to prevent data leakage
# ------------------------------------------------------------
set.seed(123)

# Aggregate at policy level
pol_sum <- freq_ef %>%
  group_by(policy_id) %>%
  summarise(
    equipment_type = first(equipment_type),
    solar_system   = first(solar_system),
    exp_pol        = sum(exposure),
    clm_pol        = sum(claim_count),
    .groups        = "drop"
  )

# Stratified 80/20 split within each equipment_type by cumulative exposure
pol_split <- pol_sum %>%
  group_by(equipment_type) %>%
  mutate(u = runif(n())) %>%
  arrange(u, .by_group = TRUE) %>%
  mutate(
    cum_exp  = cumsum(exp_pol),
    tot_exp  = sum(exp_pol),
    in_train = cum_exp <= 0.8 * tot_exp
  ) %>%
  ungroup()

train_ids <- pol_split %>% filter(in_train)  %>% pull(policy_id)

train <- freq_ef %>% filter( policy_id %in% train_ids)
test  <- freq_ef %>% filter(!policy_id %in% train_ids)

cat("Rows train / test   :", nrow(train), nrow(test), "\n")
cat("Claim rate train    :", sum(train$claim_count) / sum(train$exposure), "\n")
cat("Claim rate test     :", sum(test$claim_count)  / sum(test$exposure),  "\n")
# Claim rate train    : 0.1554254 
# Claim rate test     : 0.1529495

# Align factor levels between train and test to avoid 'new level' errors
fix_levels <- function(df, ref) {
  facs <- names(ref)[sapply(ref, is.factor)]
  for (nm in facs) {
    df[[nm]] <- factor(df[[nm]], levels = levels(ref[[nm]]))
  }
  df
}
train <- fix_levels(train, freq_ef)
test  <- fix_levels(test,  freq_ef)


# ------------------------------------------------------------
# 4.4.1 LASSO with CV on TRAIN only (Poisson with offset)
# ------------------------------------------------------------

# ── Fix types on both train AND test ─────────────────────────────────────────
train <- train %>%
  mutate(
    equipment_type  = as.factor(equipment_type),
    solar_system    = as.factor(solar_system),
    equipment_age   = as.numeric(equipment_age),
    maintenance_int = as.numeric(maintenance_int),
    usage_int       = as.numeric(usage_int)
  )

test <- test %>%
  mutate(
    equipment_type  = as.factor(equipment_type),
    solar_system    = as.factor(solar_system),
    equipment_age   = as.numeric(equipment_age),
    maintenance_int = as.numeric(maintenance_int),
    usage_int       = as.numeric(usage_int)
  )

mm_formula <- ~ equipment_type +
  solar_system   +
  equipment_age  +
  maintenance_int +
  usage_int

X_train   <- model.matrix(mm_formula, data = train)[, -1]
y_train   <- train$claim_count
off_train <- log(train$exposure)

# Penalty factor: must-keep predictors are unpenalised (penalty = 0)
penalty    <- rep(1, ncol(X_train))
must_keep  <- c("equipment_type", "solar_system", "equipment_age")
penalty[grep(paste(must_keep, collapse = "|"), colnames(X_train))] <- 0

cat("Penalised predictors  :", sum(penalty == 1), "\n")
cat("Unpenalised predictors:", sum(penalty == 0), "\n")

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

# Non-zero coefficients at each lambda
b_1se <- coef(cvfit, s = "lambda.1se")
b_min <- coef(cvfit, s = "lambda.min")

sel_1se <- rownames(b_1se)[as.numeric(b_1se) != 0]
sel_min <- rownames(b_min)[as.numeric(b_min) != 0]

cat("\nSelected (lambda.1se) — count:", length(sel_1se), "\n"); print(sel_1se)
cat("\nSelected (lambda.min) — count:", length(sel_min), "\n"); print(sel_min)


# ------------------------------------------------------------
# 4.4.2 Refit unpenalised GLMs on TRAIN for interpretability
# ------------------------------------------------------------

# Model A — domain-knowledge only (parsimonious)
m_A_train <- glm(
  claim_count ~ equipment_type + solar_system + equipment_age +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = train
)

# Model B — enriched with LASSO-selected continuous predictors
m_B_train <- glm(
  claim_count ~ equipment_type + solar_system + equipment_age +
    maintenance_int + usage_int +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = train
)

AIC(m_A_train, m_B_train)
# If AIC_A - AIC_B > 2, prefer Model B despite the extra parameters

# m_A_train  9 37507.08
# m_B_train 11 36980.94

phiA <- sum(residuals(m_A_train, type = "pearson")^2) / df.residual(m_A_train)
phiB <- sum(residuals(m_B_train, type = "pearson")^2) / df.residual(m_B_train)
cat("Pearson dispersion — Model A:", phiA, "  Model B:", phiB, "\n")

# Pearson dispersion — Model A: 1.008579   Model B: 1.004469 

summary(m_A_train)
summary(m_B_train)

# Exponentiated coefficients (multiplicative rate ratios)
exp(coef(m_A_train))
exp(coef(m_B_train))

# Correlation among continuous predictors (check multicollinearity on TRAIN)
cor(train[, c("equipment_age", "maintenance_int", "usage_int")])


# ------------------------------------------------------------
# 4.4.3 Evaluate on TEST (out-of-sample)
# ------------------------------------------------------------

# Poisson deviance helper
pois_dev <- function(y, mu) {
  mu   <- pmax(mu, 1e-12)
  term <- ifelse(y == 0, 0, y * log(y / mu))
  2 * sum(term - (y - mu))
}

test$pred_A_claims <- predict(m_A_train, newdata = test, type = "response")
test$pred_B_claims <- predict(m_B_train, newdata = test, type = "response")

# Overall calibration on TEST
actual_rate_test <- sum(test$claim_count)    / sum(test$exposure)
predA_rate_test  <- sum(test$pred_A_claims)  / sum(test$exposure)
predB_rate_test  <- sum(test$pred_B_claims)  / sum(test$exposure)

cat("\nCalibration on TEST:\n")
cat("  Actual rate :", actual_rate_test, "\n")
cat("  Model A rate:", predA_rate_test,  "\n")
cat("  Model B rate:", predB_rate_test,  "\n")

# Poisson deviance on TEST (lower = better)
devA_test <- pois_dev(test$claim_count, test$pred_A_claims)
devB_test <- pois_dev(test$claim_count, test$pred_B_claims)
cat("\nTest deviance — Model A:", devA_test, "  Model B:", devB_test, "\n")

# Test deviance — Model A: 6574.542   Model B: 6481.392 

# Select final model
final_model_ef <- if (devB_test < devA_test) m_B_train else m_A_train
final_name  <- if (devB_test < devA_test) "Model B (enriched)" else "Model A (parsimonious)"
cat("Selected final model:", final_name, "\n")

# Selected final model: Model B (enriched) 

# =============================================================================
# 4.5 Calibration plot on TEST — Predicted vs Actual claim rate
# =============================================================================

test$pred_claims <- predict(final_model_ef, newdata = test, type = "response")
test$pred_rate   <- test$pred_claims / test$exposure
test$actual_rate <- test$claim_count / test$exposure

test <- test %>% mutate(risk_decile = ntile(pred_rate, 10))

calibration_test <- test %>%
  group_by(risk_decile) %>%
  summarise(
    exposure          = sum(exposure),
    actual_claims     = sum(claim_count),
    predicted_claims  = sum(pred_claims)
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
                   final_name, ")"),
    x = "Predicted Claim Rate",
    y = "Actual Claim Rate"
  ) +
  theme_minimal()

# Fits so well

# Poisson with predictors to be 
# equipment_type + solar_system + equipment_age + maintenance_int + usage_int

# =============================================================================
# Summary pipeline
# =============================================================================
#
#  data cleaning (pre-done → ef_freq_cleaned.rds)
#  ↓
#  Poisson baseline + Pearson dispersion check
#  ↓
#  domain-knowledge model (equipment_type + solar_system + equipment_age)
#  ↓
#  LASSO + 10-fold CV variable selection (penalise maintenance_int, usage_int)
#  ↓
#  refit unpenalised GLMs (Model A parsimonious vs Model B enriched)
#  ↓
#  AIC + dispersion comparison on TRAIN
#  ↓
#  out-of-sample deviance evaluation on TEST
#  ↓
#  final model selected + calibration plot
