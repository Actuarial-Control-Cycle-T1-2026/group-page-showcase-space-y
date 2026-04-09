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

freq_wc_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_freq_cleaned.rds")
sev_wc_raw <- readRDS("C:/Users/laptop/Desktop/UNSW study/ACTL5100/cleaned data/wc_sev_cleaned.rds")

#Step 1：先做“数据结构 + 合法性”检查（必须先做）

# 1) 看变量类型与整体概况
str(freq_wc_raw)
summary(freq_wc_raw$exposure)
summary(freq_wc_raw$claim_count)

# 2) claim_count 必须是：非负整数
cat("claim_count < 0:", sum(freq_wc_raw$claim_count < 0, na.rm=TRUE), "\n")
cat("claim_count 非整数:", sum(freq_wc_raw$claim_count %% 1 != 0, na.rm=TRUE), "\n")

# 3) exposure 必须是：正数（offset 用 log(exposure)）
cat("exposure <= 0:", sum(freq_wc_raw$exposure <= 0, na.rm=TRUE), "\n")
cat("exposure 缺失:", sum(is.na(freq_wc_raw$exposure)), "\n")

# 4) 缺失值概览（每列缺失多少）
na_count <- sapply(freq_wc_raw, function(x) sum(is.na(x)))
na_count[na_count > 0]

# 5) 检查“唯一性”：每行是否是唯一暴露单位？
# 先检查 policy_id 是否唯一
cat("policy_id 是否唯一:", nrow(freq_wc_raw) == n_distinct(freq_wc_raw$policy_id), "\n")

# 再检查 (policy_id, worker_id) 是否唯一
cat("(policy_id, worker_id) 是否唯一:",
    nrow(freq_wc_raw) == n_distinct(paste(freq_wc_raw$policy_id, freq_wc_raw$worker_id)), "\n")

# 如果还不唯一，再看更细的key
cat("(policy_id, worker_id, station_id) 是否唯一:",
    nrow(freq_wc_raw) == n_distinct(paste(freq_wc_raw$policy_id, freq_wc_raw$worker_id, freq_wc_raw$station_id)), "\n")

# 6) 如果发现重复，找出重复样本看看长什么样
freq_wc_raw %>%
  count(policy_id, worker_id, station_id, name = "n") %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  head(20)

# 2: Assume no further cleaning

freq_wc <- freq_wc_raw
sev <- sev_raw

# 3.1 总体频率（每单位 exposure 的索赔率）
total_claims <- sum(freq_wc$claim_count)
total_expo   <- sum(freq_wc$exposure)
cat("Total claims:", total_claims, "\n")
cat("Total exposure:", total_expo, "\n")
cat("Overall claim rate per exposure:", total_claims / total_expo, "\n")

# 3.2 零占比（通常会非常高）
cat("Zero proportion:", mean(freq_wc$claim_count == 0), "\n")

# 3.3 claim_count 的均值和方差（粗看是否过度离散）
cat("Mean count:", mean(freq_wc$claim_count), "\n")
cat("Var  count:", var(freq_wc$claim_count), "\n")

# 粗判过度离散（方差/均值比）Choose between Poisson and NB
var(freq_wc$claim_count) / mean(freq_wc$claim_count)
# 1.010503

# 4. Fit poisson distirbution, calculating Pearson dispersion

# 4.1 拟合 Poisson baseline + 算 Pearson dispersion（最关键）
## Claim_count ~ offset(exposure)
m_pois0 <- glm(
  claim_count ~  offset(log(exposure)),
  family = poisson(link="log"),
  data = freq_wc
)

summary(m_pois0)

phi0 <- sum(residuals(m_pois0, type="pearson")^2) / df.residual(m_pois0)
cat("Pearson dispersion phi0:", phi0, "\n")

#phi0 = 0.9880913  < 1 << 1.2, Poisson functions well
exp(coef(m_pois0))

# Overall claim rate: exp(coef(m_pois0)) =  0.02833957


# 4.2 Adding solar_system (Not very necessary, more like an example)
# 4.2.1 按 solar_system 看差异
freq_wc %>%
  group_by(solar_system) %>%
  summarise(
    n = n(),
    expo = sum(exposure),
    claims = sum(claim_count),
    rate = claims / expo,
    zero_prop = mean(claim_count==0)
  ) %>% arrange(desc(rate))


# 4.2.2 GLM (claim~ solar_system + offset)

m_pois1 <- glm(
  claim_count ~ solar_system + offset(log(exposure)),
  family = poisson(link="log"),
  data = freq_wc
)

summary(m_pois1)

phi1 <- sum(residuals(m_pois1, type="pearson")^2) / df.residual(m_pois1)
cat("Pearson dispersion phi1:", phi1, "\n")

#phi1 = 0.9874432   < 1 << 1.2, Poisson functions well

# 4.3 Domain knowledge (Very important) 

# must-keep predictors: 
  # - solar_system (capturing environmental risk)
  # - occupation (occupation risk)
  # - accident_history_flag (historic risk)



m_pois2 <- glm(
  claim_count ~ solar_system + occupation + accident_history_flag + offset(log(exposure)),
  family = poisson(link="log"),
  data = freq_wc
)

summary(m_pois2)

phi2 <- sum(residuals(m_pois2, type="pearson")^2) / df.residual(m_pois2)
cat("Pearson dispersion phi2:", phi2, "\n")


#phi2 = 0.9911246    < 1 << 1.2, Poisson functions well

# ============================================================
# 4.4 Data-driven variable selection (with train/test split)
# ============================================================

# ------------------------------------------------------------
# 4.4.0 Split data by policy_id (recommended to avoid leakage)
# ------------------------------------------------------------

set.seed(123)

# 先在 policy 层面汇总（关键：exposure/claims 在 policy 层面）
pol_sum <- freq_wc %>%
  group_by(policy_id) %>%
  summarise(
    solar_system = first(solar_system),
    exp_pol      = sum(exposure),
    clm_pol      = sum(claim_count),
    .groups = "drop"
  )

# 在每个 solar_system 内随机打散，然后按累计 exposure 切到 80%
pol_split <- pol_sum %>%
  group_by(solar_system) %>%
  mutate(u = runif(n())) %>%
  arrange(u, .by_group = TRUE) %>%
  mutate(
    cum_exp = cumsum(exp_pol),
    tot_exp = sum(exp_pol),
    in_train = cum_exp <= 0.8 * tot_exp
  ) %>%
  ungroup()

train_ids <- pol_split %>% filter(in_train) %>% pull(policy_id)

train <- freq_wc %>% filter(policy_id %in% train_ids)
test  <- freq_wc %>% filter(!policy_id %in% train_ids)

cat("Rows train/test:", nrow(train), nrow(test), "\n")
cat("Claim rate train:", sum(train$claim_count)/sum(train$exposure), "\n")
cat("Claim rate test :", sum(test$claim_count)/sum(test$exposure), "\n")

# Rows train/test: 104949 26285 
# Claim rate train: 0.02804128 
# Claim rate test : 0.02952984 
# A good split


# Make sure factor levels in test match train (avoid new levels issues)
fix_levels <- function(df, ref){
  facs <- names(ref)[sapply(ref, is.factor)]
  for(nm in facs){
    df[[nm]] <- factor(df[[nm]], levels = levels(ref[[nm]]))
  }
  df
}
train <- fix_levels(train, freq_wc)
test  <- fix_levels(test,  freq_wc)

# ------------------------------------------------------------
# 4.4.1 LASSO + CV on TRAIN only (Poisson with offset)
# ------------------------------------------------------------

mm_formula <- ~ solar_system +
  occupation +
  accident_history_flag +
  experience_yrs +
  psych_stress_index +
  hours_per_week +
  supervision_level +
  gravity_level +
  safety_training_index +
  protective_gear_quality +
  base_salary

X_train <- model.matrix(mm_formula, data = train)[, -1]
y_train <- train$claim_count
off_train <- log(train$exposure)

# Set penalty.factor: do not penalize must-keep predictors
penalty <- rep(1, ncol(X_train))
## Domain knowledge: solar_system, occupation, accident_history_flag

must_keep <- c("solar_system", "occupation", "accident_history_flag")
penalty[grep(paste(must_keep, collapse="|"), colnames(X_train))] <- 0

# generate the number of the penalized/unpenalized predictors

table(penalty)
head(colnames(X_train)[penalty == 0], 20)

set.seed(123)
cvfit <- cv.glmnet(
  x = X_train,
  y = y_train,
  family = "poisson",
  offset = off_train,
  alpha = 1,
  penalty.factor = penalty,
  nfolds = 10,
  type.measure = "deviance"
)

plot(cvfit)
cvfit$lambda.1se #[1] 0.0006671307
cvfit$lambda.min  # [1] 0.0001250081





# Extract selected variables (non-zero coefficients)
b_1se <- coef(cvfit, s = "lambda.1se")
b_min <- coef(cvfit, s = "lambda.min")

sel_1se <- rownames(b_1se)[as.numeric(b_1se) != 0]
sel_min <- rownames(b_min)[as.numeric(b_min) != 0]

sel_1se; length(sel_1se)
sel_min; length(sel_min)

# ------------------------------------------------------------
# 4.4.2 Refit unpenalized GLM on TRAIN for interpretability
# ------------------------------------------------------------

m_A_train <- glm(
  claim_count ~ solar_system + occupation + accident_history_flag +
    offset(log(exposure)),
  family = poisson(link="log"),
  data = train
)

m_B_train <- glm(
  claim_count ~ solar_system + occupation + accident_history_flag +
    psych_stress_index+ safety_training_index  +
    offset(log(exposure)),
  family = poisson(link="log"),
  data = train
)

AIC(m_A_train, m_B_train)

#           df      AIC
# m_A_train 14 14976.32
# m_B_train 16 14941.72
# AIC_A-AIC_B = 34.6, we should use model B even we have 2 more variables.

phiA <- sum(residuals(m_A_train, type="pearson")^2) / df.residual(m_A_train)
phiB <- sum(residuals(m_B_train, type="pearson")^2) / df.residual(m_B_train)
c(phiA=phiA, phiB=phiB)

# 0.9927529 0.9929938 both Poisson

summary(m_A_train)
summary(m_B_train)

exp(coef(m_A_train))
exp(coef(m_B_train))

# Optional: correlation check (on TRAIN to avoid leakage)
cor(train[,c(
  "psych_stress_index",
  "safety_training_index"
)])

# Very uncorrelated, good options
# Now prefer MODEL B

# ------------------------------------------------------------
# 4.4.3 Evaluate on TEST (out-of-sample)
# ------------------------------------------------------------

# helper: Poisson deviance on test
pois_dev <- function(y, mu){
  mu <- pmax(mu, 1e-12)
  term <- ifelse(y==0, 0, y*log(y/mu))
  2*sum(term - (y-mu))
}

# predict expected claim counts on test
test$pred_A_claims <- predict(m_A_train, newdata = test, type = "response")
test$pred_B_claims <- predict(m_B_train, newdata = test, type = "response")

# overall calibration on test
actual_rate_test <- sum(test$claim_count) / sum(test$exposure)
predA_rate_test  <- sum(test$pred_A_claims) / sum(test$exposure)
predB_rate_test  <- sum(test$pred_B_claims) / sum(test$exposure)

c(actual_rate_test=actual_rate_test, predA_rate_test=predA_rate_test, predB_rate_test=predB_rate_test)

# actual_rate_test  predA_rate_test  predB_rate_test 
# 0.02952984       0.02789207       0.02784996 

# deviance on test (lower is better)
devA_test <- pois_dev(test$claim_count, test$pred_A_claims)
devB_test <- pois_dev(test$claim_count, test$pred_B_claims)
c(devA_test=devA_test, devB_test=devB_test)

# 3174.529  3164.617 
# Choose model B

# Choose final model based on TEST performance
# (If devB_test < devA_test, prefer Model B; otherwise Model A)
final_model_wc <- if (devB_test < devA_test) m_B_train else m_A_train
final_name  <- if (devB_test < devA_test) "Model B (enriched)" else "Model A (parsimonious)"
cat("Selected final model:", final_name, "\n")

# ------------------------------------------------------------
# 4.5 Calibration plot on TEST (Predicted vs Actual claim rate)
# ------------------------------------------------------------

test$pred_claims <- predict(final_model_wc, newdata = test, type = "response")
test$pred_rate   <- test$pred_claims / test$exposure
test$actual_rate <- test$claim_count / test$exposure

test <- test %>% mutate(risk_decile = ntile(pred_rate, 10))

calibration_test <- test %>%
  group_by(risk_decile) %>%
  summarise(
    exposure = sum(exposure),
    actual_claims = sum(claim_count),
    predicted_claims = sum(pred_claims)
  ) %>%
  mutate(
    actual_rate = actual_claims / exposure,
    predicted_rate = predicted_claims / exposure
  )

print(calibration_test)

ggplot(calibration_test, aes(x = predicted_rate, y = actual_rate)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = paste0("TEST Calibration Plot: Predicted vs Actual Claim Rates (", final_name, ")"),
    x = "Predicted Claim Rate",
    y = "Actual Claim Rate"
  ) +
  theme_minimal()

# Great calibration plot!

# Save final model for WC_new_old.R (final_model_wc_wc 来源)
# 路径：C:\Users\laptop\Desktop\UNSW study\ACTL5100
saveRDS(final_model_wc, "C:/Users/laptop/Desktop/UNSW study/ACTL5100/final_model_wc_wc.rds")

# Whole pipeline:
  
#  data cleaning
# ↓
# Poisson baseline
# ↓
# dispersion test
# ↓
# LASSO + CV variable selection
# ↓
# final GLM
# ↓
# coefficient interpretation
# ↓
# calibration plot