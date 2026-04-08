# SpaceY: Safe Yields from Space Resources

**Xinran Dai • Yu Gao • Thomas Ko • Rose Parajuli • Yuan Zhang**  
*Advisor: Xiao Xu | University of New South Wales*

---

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [Product Design](#product-design)
3. [Pricing & Capital Modelling](#pricing--capital-modelling)
4. [Risk Assessment](#risk-assessment)
5. [ESG Evaluation](#esg-evaluation)
6. [Modelling Methodology](#modelling-methodology)
7. [Data & Assumptions](#data--assumptions)
8. [Team Contributions](#team-contributions)

---

## Executive Summary

Cosmic Quarry Mining Corporation (CQM) is expanding its interstellar mining operations across three solar systems: **Helionis Cluster**, **Bayesia System**, and **Oryn Delta**. These environments expose the company to four main operational hazards:

| Hazard | Abbreviation | Key Driver |
|---|---|---|
| Workers' Compensation | WC | Occupational injury & illness |
| Cargo Loss | CL | Transit debris & navigation disruption |
| Equipment Failure | EF | Machinery breakdown & radiation |
| Business Interruption | BI | Operational downtime |

We recommend that **Galaxy General Insurance Company** offer a full four-product portfolio. The portfolio is commercially viable under both short-term and long-term views:

- **1-Year Baseline:** Expected loss $505.42M | Premium $631.77M | Net revenue $126.35M
- **10-Year Horizon:** PV expected loss $4,932.55M | Single premium $5,605.17M | Net revenue $672.62M

Stress testing confirms the portfolio remains supportable under adverse conditions, with required capital increasing by **23.5%** under a moderate scenario and **50.9%** under the worst-case scenario.

---

## Product Design

Each hazard line has a tailored benefit structure reflecting its underlying risk characteristics.

| Hazard | Deductible | Limit | Special Feature |
|---|---|---|---|
| WC | VaR50 severity | VaR95 severity | Lower deductible to protect workers |
| CL | VaR60 severity | VaR80 severity | Tighter limit for accumulation risk |
| EF | VaR60 severity | VaR95 severity | Quantum Bore sublimit (VaR90) |
| BI | 2-day revenue proxy | VaR90 severity | Annual aggregate cap (VaR99) |

### Coverage Triggers & Exclusions

**Triggers:**
- **EF:** Sudden machinery breakdown from debris impact, asteroid collision, or radiation disturbance
- **CL:** Physical loss or damage to insured cargo during authorised transit
- **BI:** Measurable revenue loss following operational suspension caused by an insured disruption
- **WC:** Employee injury or illness directly arising from mining activities

**Key Exclusions:** wear and tear, gradual deterioration, inadequate maintenance, operation outside approved limits, intentional misconduct, regulatory non-compliance, pure market-driven revenue decline

### Aggregate Loss Distributions

The figures below show the simulated annual loss distributions across all three solar systems for each hazard line, before applying policy limits. These distributions directly inform the product design choices above.

*See Figure 2.1 in the [full report](SpaceY_report.pdf) for aggregate loss distribution plots by hazard.*

**Key observations:**
- **Cargo Loss** is right-skewed with a heavy Pareto-type tail — drives the tighter VaR80 limit
- **Business Interruption** has the heaviest tail relative to expected loss — justifies the annual aggregate cap
- **Workers' Compensation** is relatively contained — lower deductible preserves worker protection
- **Equipment Failure** is concentrated around Quantum Bore — drives the bespoke sublimit

---

## Pricing & Capital Modelling

### Short-Term Baseline (1-Year)

All amounts in millions of dollars.

| Metric | WC | CL | EF | BI | **Aggregate** |
|---|---|---|---|---|---|
| Expected Loss | 0.56 | 465.18 | 16.17 | 23.50 | **505.42** |
| Std Deviation | 0.05 | 41.06 | 1.20 | 22.51 | **46.86** |
| VaR99 | 0.75 | 641.72 | 20.97 | 150.71 | **814.16** |
| Premium | 0.71 | 581.48 | 20.22 | 29.38 | **631.77** |
| Net Revenue | 0.14 | 116.30 | 4.04 | 5.88 | **126.35** |
| Target Return | 20% | 20% | 20% | 20% | **20%** |

Premiums are set using a **70% target loss ratio**, consistent with Galaxy General's internal pricing benchmarks.

### Long-Term Baseline (10-Year, Present Value)

| Metric | WC | CL | EF | BI | **Aggregate** |
|---|---|---|---|---|---|
| Expected Loss | 5.57 | 4,573.33 | 126.61 | 227.05 | **4,932.55** |
| VaR99 | 6.64 | 5,550.70 | 151.53 | 960.76 | **6,100.88** |
| Single Premium | 6.33 | 5,196.96 | 143.87 | 258.01 | **5,605.17** |
| Net Revenue | 0.76 | 623.64 | 17.26 | 30.96 | **672.62** |
| Target Return | 12% | 12% | 12% | 12% | **12%** |

The lower 12% long-term return reflects the strategic value of a stable multi-year client relationship and the risk mitigation provided by deductibles, limits, and sublimits.

### Stress Testing

| Scenario | Systems Stressed | EL | VaR99 | Required Capital | Delta Capital |
|---|---|---|---|---|---|
| Best / Baseline | All (attritional) | 505.42 | 625.37 | 119.95 | — |
| Moderate | Helionis + Oryn | 605.27 | 753.42 | 148.15 | **+23.5%** |
| Worst | All three | 757.31 | 938.26 | 180.95 | **+50.9%** |

The moderate scenario captures disruption through shared communications and navigation channels. The worst scenario reflects a firm-wide correlated shock via common transport, debris, or regulatory failures.

### Macroeconomic Assumptions (2175 Yield Curve)

| Maturity | Spot Rate | Discount Factor |
|---|---|---|
| 1 year | 2.53% | 0.9753 |
| 5 years | 2.95% | 0.8646 |
| 10 years | 3.48% | 0.7103 |

Intermediate rates are linearly interpolated. The positively sloped yield curve is consistent with the projected macro environment (VAR(1) model, 15-year weighted anchors).

---

## Risk Assessment

### Risk Scores by Solar System

Ordinal scores from 1 (very low) to 5 (very high):

| System | WC | CL | EF | BI | Profile Summary |
|---|---|---|---|---|---|
| Helionis Cluster | 3 | 4 | **5** | 3 | Aging machinery, debris-heavy transit |
| Bayesia System | 4 | 2 | 3 | 4 | Radiation and EM stress |
| Oryn Delta | 4 | **5** | 4 | **5** | Unstable environment, weak comms |

### Top Threats by Risk Level

| Threat | Score | Higher-Risk Systems |
|---|---|---|
| Debris / navigation incidents | **5** | Helionis, Oryn |
| Supply chain / transport disruption | **5** | All, especially Helionis & Oryn |
| Communications / relay failure | 4 | Oryn, Helionis |
| Radiation / electromagnetic events | 4 | Bayesia (spillover to Oryn) |
| Operational expansion strain | 3 | Helionis, Bayesia |
| Environmental / regulatory change | 3 | All systems |
| Equipment breakdown / maintenance | 3 | Helionis |
| Terrain hazards | 3 | Oryn |

### Correlated Risk Scenarios

The three solar systems are distinct but correlated through shared interstellar channels:

| Scenario | Systems | Channel | Main Hazards | Score |
|---|---|---|---|---|
| Deep-space radiation transient | Bayesia + Oryn | Radiation / electronics | EF, BI, WC | 4 |
| Relay / comms backbone failure | Helionis + Oryn | Communications | BI, CL, EF | 4 |
| Navigation / trajectory model failure | Helionis + Oryn | Navigation / debris | CL, EF, BI | 4 |
| Transport congestion / debris cascade | **All three** | Logistics / transport | CL, BI, EF, WC | **5** |

---

## ESG Evaluation

ESG scores are used as underwriting-reference indicators, not direct pricing variables.

### Final ESG Scores

| Solar System | Environmental | Social | Governance | **ESG Score** |
|---|---|---|---|---|
| Helionis Cluster | 58.2 | 65.0 | 67.0 | **62.3** |
| Bayesia System | 61.4 | 54.5 | 70.0 | **63.4** |
| Oryn Delta | 46.4 | 42.0 | 63.8 | **51.8** |
| **Company Overall** | — | — | — | **60.7** |

Overall ESG formula: `ESG_s = 0.50 * E_s + 0.15 * S_s + 0.35 * G_s`

### Underwriting Interpretation

| ESG Range | Interpretation |
|---|---|
| 70+ | Generally acceptable; background reference only |
| 55–70 | Insurable, subject to monitoring |
| 40–55 | Cautious / conditional underwriting |
| Below 40 | Unfavourable without substantial remediation |

Helionis and Bayesia fall into the insurable-but-monitored range. Oryn is closer to cautious underwriting and warrants tighter terms.

---

## Modelling Methodology

### Frequency & Severity Models

| Hazard | Frequency Model | Key Predictors | Severity Model |
|---|---|---|---|
| WC | Poisson GLM | Solar system, occupation, accident history, psych stress, safety training | Spliced Lognormal + Weibull-GPD tail |
| CL | Quasi-Poisson GLM | Route risk, radiation, debris density, cargo type, container type, value, duration, pilot experience | Spliced Lognormal + Frechet-GPD tail |
| BI | Negative Binomial GLM | Solar system, production load, energy backup score | Lognormal |
| EF | Poisson GLM | Equipment type, solar system, age, maintenance intensity, usage intensity | Quantum Bore: Lognormal + bootstrap tail; Other: Lognormal |

### General Frequency Framework

For each risk type, claim counts are modelled using a GLM with log link and exposure offset:

```
log(E[N_i | X_i]) = log(E_i) + eta_i
```

Model family selection is based on empirical variance-to-mean ratio, Pearson dispersion diagnostics, likelihood-based comparisons, and out-of-sample validation.

### Workers' Compensation Frequency

```r
# Poisson GLM for WC claim counts
wc_model <- glm(
  claim_count ~ offset(log(exposure))
    + solar_system
    + occupation
    + accident_history_flag
    + psych_stress_index
    + safety_training_index,
  family = poisson(link = "log"),
  data = wc_data
)
```

Near-equidispersion after conditioning supports Poisson. Psychological stress and safety training improved out-of-sample performance over the parsimonious benchmark.

### Cargo Loss Frequency

```r
# Quasi-Poisson GLM for Cargo Loss (overdispersion adjustment)
cl_model <- glm(
  claim_count ~ offset(log(exposure))
    + route_risk
    + solar_radiation
    + debris_density
    + cargo_type
    + container_type
    + cargo_value
    + transit_duration
    + pilot_experience,
  family = quasipoisson(link = "log"),
  data = cl_data
)
# Negative Binomial considered but overstated dispersion out-of-sample
```

### Business Interruption Frequency

```r
# Negative Binomial GLM - overdispersion confirmed via Pearson stat >> 1
bi_model <- glm.nb(
  claim_count ~ offset(log(exposure))
    + solar_system
    + production_load
    + energy_backup_score,
  data = bi_data
)
```

### Equipment Failure Frequency

```r
# Poisson GLM - near-equidispersed before and after conditioning
ef_model <- glm(
  claim_count ~ offset(log(exposure))
    + equipment_type
    + solar_system
    + equipment_age
    + maintenance_intensity
    + usage_intensity,
  family = poisson(link = "log"),
  data = ef_data
)
```

### Spliced Severity Model (WC Example)

```r
# Lognormal body + GPD tail
splice_threshold <- quantile(wc_claims, 0.90)

# Body: Lognormal
body_fit <- fitdistr(wc_claims[wc_claims <= splice_threshold], "lognormal")

# Tail: GPD via POT
library(evd)
tail_fit <- fpot(wc_claims, threshold = splice_threshold, model = "gpd")

# Simulate from spliced distribution
simulate_spliced <- function(n, phi, body_params, tail_params, threshold) {
  u <- runif(n)
  ifelse(u <= phi,
    qlnorm(u / phi * plnorm(threshold, body_params$meanlog, body_params$sdlog),
           body_params$meanlog, body_params$sdlog),
    threshold + qgpd(
      (u - phi) / (1 - phi),
      loc = 0,
      scale = tail_params$scale,
      shape = tail_params$shape
    )
  )
}
```

### Monte Carlo Aggregate Loss Simulation

```r
# 10,000 simulation runs
n_sim <- 10000
aggregate_loss <- numeric(n_sim)

for (i in 1:n_sim) {
  # Simulate claim counts per stratum
  N_wc  <- rpois(1, lambda = mu_wc)
  N_cl  <- rnbinom(1, mu = mu_cl, size = k_cl)
  N_bi  <- rnbinom(1, mu = mu_bi, size = theta_bi)
  N_ef  <- rpois(1, lambda = mu_ef)

  # Simulate severities
  S_wc <- sum(simulate_spliced(N_wc, phi_wc, body_wc, tail_wc, u_wc))
  S_cl <- sum(simulate_spliced(N_cl, phi_cl, body_cl, tail_cl, u_cl))
  S_bi <- sum(rlnorm(N_bi, meanlog = mu_bi_sev, sdlog = sigma_bi_sev))
  S_ef <- sum(simulate_ef_severity(N_ef, equipment_type))

  # Apply policy limits and deductibles
  S_wc <- apply_policy(S_wc, ded = ded_wc, limit = lim_wc)
  S_cl <- apply_policy(S_cl, ded = ded_cl, limit = lim_cl)
  S_bi <- apply_policy(S_bi, ded = ded_bi, limit = lim_bi, cap = cap_bi)
  S_ef <- apply_policy(S_ef, ded = ded_ef, limit = lim_ef, sublimit_qb = sublim_qb)

  aggregate_loss[i] <- S_wc + S_cl + S_bi + S_ef
}

# Summary metrics
EL    <- mean(aggregate_loss)
VaR99 <- quantile(aggregate_loss, 0.99)
SD    <- sd(aggregate_loss)
```

### Historical-to-New System Transfer (WC Example)

```r
# Step 1: Reconstruct effective exposure
E_eff <- N_FT * mean_exp_FT + N_CT * mean_exp_CT

# Step 2: Occupation relativities from historical model
lambda_hat <- predict(wc_model, type = "response") / exposure
lambda_base <- mean(lambda_hat[occupation == "Technical"])
R_o <- tapply(lambda_hat, occupation, mean) / lambda_base

# Step 3: Solar system proxy via equipment activity
A_se <- equipment_count * operation_ratio
W_s  <- rowSums(A_se)
w_s  <- W_s / sum(W_s)
F_solar <- sum(w_s * F_qualitative)

# Transferred frequency rate
lambda_new <- lambda_base * R_o * F_solar
mu_o <- E_eff * lambda_new
```

### Macroeconomic Forecasting (VAR(1) Model)

```r
library(vars)

# Log-gap transformation to anchor variable
anchor <- apply(macro_data[tail(1:nrow(macro_data), 15), ], 2,
                function(x) weighted.mean(x, w = exp(0.2 * seq_along(x))))

Z <- log(sweep(macro_data, 2, anchor, "/"))

# VAR(1)
var_model <- VAR(Z, p = 1, type = "const")

# Forecast and convert back to levels
z_fcast <- predict(var_model, n.ahead = 10)
x_fcast <- lapply(seq_along(anchor), function(j)
  anchor[j] * exp(z_fcast$fcst[[j]][, "fcst"])
)
```

### Yield Curve Construction (Linear Interpolation, 2175)

```r
r_1y  <- 0.0253
r_10y <- 0.0348

spot_rates <- r_1y + (0:9) / 9 * (r_10y - r_1y)
discount_factors <- 1 / (1 + spot_rates)^(1:10)

# Present value of expected losses
pv_losses <- sum(expected_losses_by_year * discount_factors)
```

---

## Data & Assumptions

### Data Sources
- SOA 2026 Case Study materials: claims datasets for WC, CL, EF, and BI; exposure data; personnel and equipment inventories
- Cosmic Quarry operational data: mine counts, production volumes, planned expansion, workforce composition
- Macroeconomic series: inflation, overnight rate, 1Y and 10Y risk-free spot rates (historical 2160–2174)

### Data Cleaning

| Step | Action |
|---|---|
| String standardisation | Removed invalid suffixes, aligned labels with data dictionary |
| Type conversion | Categorical to factor; quantitative to numeric |
| Range validation | Out-of-range values recoded as missing; no upper-bound filter on claim amounts (preserves large losses) |
| Missing values | Complete-case filtering prior to model fitting |
| Duplicates | First occurrence retained per record identifier |

### Key Assumptions

- Severity distributions are common across solar systems; all severity observations are accurately recorded
- CQM expansion is even over 10 years: 2.5% annual growth in Helionis and Bayesia, 1.5% in Oryn
- WC severity multipliers use median claim size per occupation bucket to reduce heavy-tail influence
- New solar system frequencies are derived via historical-model transfer, not direct re-estimation
- BI deductible approximated as 2 days of lost revenue, allocated by solar system revenue proxy weight

### Data Limitations

- No directly comparable claims experience exists for the three new solar systems; all pricing relies on historical transfer
- Workforce counts are not available by solar system; equipment-based proxies are used for WC allocation
- BI transfer is the most assumption-driven, using proxy-system weights and equipment-based exposure scaling
- New environmental conditions are described qualitatively; the framework is partly judgment-based

---

## Team Contributions

| Member | Primary Responsibilities |
|---|---|
| **Xinran Dai** | Cargo Loss modelling (frequency GLM, severity fitting, transfer methodology) |
| **Yu Gao** | Business Interruption modelling, macroeconomic forecasting (VAR(1), yield curve) |
| **Thomas Ko** | Equipment Failure modelling, Quantum Bore sublimit calibration, stress testing |
| **Rose Parajuli** | Workers' Compensation modelling, ESG scoring framework, risk assessment |
| **Yuan Zhang** | Pricing framework, long-term PV analysis, financial performance projection |

---

## Report & Code

- [Full Report (PDF)](SpaceY_report.pdf)
- [Full Code (zip)](Whole%20modeling%20process.zip)
- [Workers' Compensation Code](code/wc_model.R)
- [Cargo Loss Code](code/cl_model.R)
- [Equipment Failure Code](code/ef_model.R)
- [Business Interruption Code](code/bi_model.R)
- [Pricing & Simulation Code](code/pricing_simulation.R)
- [Macroeconomic Forecasting Code](code/macro_forecast.R)

---

## Disclaimer: AI Usage

AI tools were used in a limited supporting capacity:
- Language refinement and improvement of writing clarity
- Minor code editing and formatting adjustments
- Generation of preliminary brainstorming ideas during early project stages

All actuarial modelling, data analysis, and final interpretations were developed and verified by the authors. The authors retain full responsibility for the methodology and conclusions presented.

---

University of New South Wales | Actuarial Theory and Practice A | 2026
