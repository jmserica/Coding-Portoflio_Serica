# moderation_analysis.R
# Purpose: Robust panel moderation analysis (fixed‑effects for ROA/ROE) with diagnostics
# Author: [Your Name]
# Date: 2025-04-19

# ===============================================================================
# 0.  House‑Keeping | Load & attach required packages
# ===============================================================================
required_pkgs <- c(
  "readxl", "dplyr", "plm", "lmtest", "sandwich", "psych", "car", "fixest"
)
invisible(lapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# ------------------------------------------------------------------------------
# 1.  Data Import & Preprocessing
# ------------------------------------------------------------------------------
data_file <- normalizePath("~/Desktop/CARLIT.xlsx", mustWork = TRUE)
raw       <- readxl::read_excel(data_file)
stopifnot(all(c("FirmID", "Year") %in% names(raw)))

df <- raw %>%
  mutate(
    log_TA  = log(TA),
    log_GWP = log(GWP)
  )

# ------------------------------------------------------------------------------
# 2.  Descriptive Summary (sanity check)
# ------------------------------------------------------------------------------
cat("\nBase Summary:\n");     print(summary(df))
cat("\nDetailed (psych::describe):\n"); print(psych::describe(df))

# ------------------------------------------------------------------------------
# 3.  Panel Setup
# ------------------------------------------------------------------------------
pdata <- plm::pdata.frame(df, index = c("FirmID", "Year"))

# ------------------------------------------------------------------------------
# 4.  Pooled OLS Benchmark (HC1 SE)
# ------------------------------------------------------------------------------
quick_lm <- function(dep, rhs, data = df) {
  form  <- as.formula(paste(dep, "~", rhs))
  mod   <- lm(form, data)
  cat("\n--- Pooled OLS:", dep, "---\n")
  print(summary(mod))
  se    <- sqrt(diag(vcovHC(mod, type = "HC1")))
  cat("Robust SE (HC1):\n"); print(se)
  invisible(mod)
}

dvs      <- c("ROA", "ROE")
ivs      <- c("OPM","ER","IR","GR","log_TA","FL","FLEV","MS")
base_rhs <- paste(ivs, collapse = " + ")
pooled_lm_list <- lapply(dvs, function(dv) quick_lm(dv, base_rhs))

# ------------------------------------------------------------------------------
# 5.  Multiple Linear Regression (Standard OLS)
# ------------------------------------------------------------------------------
# ROA model
cat("\n--- Multiple Linear Regression (ROA) ---\n")
lm_ROA <- lm(ROA ~ OPM + ER + IR + GR + log_TA + FL + FLEV + MS, data = df)
print(summary(lm_ROA))

# ROE model
cat("\n--- Multiple Linear Regression (ROE) ---\n")
lm_ROE <- lm(ROE ~ OPM + ER + IR + GR + log_TA + FL + FLEV + MS, data = df)
print(summary(lm_ROE))

# Robust SEs for OLS models
cat("\n--- Robust SEs (HC1) for Multiple Regression ---\n")
se_ROA <- sqrt(diag(vcovHC(lm_ROA, type = "HC1")))
se_ROE <- sqrt(diag(vcovHC(lm_ROE, type = "HC1")))
cat("ROA robust SE:\n"); print(se_ROA)
cat("ROE robust SE:\n"); print(se_ROE)

# ------------------------------------------------------------------------------
# 6.  Diagnostic Tests
# ------------------------------------------------------------------------------
# 6.1  Correlation Matrix among IVs
cat("\n--- Correlation Matrix for IVs ---\n")
cor_matrix <- cor(df[, ivs], use = "pairwise.complete.obs")
print(round(cor_matrix, 3))

# 6.2  Variance Inflation Factors (VIF) for Multicollinearity
cat("\n--- VIF for IVs (based on pooled OLS for ROA) ---\n")
vif_model_ROA <- lm(as.formula(paste("ROA ~", base_rhs)), data = df)
print(car::vif(vif_model_ROA))

cat("\n--- VIF for IVs (based on pooled OLS for ROE) ---\n")
vif_model_ROE <- lm(as.formula(paste("ROE ~", base_rhs)), data = df)
print(car::vif(vif_model_ROE))

# 6.3  Loop through each DV: Panel model selection & specification tests
for (dv in dvs) {
  rhs_formula <- paste(base_rhs)
  plm_formula <- as.formula(paste(dv, "~", base_rhs))
  pooled_plm  <- plm::plm(plm_formula, data = pdata, model = "pooling")
  fe_plm      <- plm::plm(plm_formula, data = pdata, model = "within", effect = "individual")
  re_plm      <- plm::plm(plm_formula, data = pdata, model = "random", effect = "individual")
  cat("\n=== Diagnostics for", dv, "===\n")
  cat("\n--- F-Test (pFtest) for Fixed Effects vs. Pooled OLS ---\n")
  print(plm::pFtest(fe_plm, pooled_plm))
  cat("\n--- Hausman Test (phtest) for FE vs. RE ---\n")
  print(plm::phtest(fe_plm, re_plm))
  cat("\n--- Wooldridge Test (pbgtest) for Serial Correlation ---\n")
  print(plm::pbgtest(fe_plm))
  cat("\n--- Breusch-Pagan Test (bptest) for Heteroskedasticity ---\n")
  print(lmtest::bptest(pooled_plm))
}

# ------------------------------------------------------------------------------
# 7.  Fixed‑Effects + Single‑Moderator Interaction (example)
# ------------------------------------------------------------------------------
mod  <- "Age"
rhs1 <- base_rhs
rhs2 <- paste0(rhs1, " + ", mod, " + (", ivs[1], " * ", mod, ")")
for (dv in dvs) {
  formula_fe <- as.formula(paste(dv, "~", rhs2, "| FirmID"))
  cat("\n=== FE Moderation for", dv, "with moderator", mod, "===\n")
  fe_mod <- feols(formula_fe, data = df, cluster = "FirmID")
  print(summary(fe_mod))
}

# ------------------------------------------------------------------------------
# 8.  FE Moderation: All IVs × All Moderators
# ------------------------------------------------------------------------------
moderators <- c("Age", "log_TA", "log_GWP")
for (dv in dvs) {
  for (mod in moderators) {
    rhs_base         <- paste(ivs, collapse = " + ")
    interaction_terms <- paste(paste0(ivs, ":", mod), collapse = " + ")
    rhs_m            <- paste(rhs_base, "+", mod, "+", interaction_terms)
    formula_fe       <- as.formula(paste(dv, "~", rhs_m, "| FirmID"))
    cat("\n>>> FE Moderation for", dv, "×", mod, "<<<\n")
    fe_mod           <- feols(formula_fe, data = df, cluster = "FirmID")
    print(summary(fe_mod, vcov = "cluster"))
  }
}

# ===============================================================================
# END OF SCRIPT
# ===============================================================================
