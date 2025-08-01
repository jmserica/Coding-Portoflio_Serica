# ───────────────────────────────────────────────────────────────────────────────
# R Script: Full Descriptives & PLS‐SEM for Card.xlsx on Mac Desktop
# (blocks section FIXED — rows == blocks, includes Cronbach's α,
#  RQ 1–5 descriptives, PLS‐PM with bootstrap=5000, validity checks, and moderations)
# ───────────────────────────────────────────────────────────────────────────────

# 1. Package Installation & Loading ------------------------------------------------
pkgs <- c("readxl","dplyr","stringr","tidyr","plspm","psych","tibble")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(new_pkgs)) install.packages(new_pkgs, repos="https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only=TRUE))

# 2. Define File Path & Pre-flight Check ------------------------------------------
file_path <- file.path(Sys.getenv("HOME"), "Desktop", "Card.xlsx")
if(!file.exists(file_path)) stop("File not found at: ", file_path)

# 3. Import Excel Data -------------------------------------------------------------
card_df <- readxl::read_excel(path=file_path, sheet=1, col_names=TRUE)

# 4. Trim whitespace in all character columns -------------------------------------
card_df <- card_df %>% mutate(across(where(is.character), str_trim))

# 5. Classify Age Brackets & Recode Other Demographics ----------------------------
card_df <- card_df %>% mutate(
  age_code = case_when(
    Age == "18 – 28" ~ 1L,
    Age == "29 – 44" ~ 2L,
    Age == "45 – 60" ~ 3L,
    Age == "61 – 79" ~ 4L,
    TRUE            ~ NA_integer_
  ),
  sex_code = dplyr::recode(`Biological Sex`,
                           "Male"   = 1L,
                           "Female" = 2L,
                           .default = NA_integer_),
  civil_status_code = dplyr::recode(`Civil Status`,
                                    "Single"    = 1L,
                                    "Married"   = 2L,
                                    "Widowed"   = 3L,
                                    "Divorced"  = 4L,
                                    "Separated" = 5L,
                                    .default    = NA_integer_),
  education_code = dplyr::recode(`Highest Educational Attainment`,
                                 "High school graduate"                    = 1L,
                                 "Trade/technical/vocational training"     = 2L,
                                 "Associate degree"                        = 3L,
                                 "Bachelor’s degree"                       = 4L,
                                 "Master’s degree"                         = 5L,
                                 "Doctorate degree"                        = 6L,
                                 "Professional degree"                     = 7L,
                                 .default = NA_integer_),
  occupation_code = dplyr::recode(Occupation,
                                  "Employed for wages"                        = 1L,
                                  "Self-employed"                             = 2L,
                                  "Out of work and looking for work"          = 3L,
                                  "Out of work but not currently looking for work" = 4L,
                                  "A homemaker"                               = 5L,
                                  "A student"                                 = 6L,
                                  "Retired"                                   = 7L,
                                  .default = NA_integer_),
  income_code = dplyr::recode(`Personal Income (monthly)`,
                              "PHP 21,194 – PHP 43,828"   = 1L,
                              "PHP 43,828 – PHP 76,669"   = 2L,
                              "PHP 76,669 – PHP 131,484"  = 3L,
                              "PHP 131,484 – PHP 219,140" = 4L,
                              "More than PHP 219,140"     = 5L,
                              .default = NA_integer_)
)

# 6. Clean / normalise column names -----------------------------------------------
card_df <- card_df %>% rename_with(~ str_squish(str_replace_all(., "[\r\n]", " ")))

# 7. Locate the survey items by construct -----------------------------------------
incentives_cols   <- names(card_df) %>% str_subset("^Credit Card - Incentives")
service_cols      <- names(card_df) %>% str_subset("^Credit Card - Customer Service")
investment_cols   <- names(card_df) %>% str_subset("^Credit Card - Investment Size")
loyalty_cols      <- names(card_df) %>% str_subset("Customer Loyalty")
satisfaction_cols <- names(card_df) %>% str_subset("Credit Card - Satisfaction")

expected_counts <- c(Incentives=3, Service=4, Investment=3, Loyalty=2, Satisfaction=2)
actual_counts   <- c(length(incentives_cols), length(service_cols), length(investment_cols),
                     length(loyalty_cols),    length(satisfaction_cols))
names(actual_counts) <- names(expected_counts)
if(!all(actual_counts==expected_counts)) {
  stop("Column-detection error → Expected vs. found:\n",
       paste(names(expected_counts), expected_counts, actual_counts, sep=": ", collapse="\n"))
}

# 8. Cast survey items to numeric (_num) ------------------------------------------
all_cc_items <- c(incentives_cols, service_cols, investment_cols, loyalty_cols, satisfaction_cols)
card_df      <- card_df %>% mutate(across(all_of(all_cc_items), as.numeric, .names="{.col}_num"))

# 9. Build vectors of the new _num columns by construct ---------------------------
incentives_cols_num   <- paste0(incentives_cols,   "_num")
service_cols_num      <- paste0(service_cols,      "_num")
investment_cols_num   <- paste0(investment_cols,   "_num")
loyalty_cols_num      <- paste0(loyalty_cols,      "_num")
satisfaction_cols_num <- paste0(satisfaction_cols, "_num")

# 10. Compute each construct’s mean score -----------------------------------------
card_df <- card_df %>% rowwise() %>% mutate(
  incentives_score   = mean(c_across(all_of(incentives_cols_num)),   na.rm=TRUE),
  service_score      = mean(c_across(all_of(service_cols_num)),      na.rm=TRUE),
  investment_score   = mean(c_across(all_of(investment_cols_num)),   na.rm=TRUE),
  loyalty_score      = mean(c_across(all_of(loyalty_cols_num)),      na.rm=TRUE),
  satisfaction_score = mean(c_across(all_of(satisfaction_cols_num)), na.rm=TRUE)
) %>% ungroup()

# 11. Preview the new construct scores --------------------------------------------
cat(">>> Preview of construct scores:\n")
print(card_df %>% select(ends_with("_score")) %>% slice_head(n=5))

# 12. Descriptive Statistics: Constructs & Items ----------------------------------
## 12.1 Construct-level -----------------------------------------------------------
construct_stats <- card_df %>% summarize(
  incentives_mean   = mean(incentives_score,   na.rm=TRUE),
  incentives_sd     = sd(  incentives_score,   na.rm=TRUE),
  service_mean      = mean(service_score,      na.rm=TRUE),
  service_sd        = sd(  service_score,      na.rm=TRUE),
  investment_mean   = mean(investment_score,   na.rm=TRUE),
  investment_sd     = sd(  investment_score,   na.rm=TRUE),
  loyalty_mean      = mean(loyalty_score,      na.rm=TRUE),
  loyalty_sd        = sd(  loyalty_score,      na.rm=TRUE),
  satisfaction_mean = mean(satisfaction_score, na.rm=TRUE),
  satisfaction_sd   = sd(  satisfaction_score, na.rm=TRUE)
)
cat("\n>>> Construct-level Descriptives:\n")
print(construct_stats)

## 12.2 Item-level ----------------------------------------------------------------
item_cols <- c(incentives_cols_num, service_cols_num, investment_cols_num,
               loyalty_cols_num, satisfaction_cols_num)
item_stats <- card_df %>% select(all_of(item_cols)) %>%
  pivot_longer(everything(), names_to="item", values_to="value") %>%
  group_by(item) %>%
  summarize(mean=mean(value,na.rm=TRUE), sd=sd(value,na.rm=TRUE), .groups="drop")
cat("\n>>> Item-level Descriptives:\n")
print(item_stats)

## 12.3 Per-Construct Item Summaries ----------------------------------------------
cat("\n>>> Incentives Items:\n");   print(item_stats %>% filter(item %in% incentives_cols_num))
cat("\n>>> Service Items:\n");      print(item_stats %>% filter(item %in% service_cols_num))
cat("\n>>> Investment Items:\n");   print(item_stats %>% filter(item %in% investment_cols_num))
cat("\n>>> Loyalty Items:\n");      print(item_stats %>% filter(item %in% loyalty_cols_num))
cat("\n>>> Satisfaction Items:\n"); print(item_stats %>% filter(item %in% satisfaction_cols_num))

## 12.4 Cronbach’s α per construct -------------------------------------------------
reliability_df   <- card_df %>% select(all_of(item_cols)) %>% na.omit()
cronbach_results <- list(
  Incentives   = psych::alpha(reliability_df[, incentives_cols_num])$total$raw_alpha,
  Service      = psych::alpha(reliability_df[, service_cols_num])$total$raw_alpha,
  Investment   = psych::alpha(reliability_df[, investment_cols_num])$total$raw_alpha,
  Loyalty      = psych::alpha(reliability_df[, loyalty_cols_num])$total$raw_alpha,
  Satisfaction = psych::alpha(reliability_df[, satisfaction_cols_num])$total$raw_alpha
)
cronbach_df <- data.frame(Construct=names(cronbach_results),
                          Alpha=unlist(cronbach_results))
cat("\n>>> Cronbach’s α per construct:\n")
print(cronbach_df)

# 13. RQ 1–5: Construct-level Means & SDs -----------------------------------------
rq_stats <- construct_stats
cat("\n>>> RQ 1–5 Descriptives:\n")
print(rq_stats)

# 14. PLS‐PM Specification & Estimation (H1–H8) -----------------------------------
sem_data <- card_df %>% select(all_of(c(
  incentives_cols_num, service_cols_num, satisfaction_cols_num, investment_cols_num, loyalty_cols_num)))
blocks <- list(
  Incentives   = incentives_cols_num,
  Service      = service_cols_num,
  Satisfaction = satisfaction_cols_num,
  Investment   = investment_cols_num,
  Loyalty      = loyalty_cols_num
)
inner_model <- matrix(0,5,5, dimnames=list(names(blocks), names(blocks)))
inner_model["Satisfaction",c("Incentives","Service")] <- 1
inner_model["Investment",  c("Incentives","Service")] <- 1
inner_model["Loyalty",     c("Incentives","Service","Satisfaction","Investment")] <- 1
modes <- rep("A", length(blocks))
pls_out <- plspm(sem_data, inner_model, blocks, modes,
                 scaled=TRUE, boot.val=TRUE, br=10000)

# --------- Indicator (Factor) Loadings Per Construct (robust print for all rows) ------
indicator_loadings <- dplyr::select(pls_out$outer_model,
                                    Construct = block,
                                    Indicator = name,
                                    Loading   = loading)
indicator_loadings <- dplyr::arrange(indicator_loadings,
                                     Construct, dplyr::desc(abs(Loading)))
indicator_loadings <- tibble::as_tibble(indicator_loadings)  # for safe printing
cat("\n>>> Indicator (Factor) Loadings Per Construct:\n")
print(indicator_loadings, n = Inf)

# ----------------------------- R² for PLS-SEM Endogenous Constructs -----------------
cat("\n>>> PLS-SEM R-squared (Endogenous Constructs):\n")
pls_r2_vec <- pls_out$R2
pls_r2 <- data.frame(
  Construct = names(pls_r2_vec),
  R2 = round(as.numeric(pls_r2_vec), 3),
  row.names = NULL,
  stringsAsFactors = FALSE
)
print(pls_r2, row.names = FALSE)
# ------------------------------------------------------------------------------------

# 15. Direct Effects (H1–H8) & p-values ----------------------------------------
boot_paths <- pls_out$boot$paths %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Path") %>%
  filter(Original != 0)
coef_tbl <- boot_paths %>%
  mutate(
    Beta      = round(Original,   3),
    SE        = round(Std.Error,  3),
    z         = round(Original/Std.Error, 3),
    p.value   = round(2*pnorm(-abs(Original/Std.Error)), 4),
    Supported = ifelse(p.value<0.05,"Yes","No")
  ) %>%
  select(Path, Beta, SE, z, p.value, Supported)
cat("\n>>> H1–H8 Path Coefficients & Significance:\n")
print(coef_tbl, row.names=FALSE)

# 16. Indirect & Total Effects (Mediation H3–H6) -----------------------------------
effs <- as.data.frame(pls_out$effects) %>%
  mutate(across(where(is.numeric), ~ round(.x,3)))
cat("\n>>> Mediation Effects (Indirect & Total):\n")
print(effs)

# 17. Convergent Validity (AVE & Composite Reliability) ---------------------------
loadings <- pls_out$outer_model
constructs <- names(blocks)
convergent_results <- do.call(rbind, lapply(constructs, function(lv) {
  lv_loads <- loadings$loading[loadings$block==lv]
  ave      <- sum(lv_loads^2)/length(lv_loads)
  sum_loads<- sum(lv_loads)
  error_var<- sum(1 - lv_loads^2)
  cr       <- sum_loads^2/(sum_loads^2 + error_var)
  data.frame(Construct=lv, AVE=ave, CompositeReliability=cr)
}))
cat("\n>>> Convergent Validity (AVE & Composite Reliability):\n")
print(convergent_results)

# 18. Discriminant Validity -------------------------------------------------------
lv_scores<- pls_out$scores
ave_vals <- setNames(convergent_results$AVE, convergent_results$Construct)
corr_lv  <- cor(lv_scores)
fl_matrix<- corr_lv; diag(fl_matrix)<- sqrt(ave_vals[colnames(fl_matrix)])
cat("\n>>> Fornell–Larcker Discriminant Validity (√AVE on diagonal):\n")
print(round(fl_matrix,3))

# 18.2 Indicator Cross-Loadings ---------------------------------------------------
manifests <- unlist(blocks)
crosslds  <- cor(sem_data[,manifests], lv_scores, use="pairwise.complete.obs")
cat("\n>>> Indicator Cross-Loadings (each item vs. all LVs):\n")
print(round(crosslds,3))

# 19. Average Full Collinearity VIF (AFVIF) ---------------------------------------
# 19a. Compute mean VIF per construct -----------------------------------------
vif_fun <- function(data, items) {
  vifs <- sapply(items, function(x) {
    others <- setdiff(items, x)
    if(length(others)==0) return(NA)
    # Add backticks to handle variable names with spaces/special characters
    safe_x <- paste0("`", x, "`")
    safe_others <- paste0("`", others, "`")
    mdl <- lm(as.formula(paste(safe_x, "~", paste(safe_others, collapse="+"))), data=data)
    1/(1 - summary(mdl)$r.squared)
  })
  tibble::tibble(Item=items, VIF=round(as.numeric(vifs),3))
}

construct_vif_list <- lapply(names(blocks), function(lv) {
  vdf <- vif_fun(data = card_df, items = blocks[[lv]])
  mean_vif <- mean(vdf$VIF, na.rm = TRUE)
  data.frame(Construct = lv, Average_VIF = round(mean_vif, 3))
})
construct_vif_df <- do.call(rbind, construct_vif_list)
cat("\n>>> Average Collinearity VIF per Construct:\n")
print(construct_vif_df)

# 20. Reliability Coefficients (Cronbach’s α & Composite Reliability) -------------
reliability_coeffs <- merge(
  cronbach_df,
  convergent_results[,c("Construct","CompositeReliability")],
  by="Construct"
)
reliability_coeffs$Alpha                <- round(reliability_coeffs$Alpha,3)
reliability_coeffs$CompositeReliability <- round(reliability_coeffs$CompositeReliability,3)
cat("\n>>> Reliability Coefficients per latent variable:\n")
print(reliability_coeffs)

# 20b. Dillon–Goldstein's Rho-A (ρA) per construct -------------------------------
rhoA_results <- data.frame(
  Construct=names(blocks),
  RhoA=sapply(blocks, function(cols) psych::alpha(reliability_df[,cols])$total$std.alpha)
)
rhoA_results$RhoA <- round(rhoA_results$RhoA,3)
cat("\n>>> Dillon–Goldstein’s Rho-A (ρA) per construct:\n")
print(rhoA_results)

# 20c. Heterotrait–Monotrait Ratio (HTMT) Matrix ---------------------------------
htmt_matrix <- function(data, blocks) {
  res <- matrix(NA, nrow=length(blocks), ncol=length(blocks),
                dimnames=list(names(blocks),names(blocks)))
  for(i in seq_along(blocks)) for(j in seq_along(blocks)) {
    if(i==j) next
    X <- as.matrix(data[,blocks[[i]]])
    Y <- as.matrix(data[,blocks[[j]]])
    vals <- c()
    for(x in 1:ncol(X)) for(y in 1:ncol(Y)) {
      vals <- c(vals, abs(cor(X[,x], Y[,y], use="pairwise.complete.obs")))
    }
    res[i,j] <- mean(vals, na.rm=TRUE)
  }
  res
}
htmt_out <- htmt_matrix(card_df, blocks)
cat("\n>>> Heterotrait–Monotrait Ratio (HTMT):\n")
print(round(htmt_out,3))

# 21. Define run_mod() function for moderation ------------------------------------
run_mod <- function(formula, df, label) {
  mod <- lm(formula, data = df)
  res <- summary(mod)
  coefs <- as.data.frame(coef(res))
  coefs$Variable <- rownames(coefs)
  rownames(coefs) <- NULL
  cat("\n", label, "\n")
  print(coefs)
  cat(sprintf("R-squared: %.3f | Adj. R-squared: %.3f\n", res$r.squared, res$adj.r.squared))
  invisible(list(model=mod, summary=res, coefs=coefs, R2=res$r.squared, adjR2=res$adj.r.squared))
}

# 22. Moderation Check by Demographics (H7–H8 expanded) ---------------------------
moderators <- list(
  Age          = "age_code",
  Sex          = "sex_code",
  CivilStatus  = "civil_status_code",
  Education    = "education_code",
  Occupation   = "occupation_code",
  Income       = "income_code"
)

# Create interaction terms for incentives and service scores with each moderator
for (mod_name in names(moderators)) {
  code_col <- moderators[[mod_name]]
  card_df <- card_df %>% mutate(
    !!paste0("incent_", code_col) := incentives_score * .data[[code_col]],
    !!paste0("service_", code_col) := service_score    * .data[[code_col]]
  )
}

# Run moderation analyses for each demographic moderator
for (mod_name in names(moderators)) {
  code_col <- moderators[[mod_name]]
  cat(sprintf("\n>>> Moderation by %s (%s)\n", mod_name, code_col))
  # Incentives × Moderator → Loyalty
  run_mod(
    formula = as.formula(paste(
      "loyalty_score ~ incentives_score +", code_col, "+", paste0("incent_", code_col)
    )),
    df     = card_df,
    label  = paste0("H: Incentives × ", mod_name, " → Loyalty")
  )
  # Service × Moderator → Loyalty
  run_mod(
    formula = as.formula(paste(
      "loyalty_score ~ service_score +", code_col, "+", paste0("service_", code_col)
    )),
    df     = card_df,
    label  = paste0("H: Service × ", mod_name, " → Loyalty")
  )
}
