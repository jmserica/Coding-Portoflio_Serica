# ───────────────────────────────────────────────────────────────────────────────
# R Script: Full Descriptives & PLS-SEM for Generic.xlsx on Mac Desktop
# (blocks section FIXED — rows == blocks, includes Cronbach's alpha & convergent validity, PLS R2 and moderation R2)
# ───────────────────────────────────────────────────────────────────────────────

# 1. Package Installation & Loading ------------------------------------------------
pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "plspm", "psych")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

# 2. Define File Path & Pre-flight Check ------------------------------------------
file_path <- file.path(Sys.getenv("HOME"), "Desktop", "Generic.xlsx")
if (!file.exists(file_path)) stop("File not found at: ", file_path)

# 3. Import Excel Data -------------------------------------------------------------
generic_df <- readxl::read_excel(path = file_path, sheet = 1, col_names = TRUE)

# 4. Trim whitespace in all character columns -------------------------------------
generic_df <- generic_df %>% mutate(across(where(is.character), str_trim))

# 5. Recode Demographics into Integer Labels --------------------------------------
generic_df <- generic_df %>% mutate(
  gender_code       = dplyr::recode(Gender,
                                    "Male"   = 1,
                                    "Female" = 2,
                                    .default = NA_real_),
  age_code          = dplyr::recode(`Age`,
                                    "18-30 years old" = 1,
                                    "31-40 years old" = 2,
                                    "41-50 years old" = 3,
                                    "51-60 years old" = 4,
                                    "60 above"        = 5,
                                    .default          = NA_real_),
  civil_status_code = dplyr::recode(`Civil Status`,
                                    "Single"  = 1,
                                    "Married" = 2,
                                    .default  = NA_real_),
  education_code    = dplyr::recode(`Education Level`,
                                    "No formal education" = 1,
                                    "Elementary school"   = 2,
                                    "High School"         = 3,
                                    "Undergraduate"       = 4,
                                    "Master's degree"     = 5,
                                    "PhD or above"        = 6,
                                    "Other"               = 7,
                                    .default              = NA_real_),
  income_code       = dplyr::recode(`Household Income (monthly)`,
                                    "Below 20,000"  = 1,
                                    "20,001-40,000" = 2,
                                    "40,001-60,000" = 3,
                                    "60,001-80,000" = 4,
                                    "Above 80,000"  = 5,
                                    .default        = NA_real_)
)

# 6. Define the 21 survey-item column names exactly as in your sheet ---------------
all_items <- c(
  # Knowledge (5)
  "The efficacy and safety of generic medicines are guaranteed",
  "I am very well-informed of the effectiveness of generic medicines",
  "My past experience with generic medicines significantly influence my purchasing decisions",
  "Brand name has very large influence of my choice of generic medicines",
  "There is a very important relation between the standard packaging ( labelling of the drug product which is compliant to the requirement of the health regulations) of generic medicines and their quality",
  # Perception (3)
  "Generic medicines are as effective as branded medicines",
  "Generic medicines have the same quality as branded medicines",
  "Generic medicines have more side effects than the branded medicines",
  # Trust (6)
  "Generic medicines give me a feeling of trust",
  "Generic medicines give me a trustworthy impression",
  "I have trust in generic medicines",
  "Generic medicines can be relied upon to keep promises",
  "Generic medicines are trustworthy",
  "I have full confidence in generic medicines",
  # Purchase Intention (3)
  "I would buy generic medicines in the near future",
  "I plan to buy generic medicines on a regular basis",
  "I intend to buy generic medicines for my long-term health benefits",
  # Purchase Behavior (4)
  "I buy generic medicines",
  "I buy generic medicines on a regular basis",
  "I buy generic medicines because they are safe to consume",
  "I buy generic medicines for my health"
)

# 7. Ensure all_items exist --------------------------------------------------------
missing <- setdiff(all_items, names(generic_df))
if (length(missing)) stop("Missing survey columns:\n", paste(missing, collapse = "\n"))

# 8. Cast survey-items to numeric (_num suffix) ------------------------------------
generic_df <- generic_df %>%
  mutate(across(all_of(all_items), as.numeric, .names = "{.col}_num"))

# 9. Build vectors of the new _num columns by construct ----------------------------
knowledge_cols   <- paste0(all_items[1:5],  "_num")
perception_cols  <- paste0(all_items[6:8],  "_num")
trust_cols       <- paste0(all_items[9:14], "_num")
intent_cols      <- paste0(all_items[15:17], "_num")
behavior_cols    <- paste0(all_items[18:21], "_num")

# 10. Compute each construct’s mean score -----------------------------------------
generic_df <- generic_df %>%
  rowwise() %>%
  mutate(
    knowledge_score         = mean(c_across(all_of(knowledge_cols)),    na.rm = TRUE),
    perception_score        = mean(c_across(all_of(perception_cols)),   na.rm = TRUE),
    trust_score             = mean(c_across(all_of(trust_cols)),        na.rm = TRUE),
    purchase_intent_score   = mean(c_across(all_of(intent_cols)),       na.rm = TRUE),
    purchase_behavior_score = mean(c_across(all_of(behavior_cols)),     na.rm = TRUE)
  ) %>%
  ungroup()

# 11. Preview (first 5 rows) -------------------------------------------------------
cat(">>> Preview of _num & _score columns:\n")
generic_df %>%
  select(ends_with("_num"), ends_with("_score")) %>%
  slice_head(n = 5) %>%
  print()

# 12. Descriptive Statistics: Constructs & Items -----------------------------------
# 12.1 Construct-level -------------------------------------------------------------
construct_stats <- generic_df %>%
  summarize(
    knowledge_mean         = mean(knowledge_score,         na.rm = TRUE),
    knowledge_sd           = sd(  knowledge_score,         na.rm = TRUE),
    perception_mean        = mean(perception_score,        na.rm = TRUE),
    perception_sd          = sd(  perception_score,        na.rm = TRUE),
    trust_mean             = mean(trust_score,             na.rm = TRUE),
    trust_sd               = sd(  trust_score,             na.rm = TRUE),
    purchase_intent_mean   = mean(purchase_intent_score,   na.rm = TRUE),
    purchase_intent_sd     = sd(  purchase_intent_score,   na.rm = TRUE),
    purchase_behavior_mean = mean(purchase_behavior_score, na.rm = TRUE),
    purchase_behavior_sd   = sd(  purchase_behavior_score, na.rm = TRUE)
  )
cat("\n>>> Construct-level Descriptives:\n")
print(construct_stats)

# 12.2 Item-level (all 21) ---------------------------------------------------------
item_cols <- c(knowledge_cols, perception_cols, trust_cols, intent_cols, behavior_cols)
item_stats <- generic_df %>%
  select(all_of(item_cols)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "value") %>%
  group_by(item) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(  value, na.rm = TRUE),
    .groups = "drop"
  )
cat("\n>>> Item-level Descriptives (all 21 items):\n")
print(item_stats, n = nrow(item_stats))

# 12.3 Per-Construct Item Summaries ------------------------------------------------
cat("\n>>> Knowledge Items:\n");   item_stats %>% filter(item %in% knowledge_cols)  %>% print(n = nrow(.))
cat("\n>>> Perception Items:\n"); item_stats %>% filter(item %in% perception_cols) %>% print(n = nrow(.))
cat("\n>>> Trust Items:\n");      item_stats %>% filter(item %in% trust_cols)      %>% print(n = nrow(.))
cat("\n>>> Intention Items:\n");  item_stats %>% filter(item %in% intent_cols)     %>% print(n = nrow(.))
cat("\n>>> Behavior Items:\n");   item_stats %>% filter(item %in% behavior_cols)   %>% print(n = nrow(.))

# 12.4 Cronbach's alpha per construct ---------------------------------------------
reliability_df <- generic_df %>%
  select(all_of(item_cols)) %>%
  na.omit()
cronbach_results <- list(
  Knowledge        = psych::alpha(reliability_df[, knowledge_cols])$total$raw_alpha,
  Perception       = psych::alpha(reliability_df[, perception_cols])$total$raw_alpha,
  Trust            = psych::alpha(reliability_df[, trust_cols])$total$raw_alpha,
  PurchaseIntent   = psych::alpha(reliability_df[, intent_cols])$total$raw_alpha,
  PurchaseBehavior = psych::alpha(reliability_df[, behavior_cols])$total$raw_alpha
)
cronbach_df <- data.frame(Construct = names(cronbach_results), Alpha = unlist(cronbach_results))
cat("\n>>> Cronbach's alpha per construct:\n")
print(cronbach_df)

# 13. PLS-SEM (plspm) -------------------------------------------------------------
# 13.1 Create demographic × knowledge/perception interactions ----------------------
demogs <- c("gender_code","age_code","civil_status_code","education_code","income_code")
for (d in demogs) {
  generic_df[[paste0("Know_",   d)]] <- generic_df$knowledge_score  * generic_df[[d]]
  generic_df[[paste0("Percep_", d)]] <- generic_df$perception_score * generic_df[[d]]
}

# 13.2 Define latent-variable order ------------------------------------------------
lv_order <- c(paste0("Know_",   demogs), paste0("Percep_", demogs),
              "Knowledge","Perception","Trust","PurchaseIntent","PurchaseBehavior")

# 13.3 Define measurement blocks ---------------------------------------------------
blocks <- c(
  setNames(lapply(demogs, function(d) paste0("Know_",   d)), paste0("Know_",   demogs)),
  setNames(lapply(demogs, function(d) paste0("Percep_", d)), paste0("Percep_", demogs)),
  list(Knowledge        = knowledge_cols,
       Perception       = perception_cols,
       Trust            = trust_cols,
       PurchaseIntent   = intent_cols,
       PurchaseBehavior = behavior_cols)
)
stopifnot(length(blocks) == length(lv_order))

# 13.4 Structural (inner) model definition ----------------------------------------
inner <- matrix(0, nrow = length(lv_order), ncol = length(lv_order),
                dimnames = list(lv_order, lv_order))
inner["Trust",         "Knowledge"]        <- 1
inner["Trust",         "Perception"]       <- 1
inner["PurchaseIntent","Knowledge"]        <- 1
inner["PurchaseIntent","Perception"]       <- 1
inner["PurchaseBehavior","Knowledge"]      <- 1
inner["PurchaseBehavior","Perception"]     <- 1
inner["PurchaseIntent","Trust"]            <- 1
inner["PurchaseBehavior","Trust"]          <- 1
inner["PurchaseBehavior","PurchaseIntent"] <- 1
for (d in demogs) {
  inner["PurchaseIntent",    paste0("Know_",   d)] <- 1
  inner["PurchaseBehavior",  paste0("Know_",   d)] <- 1
  inner["PurchaseIntent",    paste0("Percep_", d)] <- 1
  inner["PurchaseBehavior",  paste0("Percep_", d)] <- 1
}

# 13.5 Prepare SEM data.frame & drop incomplete cases -----------------------------
manifest_vars <- unlist(blocks)
sem_data <- generic_df[stats::complete.cases(generic_df[, manifest_vars]), manifest_vars]

# 13.6 Run PLS-PM with bootstrap --------------------------------------------------
pls_out <- plspm(Data           = sem_data,
                 path_matrix    = inner,
                 blocks         = blocks,
                 modes          = rep("A", length(blocks)),
                 scaled         = TRUE,
                 boot.val       = TRUE,
                 br             = 5000)

# ----------- R² for PLS-SEM Endogenous Constructs -----------
cat("\n>>> PLS-SEM R-squared (Endogenous Constructs):\n")
pls_r2_vec <- pls_out$R2
pls_r2 <- data.frame(
  Construct = names(pls_r2_vec),
  R2 = round(as.numeric(pls_r2_vec), 3),
  row.names = NULL,
  stringsAsFactors = FALSE
)
print(pls_r2, row.names = FALSE)
# ------------------------------------------------------------

# 13.7 Print PLS-PM results -------------------------------------------------------
cat("\n>>> Outer (measurement) loadings & communalities:\n")
print(pls_out$outer_model)
cat("\n>>> Inner (structural) path coefficients & R²:\n")
print(pls_out$inner_model)
cat("\n>>> Bootstrapped path estimates (mean, std.error, t, p):\n")
print(pls_out$boot$paths)
cat("\n>>> Total effects (direct + indirect):\n")
print(pls_out$effects)

# ------------------- MODERATION REGRESSIONS (All Interactions, with R²) -------------------
mod_results <- list()
for (d in demogs) {
  # Clean any accidental whitespace in var names
  safe_d <- gsub("\\s+", "_", d)
  know_var   <- paste0("Know_",   safe_d)
  percep_var <- paste0("Percep_", safe_d)
  
  # Knowledge × Demog → Purchase Intention
  f_ki <- as.formula(paste("purchase_intent_score ~ knowledge_score +", d, "+", know_var))
  fit_ki <- lm(f_ki, data = generic_df)
  cat(sprintf("\nModeration: Knowledge × %s → Purchase Intention\n", d))
  print(summary(fit_ki)$coefficients)
  cat(sprintf("R-squared: %.3f | Adj. R-squared: %.3f\n", summary(fit_ki)$r.squared, summary(fit_ki)$adj.r.squared))
  mod_results[[paste0("KI_", d)]] <- list(
    model = fit_ki,
    r2    = summary(fit_ki)$r.squared,
    adjr2 = summary(fit_ki)$adj.r.squared
  )
  
  # Perception × Demog → Purchase Intention
  f_pi <- as.formula(paste("purchase_intent_score ~ perception_score +", d, "+", percep_var))
  fit_pi <- lm(f_pi, data = generic_df)
  cat(sprintf("\nModeration: Perception × %s → Purchase Intention\n", d))
  print(summary(fit_pi)$coefficients)
  cat(sprintf("R-squared: %.3f | Adj. R-squared: %.3f\n", summary(fit_pi)$r.squared, summary(fit_pi)$adj.r.squared))
  mod_results[[paste0("PI_", d)]] <- list(
    model = fit_pi,
    r2    = summary(fit_pi)$r.squared,
    adjr2 = summary(fit_pi)$adj.r.squared
  )
  
  # Knowledge × Demog → Purchase Behavior
  f_kb <- as.formula(paste("purchase_behavior_score ~ knowledge_score +", d, "+", know_var))
  fit_kb <- lm(f_kb, data = generic_df)
  cat(sprintf("\nModeration: Knowledge × %s → Purchase Behavior\n", d))
  print(summary(fit_kb)$coefficients)
  cat(sprintf("R-squared: %.3f | Adj. R-squared: %.3f\n", summary(fit_kb)$r.squared, summary(fit_kb)$adj.r.squared))
  mod_results[[paste0("KB_", d)]] <- list(
    model = fit_kb,
    r2    = summary(fit_kb)$r.squared,
    adjr2 = summary(fit_kb)$adj.r.squared
  )
  
  # Perception × Demog → Purchase Behavior
  f_pb <- as.formula(paste("purchase_behavior_score ~ perception_score +", d, "+", percep_var))
  fit_pb <- lm(f_pb, data = generic_df)
  cat(sprintf("\nModeration: Perception × %s → Purchase Behavior\n", d))
  print(summary(fit_pb)$coefficients)
  cat(sprintf("R-squared: %.3f | Adj. R-squared: %.3f\n", summary(fit_pb)$r.squared, summary(fit_pb)$adj.r.squared))
  mod_results[[paste0("PB_", d)]] <- list(
    model = fit_pb,
    r2    = summary(fit_pb)$r.squared,
    adjr2 = summary(fit_pb)$adj.r.squared
  )
}
# ----------------------------------------------------------------------------------

# 14. Convergent Validity (AVE & Composite Reliability) ---------------------------
loadings <- pls_out$outer_model
convergent_results <- do.call(rbind, lapply(c("Knowledge","Perception","Trust","PurchaseIntent","PurchaseBehavior"), function(lv) {
  lv_loads  <- loadings$loading[loadings$block == lv]
  ave       <- sum(lv_loads^2) / length(lv_loads)
  sum_loads <- sum(lv_loads)
  err_var   <- sum(1 - lv_loads^2)
  cr        <- sum_loads^2 / (sum_loads^2 + err_var)
  data.frame(Construct = lv, AVE = round(ave,3), CompositeReliability = round(cr,3))
}))
cat("\n>>> Convergent Validity (AVE & Composite Reliability):\n")
print(convergent_results)

# 15. Discriminant Validity ------------------------------------------------------
lv_scores <- pls_out$scores
ave_vals  <- setNames(convergent_results$AVE, convergent_results$Construct)
fl_matrix <- cor(lv_scores)
diag(fl_matrix) <- sqrt(ave_vals[colnames(fl_matrix)])
cat("\n>>> Fornell–Larcker Discriminant Validity (√AVE on diagonal):\n")
print(round(fl_matrix, 3))

# 15.2 Indicator Cross-Loadings --------------------------------------------------
crosslds <- cor(sem_data[, manifest_vars], lv_scores, use = "pairwise.complete.obs")
cat("\n>>> Indicator Cross-Loadings (items vs. LVs):\n")
print(round(crosslds, 3))

# 16. Average Full Collinearity VIF (AFVIF) ---------------------------------------
cat("\n>>> Average Full Collinearity VIF (AFVIF):", round(pls_out$gof["AFVIF"], 3), "\n")

# 17. Reliability Coefficients (Cronbach’s α & Composite Reliability) -------------
cronbach_df <- cronbach_df  # from step 12.4
reliability_coeffs <- merge(
  cronbach_df,
  convergent_results[, c("Construct","CompositeReliability")],
  by = "Construct"
)
reliability_coeffs$Alpha               <- round(reliability_coeffs$Alpha, 3)
reliability_coeffs$CompositeReliability <- round(reliability_coeffs$CompositeReliability, 3)
cat("\n>>> Reliability Coefficients per latent variable:\n")
print(reliability_coeffs)

# ───────────────────────────────────────────────────────────────────────────────
# 17A. Dillon–Goldstein’s Rho (ρA) [Fixed] ----------------------------------------
# Only for core constructs, not interaction blocks!
core_constructs <- c("Knowledge", "Perception", "Trust", "PurchaseIntent", "PurchaseBehavior")
rhoA_calc <- function(loads) {
  sum_loads <- sum(loads)
  error_var <- sum(1 - loads^2)
  rhoA <- sum_loads^2 / (sum_loads^2 + error_var)
  return(rhoA)
}
rhoA_results <- sapply(core_constructs, function(lv) {
  lv_loads <- loadings$loading[loadings$block == lv]
  if (length(lv_loads) == 0 || any(is.na(lv_loads))) return(NA)
  rhoA_calc(lv_loads)
})
rhoA_df <- data.frame(Construct = core_constructs, RhoA = round(rhoA_results, 3))
cat("\n>>> Dillon–Goldstein’s Rho (ρA) per Construct:\n")
print(rhoA_df)

# 17B. HTMT (Heterotrait–Monotrait Ratio) -----------------------------------------
htmt_calc <- function(blocks, data) {
  lvs <- names(blocks)
  htmt_mat <- matrix(NA, nrow=length(lvs), ncol=length(lvs), dimnames=list(lvs,lvs))
  for (i in seq_along(lvs)) {
    for (j in seq_along(lvs)) {
      if (i < j) {
        xi <- as.matrix(data[,blocks[[lvs[i]]]])
        xj <- as.matrix(data[,blocks[[lvs[j]]]])
        cor_cross <- abs(cor(xi, xj, use="pairwise.complete.obs"))
        cor_within_i <- abs(cor(xi, xi, use="pairwise.complete.obs"))
        cor_within_j <- abs(cor(xj, xj, use="pairwise.complete.obs"))
        cor_within_i <- cor_within_i[upper.tri(cor_within_i)]
        cor_within_j <- cor_within_j[upper.tri(cor_within_j)]
        htmt_val <- mean(cor_cross) / sqrt(mean(cor_within_i) * mean(cor_within_j))
        htmt_mat[i,j] <- htmt_val
      }
    }
  }
  return(round(htmt_mat,3))
}
cat("\n>>> HTMT Matrix (Heterotrait–Monotrait Ratio):\n")
htmt_matrix <- htmt_calc(blocks[c("Knowledge","Perception","Trust","PurchaseIntent","PurchaseBehavior")], sem_data)
print(htmt_matrix)

# 17C. Indicator (Factor) Loadings Table ------------------------------------------
cat("\n>>> Indicator (Factor) Loadings Table (Full):\n")
print(loadings[,c("block","name","loading")])

# 17D. VIFs for Multicollinearity (Indicator-Level) -------------------------------
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
library(car)
vif_results <- list()
for (lv in names(blocks)) {
  items <- blocks[[lv]]
  # Only check VIF if 2+ indicators
  if (length(items) > 1 && all(items %in% names(sem_data))) {
    dat <- sem_data[, items, drop=FALSE]
    # For each indicator in block, regress on others
    vifs <- sapply(names(dat), function(x) {
      others <- names(dat)[names(dat)!=x]
      if (length(others)==0) return(NA)
      model <- lm(dat[[x]] ~ ., data=dat[,others,drop=FALSE])
      car::vif(model)
    })
    vif_results[[lv]] <- vifs
  }
}
cat("\n>>> VIFs for Indicators per Construct:\n")
print(vif_results)

# 18. Research Question #3 and RQ3a Output ----------------------------------------
cat(sprintf("\nResearch Question #3: Level of Perception - Mean = %.2f, SD = %.2f\n",
            construct_stats$perception_mean, construct_stats$perception_sd))
cat("\nPerception Item-level Descriptives:\n")
perception_item_stats <- item_stats %>% filter(item %in% perception_cols)
print(perception_item_stats)

# End of script

# ───────────────────────────────────────────────────────────────────────────────
# Updated: PLS-SEM Results Diagram (semicolon-fixed)
# ───────────────────────────────────────────────────────────────────────────────

if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR", repos = "https://cloud.r-project.org")
}
library(DiagrammeR)
library(glue)

create_results_diagram <- function(pls_results = NULL,
                                   cronbach    = NULL,
                                   ave_vals    = NULL,
                                   path_coefs  = NULL) {
  
  # 1) Placeholder defaults
  r2    <- list(Trust = "0.xxx", PurchaseIntent = "0.xxx", PurchaseBehavior = "0.xxx")
  alpha <- list(Knowledge = "0.xxx", Perception = "0.xxx", Trust = "0.xxx")
  ave   <- list(Knowledge = "0.xxx", Perception = "0.xxx", Trust = "0.xxx")
  paths <- list(
    "Knowledge->Trust"            = "0.xxx***",
    "Perception->Trust"           = "0.xxx***",
    "Knowledge->PurchaseIntent"   = "0.xxx***",
    "Perception->PurchaseIntent"  = "0.xxx***",
    "Trust->PurchaseIntent"       = "0.xxx***",
    "Knowledge->PurchaseBehavior" = "0.xxx***",
    "Perception->PurchaseBehavior"= "0.xxx***",
    "Trust->PurchaseBehavior"     = "0.xxx***",
    "PurchaseIntent->PurchaseBehavior" = "0.xxx***"
  )
  
  # 2) Override with real results if provided
  if (!is.null(pls_results))   r2    <- pls_results$R2
  if (!is.null(cronbach))      alpha <- cronbach
  if (!is.null(ave_vals))      ave   <- ave_vals
  if (!is.null(path_coefs))    paths <- path_coefs
  
  # 3) Build DOT code (every statement ends with a semicolon!)
  dot <- glue("
  digraph results_plssem {{
    graph [layout=dot, rankdir=LR, bgcolor="white", fontname="Arial"];
    node  [shape=box, style="rounded,filled", fontname="Arial", fontsize=10];

    # Demographics note at top
    subgraph rank_source {{ rank=source; }};
    Demographics 
      [shape=note
       label=<<Demographic Moderators:<BR/>Gender, Age, Civil Status,<BR/>Education, Income<BR/>(See moderation results)>>
       fillcolor=white, fontcolor=black];

    # Exogenous constructs
    Knowledge 
      [fillcolor=lightgreen
       label=<<Knowledge<BR/>(α = {alpha$Knowledge})<BR/>(AVE = {ave$Knowledge})>>];
    Perception 
      [fillcolor=lightgreen
       label=<<Perception<BR/>(α = {alpha$Perception})<BR/>(AVE = {ave$Perception})>>];

    # Mediator
    Trust 
      [fillcolor=lightyellow
       label=<<Trust<BR/>(α = {alpha$Trust})<BR/>(AVE = {ave$Trust})<BR/>(R² = {r2$Trust})>>];

    # Endogenous constructs (black fill, white text)
    PurchaseIntent  
      [fillcolor=black, fontcolor=white
       label=<<Purchase Intent<BR/>(R² = {r2$PurchaseIntent})>>];
    PurchaseBehavior
      [fillcolor=black, fontcolor=white
       label=<<Purchase Behavior<BR/>(R² = {r2$PurchaseBehavior})>>];

    # Structural paths (solid, blue)
    Knowledge       -> Trust             [label="{paths$`Knowledge->Trust`}", color=blue];
    Perception      -> Trust             [label="{paths$`Perception->Trust`}", color=blue];
    Knowledge       -> PurchaseIntent    [label="{paths$`Knowledge->PurchaseIntent`}", color=blue];
    Perception      -> PurchaseIntent    [label="{paths$`Perception->PurchaseIntent`}", color=blue];
    Trust           -> PurchaseIntent    [label="{paths$`Trust->PurchaseIntent`}", color=blue];
    Knowledge       -> PurchaseBehavior  [label="{paths$`Knowledge->PurchaseBehavior`}", color=blue];
    Perception      -> PurchaseBehavior  [label="{paths$`Perception->PurchaseBehavior`}", color=blue];
    Trust           -> PurchaseBehavior  [label="{paths$`Trust->PurchaseBehavior`}", color=blue];
    PurchaseIntent  -> PurchaseBehavior  [label="{paths$`PurchaseIntent->PurchaseBehavior`}", color=blue];
  }}
  ")
  
  # 4) Render
  grViz(dot)
}

# Example (placeholder) render:
create_results_diagram()
