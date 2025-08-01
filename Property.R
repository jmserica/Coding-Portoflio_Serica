# ───────────────────────────────────────────────────────────────────────────────
# 1. Package Installation & Loading ------------------------------------------------
pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "plspm", "psych", "tibble")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) {
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# 2. Define File Path & Pre-flight Check ------------------------------------------
file_path <- file.path(Sys.getenv("HOME"), "Desktop", "Property.xlsx")
if (!file.exists(file_path)) {
  stop("File not found at: ", file_path)
}

# 3. Import Excel Data -------------------------------------------------------------
card_df <- readxl::read_excel(path = file_path, sheet = 1, col_names = TRUE)

# 4. Trim whitespace in all character columns -------------------------------------
card_df <- card_df %>% mutate(across(where(is.character), str_trim))

# 5. Classify Age Brackets & Recode Other Demographics ----------------------------
card_df <- card_df %>% mutate(
  age_code = case_when(
    Age == "18-20"        ~ 1,
    Age == "21-25"        ~ 2,
    Age == "26-30"        ~ 3,
    Age == "31-35"        ~ 4,
    Age == "35-40"        ~ 5,
    Age == "41-45"        ~ 6,
    Age == "46-50"        ~ 7,
    Age == "51-55"        ~ 8,
    Age == "56-60"        ~ 9,
    Age == "61 and above" ~ 10,
    TRUE                  ~ NA_real_
  ),
  sex_code = case_when(
    Gender == "Male"   ~ 1,
    Gender == "Female" ~ 2,
    TRUE               ~ NA_real_
  ),
  civil_status_code = case_when(
    `Civil Status` == "Single"    ~ 1,
    `Civil Status` == "Married"   ~ 2,
    `Civil Status` == "Separated" ~ 3,
    `Civil Status` == "Widowed"   ~ 4,
    TRUE                         ~ NA_real_
  ),
  education_code = case_when(
    `Educational Attainment` == "Elementary Graduate"                  ~ 1,
    `Educational Attainment` == "High School Graduate"                 ~ 2,
    `Educational Attainment` == "Vocational/Technical Course Graduate" ~ 3,
    `Educational Attainment` == "College Graduate"                     ~ 4,
    `Educational Attainment` == "Master’s Degree Holder"               ~ 5,
    `Educational Attainment` == "Doctoral Degree Holder"               ~ 6,
    TRUE                                                              ~ NA_real_
  ),
  income_code = case_when(
    `Monthly Household Income` == "Below ₱20,000"       ~ 1,
    `Monthly Household Income` == "₱20,000 – ₱50,000"   ~ 2,
    `Monthly Household Income` == "₱50,000 – ₱100,000"  ~ 3,
    `Monthly Household Income` == "₱100,000 – ₱200,000" ~ 4,
    `Monthly Household Income` == "₱200,000 – ₱500,000" ~ 5,
    `Monthly Household Income` == "Above ₱500,000"      ~ 6,
    TRUE                                               ~ NA_real_
  ),
  residence_code = case_when(
    `Place of Residence` == "Metro Manila" ~ 1,
    `Place of Residence` == "Luzon"        ~ 2,
    `Place of Residence` == "Visayas"      ~ 3,
    `Place of Residence` == "Mindanao"     ~ 4,
    TRUE                                  ~ NA_real_
  )
)

# 6. Clean / normalize column names -----------------------------------------------
card_df <- card_df %>%
  rename_with(~ str_squish(str_replace_all(., "[\r\n]", " ")))

# 7. Locate the survey items by construct -----------------------------------------
# 7.1 Mobile marketing items
mobile_cols      <- names(card_df) %>% str_subset("^I am highly interested")
price_cols       <- names(card_df) %>% str_subset("^Price offers and financing")
notify_cols      <- names(card_df) %>% str_subset("^I find it helpful when real estate websites send")
overall_cols     <- names(card_df) %>% str_subset("^Overall, receiving new property listings")
mobile_marketing_cols <- c(mobile_cols, price_cols, notify_cols, overall_cols)
# 7.2 Content marketing items
content_cols     <- names(card_df) %>% str_subset("^The content provided about real estate properties")
search_cols      <- names(card_df) %>% str_subset("^It is convenient to search for information")
reviews_cols     <- names(card_df) %>% str_subset("^I refer to YouTube or other online reviews")
blog_cols        <- names(card_df) %>% str_subset("^I click on links provided in real estate blogs")
content_marketing_cols <- c(content_cols, search_cols, reviews_cols, blog_cols)
# 7.3 Social media items
social_active    <- names(card_df) %>% str_subset("^I am an active user on social media platforms")
social_explore   <- names(card_df) %>% str_subset("^I am used to exploring real estate properties on social media")
social_clicks    <- names(card_df) %>% str_subset("^I usually click on advertisements related to real estate")
social_visit     <- names(card_df) %>% str_subset("^I always visit the websites of online real estate agents")
social_media_cols <- c(social_active, social_explore, social_clicks, social_visit)
# 7.4 Email marketing items
email_read       <- names(card_df) %>% str_subset("^I read emails sent by real estate agents online")
email_interest   <- names(card_df) %>% str_subset("^I take interest in e-mail promotions")
email_links      <- names(card_df) %>% str_subset("^I visit links sent by real estate agents sent through email")
email_effect     <- names(card_df) %>% str_subset("^E-mails from real estate agents sometimes make me buy")
email_marketing_cols <- c(email_read, email_interest, email_links, email_effect)
# 7.5 Consumer buying behavior items
buying_broad     <- names(card_df) %>% str_subset("^Selection of real estate properties on the internet is very broad")
buying_time      <- names(card_df) %>% str_subset("^Selecting and buying real estate properties on the internet saves time")
buying_future    <- names(card_df) %>% str_subset("^I intend to continue buying real estate properties online")
buying_discounts <- names(card_df) %>% str_subset("^Buying real estate properties online offers greater discounts")
buying_convenient<- names(card_df) %>% str_subset("^It's convenient to buy real estate online")
consumer_buying_cols <- c(buying_broad, buying_time, buying_future, buying_discounts, buying_convenient)
# 7.6 Customer engagement items
engage_browse    <- names(card_df) %>% str_subset("^I often browse advertisements related to real estate")
engage_read      <- names(card_df) %>% str_subset("^I often read real estate advertisement posts")
engage_like      <- names(card_df) %>% str_subset("^I often use the “like” option on real estate advertisements")
engage_comment   <- names(card_df) %>% str_subset("^I often comment on real estate advertisement posts")
engage_share     <- names(card_df) %>% str_subset("^I often share real estate advertisement posts")
customer_engagement_cols <- c(engage_browse, engage_read, engage_like, engage_comment, engage_share)

# 8. Cast survey items to numeric (_num) ------------------------------------------
all_dmi_items <- c(
  mobile_marketing_cols,
  content_marketing_cols,
  social_media_cols,
  email_marketing_cols,
  consumer_buying_cols,
  customer_engagement_cols
)
card_df <- card_df %>%
  mutate(across(all_of(all_dmi_items), as.numeric, .names = "{.col}_num"))

# 8.1 Define "_cols_num" for each construct
mobile_marketing_cols_num    <- paste0(mobile_marketing_cols,    "_num")
content_marketing_cols_num   <- paste0(content_marketing_cols,   "_num")
social_media_cols_num        <- paste0(social_media_cols,        "_num")
email_marketing_cols_num     <- paste0(email_marketing_cols,     "_num")
consumer_buying_cols_num     <- paste0(consumer_buying_cols,     "_num")
customer_engagement_cols_num <- paste0(customer_engagement_cols, "_num")

# 9. Question-Level Averages ------------------------------------------------------
question_cols <- unlist(list(
  mobile_marketing_cols_num,
  content_marketing_cols_num,
  social_media_cols_num,
  email_marketing_cols_num,
  consumer_buying_cols_num,
  customer_engagement_cols_num
))
question_avgs <- card_df %>%
  summarize(across(all_of(question_cols), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Average") %>%
  arrange(Question)
cat(">>> Average Score per Question:\n")
print(question_avgs, n = nrow(question_avgs))

# 10. Compute each construct’s mean score -----------------------------------------
card_df <- card_df %>%
  rowwise() %>%
  mutate(
    mobile_marketing_score    = mean(c_across(all_of(mobile_marketing_cols_num)),    na.rm = TRUE),
    content_marketing_score   = mean(c_across(all_of(content_marketing_cols_num)),   na.rm = TRUE),
    social_media_score        = mean(c_across(all_of(social_media_cols_num)),        na.rm = TRUE),
    email_marketing_score     = mean(c_across(all_of(email_marketing_cols_num)),     na.rm = TRUE),
    consumer_buying_score     = mean(c_across(all_of(consumer_buying_cols_num)),     na.rm = TRUE),
    customer_engagement_score = mean(c_across(all_of(customer_engagement_cols_num)), na.rm = TRUE)
  ) %>%
  ungroup()

# 11. Descriptive Statistics ------------------------------------------------------
construct_stats <- card_df %>% summarize(
  mobile_marketing_mean       = mean(mobile_marketing_score,       na.rm = TRUE),
  mobile_marketing_sd         = sd(  mobile_marketing_score,       na.rm = TRUE),
  content_marketing_mean      = mean(content_marketing_score,      na.rm = TRUE),
  content_marketing_sd        = sd(  content_marketing_score,      na.rm = TRUE),
  social_media_mean           = mean(social_media_score,           na.rm = TRUE),
  social_media_sd             = sd(  social_media_score,           na.rm = TRUE),
  email_marketing_mean        = mean(email_marketing_score,        na.rm = TRUE),
  email_marketing_sd          = sd(  email_marketing_score,        na.rm = TRUE),
  consumer_buying_mean        = mean(consumer_buying_score,        na.rm = TRUE),
  consumer_buying_sd          = sd(  consumer_buying_score,        na.rm = TRUE),
  customer_engagement_mean    = mean(customer_engagement_score,    na.rm = TRUE),
  customer_engagement_sd      = sd(  customer_engagement_score,    na.rm = TRUE)
)
cat("\n>>> Construct-level Descriptives:\n")
print(construct_stats)

# 12. Item-level stats ------------------------------------------------------------
item_cols <- question_cols
item_stats <- card_df %>%
  select(all_of(item_cols)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "value") %>%
  group_by(item) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  )
cat("\n>>> Item-level Descriptives:\n")
print(item_stats, n = nrow(item_stats))

# 13. Cronbach’s α per construct --------------------------------------------------
reliability_df <- card_df %>%
  select(all_of(item_cols)) %>%
  na.omit()

cronbach_results <- list(
  MobileMarketing    = psych::alpha(reliability_df[, mobile_marketing_cols_num])$total$raw_alpha,
  ContentMarketing   = psych::alpha(reliability_df[, content_marketing_cols_num])$total$raw_alpha,
  SocialMedia        = psych::alpha(reliability_df[, social_media_cols_num])$total$raw_alpha,
  EmailMarketing     = psych::alpha(reliability_df[, email_marketing_cols_num])$total$raw_alpha,
  ConsumerBuying     = psych::alpha(reliability_df[, consumer_buying_cols_num])$total$raw_alpha,
  CustomerEngagement = psych::alpha(reliability_df[, customer_engagement_cols_num])$total$raw_alpha
)
cronbach_df <- data.frame(
  Construct = names(cronbach_results),
  Alpha     = unlist(cronbach_results)
)
cat("\n>>> Cronbach’s α per construct:\n")
print(cronbach_df)

# 14. PLS‐PM Specification & Estimation (H1a–d, H2, H3, H4a–d) --------------------
sem_data <- card_df %>% select(all_of(c(
  social_media_cols_num,
  email_marketing_cols_num,
  content_marketing_cols_num,
  mobile_marketing_cols_num,
  customer_engagement_cols_num,
  consumer_buying_cols_num
)))
blocks <- list(
  SocialMedia       = social_media_cols_num,
  EmailMarketing    = email_marketing_cols_num,
  ContentMarketing  = content_marketing_cols_num,
  MobileMarketing   = mobile_marketing_cols_num,
  CustomerEngagement = customer_engagement_cols_num,
  ConsumerBuying     = consumer_buying_cols_num
)
nblks <- length(blocks)
inner_model <- matrix(0, nblks, nblks,
                      dimnames = list(names(blocks), names(blocks)))
inner_model["CustomerEngagement", c("SocialMedia","EmailMarketing","ContentMarketing","MobileMarketing")] <- 1
inner_model["ConsumerBuying", "CustomerEngagement"] <- 1
inner_model["ConsumerBuying", c("SocialMedia","EmailMarketing","ContentMarketing","MobileMarketing")] <- 1
modes <- rep("A", nblks)
pls_out <- plspm(sem_data, inner_model, blocks, modes, scaled = TRUE, boot.val = TRUE, br = 5000)

# >>> R-squared for PLS-SEM endogenous constructs <<<
cat("\n>>> PLS-SEM R-squared (Endogenous Constructs):\n")
pls_r2_vec <- pls_out$R2
pls_r2 <- data.frame(
  Construct = names(pls_r2_vec),
  R2 = round(as.numeric(pls_r2_vec), 3),
  row.names = NULL,
  stringsAsFactors = FALSE
)
print(pls_r2, row.names = FALSE)


# 15. Direct Effects (H1a–d, H2, H3) & p‐values -----------------------------------
boot_paths <- pls_out$boot$paths %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Path") %>%
  filter(Original != 0)
coef_tbl <- boot_paths %>%
  mutate(
    Beta      = round(Original,   3),
    SE        = round(Std.Error,  3),
    z         = round(Original / Std.Error, 3),
    p.value   = round(2 * pnorm(-abs(Original / Std.Error)), 4),
    Supported = ifelse(p.value < 0.05, "Yes", "No")
  ) %>%
  select(Path, Beta, SE, z, p.value, Supported)
cat("\n>>> H1a–d, H2 & H3 Path Coefficients & Significance:\n")
print(coef_tbl, row.names = FALSE)

# 16. Indirect & Total Effects (Mediation H4a–d) ----------------------------------
effs <- as.data.frame(pls_out$effects) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n>>> Mediation Effects (H4a–d, indirect & total):\n")
print(effs)

# 17. Convergent Validity (AVE & Composite Reliability) ---------------------------
loadings <- pls_out$outer_model
constructs <- names(blocks)
convergent_results <- do.call(rbind, lapply(constructs, function(lv) {
  lv_loads <- loadings$loading[loadings$block == lv]
  ave      <- sum(lv_loads^2) / length(lv_loads)
  sum_loads<- sum(lv_loads)
  error_var<- sum(1 - lv_loads^2)
  cr       <- sum_loads^2 / (sum_loads^2 + error_var)
  data.frame(Construct = lv, AVE = ave, CompositeReliability = cr)
}))
cat("\n>>> Convergent Validity (AVE & Composite Reliability):\n")
print(convergent_results)

# --- Dillon–Goldstein’s Rho-A (ρA) ----------------------------------------------
rhoa_fun <- function(df) {
  eig <- eigen(cor(df, use = "pairwise.complete.obs"), only.values = TRUE)$values
  # first‐eigenvalue divided by total variance
  eig[1] / sum(eig)
}

rhoa_results <- sapply(blocks, function(x) 
  tryCatch(rhoa_fun(sem_data[, x]), error = function(e) NA)
)

rhoa_tbl <- tibble::tibble(
  Construct = names(blocks),
  RhoA      = round(rhoa_results, 3)
)

cat("\n>>> Dillon–Goldstein’s Rho-A (ρA) per construct:\n")
print(rhoa_tbl)

# 17. Indicator (Factor) Loadings -------------------------------------------------
indicator_loadings_tbl <- pls_out$outer_model[, c("block", "name", "loading")]
colnames(indicator_loadings_tbl) <- c("Construct", "Indicator", "Loading")
indicator_loadings_tbl <- indicator_loadings_tbl[order(indicator_loadings_tbl$Construct, -abs(indicator_loadings_tbl$Loading)), ]
cat("\n>>> Indicator (Factor) Loadings per construct:\n")
print(indicator_loadings_tbl, row.names = FALSE)

# 18. Convergent Validity (AVE & Composite Reliability) ---------------------------
loadings <- pls_out$outer_model
constructs <- names(blocks)
convergent_results <- do.call(rbind, lapply(constructs, function(lv) {
  lv_loads <- loadings$loading[loadings$block == lv]
  ave      <- sum(lv_loads^2) / length(lv_loads)
  sum_loads<- sum(lv_loads)
  error_var<- sum(1 - lv_loads^2)
  cr       <- sum_loads^2 / (sum_loads^2 + error_var)
  data.frame(Construct = lv, AVE = ave, CompositeReliability = cr)
}))
cat("\n>>> Convergent Validity (AVE & Composite Reliability):\n")
print(convergent_results)


# 18. Discriminant Validity -------------------------------------------------------
lv_scores <- pls_out$scores
ave_vals  <- setNames(convergent_results$AVE, convergent_results$Construct)
corr_lv   <- cor(lv_scores)
fl_matrix <- corr_lv
diag(fl_matrix) <- sqrt(ave_vals[colnames(fl_matrix)])
cat("\n>>> Fornell–Larcker Discriminant Validity (√AVE on diagonal):\n")
print(round(fl_matrix, 3))

# 18.2 Indicator Cross-Loadings ---------------------------------------------------
manifests <- unlist(blocks)
crosslds <- cor(sem_data[, manifests], lv_scores, use = "pairwise.complete.obs")
cat("\n>>> Indicator Cross-Loadings (each item vs. all LVs):\n")
print(round(crosslds, 3))

# 19. Average Full Collinearity VIF (AFVIF) ---------------------------------------
afvif <- pls_out$gof["AFVIF"]
cat("\n>>> Average Full Collinearity VIF (AFVIF):", round(afvif, 3), "\n")

# --- Heterotrait–Monotrait Ratio (HTMT) -----------------------------------------
htmt_matrix <- function(data, blocks) {
  res <- matrix(NA, nrow = length(blocks), ncol = length(blocks),
                dimnames = list(names(blocks), names(blocks)))
  for (i in seq_along(blocks)) for (j in seq_along(blocks)) {
    if (i == j) next
    X <- as.matrix(data[, blocks[[i]]])
    Y <- as.matrix(data[, blocks[[j]]])
    vals <- c()
    for (x in 1:ncol(X)) for (y in 1:ncol(Y)) {
      vals <- c(vals, abs(cor(X[, x], Y[, y], use = "pairwise.complete.obs")))
    }
    res[i, j] <- mean(vals, na.rm = TRUE)
  }
  res
}
htmt_out <- htmt_matrix(sem_data, blocks)
cat("\n>>> Heterotrait–Monotrait Ratio (HTMT) Matrix:\n")
print(round(htmt_out, 3))

# --- 19. Variance Inflation Factor (VIF) per Construct -------------------------
# Build composite scores for each latent construct
construct_scores <- as.data.frame(
  lapply(blocks, function(items) {
    rowMeans(sem_data[, items, drop = FALSE], na.rm = TRUE)
  }),
  stringsAsFactors = FALSE
)
names(construct_scores) <- names(blocks)

# VIF function for constructs
construct_vif_fun <- function(data, constructs) {
  vifs <- sapply(constructs, function(var) {
    others <- setdiff(constructs, var)
    frm <- as.formula(paste0("`", var, "` ~ ", paste0("`", others, "`", collapse = " + ")))
    mdl <- lm(frm, data = data)
    1 / (1 - summary(mdl)$r.squared)
  })
  tibble::tibble(
    Construct = constructs,
    VIF       = round(as.numeric(vifs), 3)
  )
}

cat("\n>>> VIF per Construct:\n")
construct_vif <- construct_vif_fun(construct_scores, names(blocks))
print(construct_vif)

# Average Full VIF (AFVIF)
afvif <- round(mean(construct_vif$VIF, na.rm = TRUE), 3)
cat("\n>>> AFVIF (average VIF across constructs):", afvif, "\n")

# 21. Moderation Check by Demographics (H5: Digital marketing × Demographics → ConsumerBuying) -----

# 21a. Prepare interaction terms for moderation -----------------------------------
card_df <- card_df %>%
  mutate(
    # Age interactions
    sm_age    = social_media_score    * age_code,
    email_age = email_marketing_score * age_code,
    cm_age    = content_marketing_score * age_code,
    mm_age    = mobile_marketing_score  * age_code,
    # Sex interactions
    sm_sex    = social_media_score    * sex_code,
    email_sex = email_marketing_score * sex_code,
    cm_sex    = content_marketing_score * sex_code,
    mm_sex    = mobile_marketing_score  * sex_code,
    # Civil status interactions
    sm_civil    = social_media_score    * civil_status_code,
    email_civil = email_marketing_score * civil_status_code,
    cm_civil    = content_marketing_score * civil_status_code,
    mm_civil    = mobile_marketing_score  * civil_status_code,
    # Education interactions
    sm_edu    = social_media_score    * education_code,
    email_edu = email_marketing_score * education_code,
    cm_edu    = content_marketing_score * education_code,
    mm_edu    = mobile_marketing_score  * education_code,
    # Income interactions
    sm_income    = social_media_score    * income_code,
    email_income = email_marketing_score * income_code,
    cm_income    = content_marketing_score * income_code,
    mm_income    = mobile_marketing_score  * income_code,
    # Residence interactions
    sm_res    = social_media_score    * residence_code,
    email_res = email_marketing_score * residence_code,
    cm_res    = content_marketing_score * residence_code,
    mm_res    = mobile_marketing_score  * residence_code
  )

# 21b. Helper to run moderation regressions WITH R-SQUARE --------------------------
run_mod <- function(formula, df, label) {
  sub <- df %>% select(all.vars(formula)) %>% na.omit()
  cat("\n>>>", label, "\n")
  if (nrow(sub) == 0) {
    cat("  ▶ Skipped:", label, "- no complete cases.\n")
  } else {
    lm_fit <- lm(formula, data = sub)
    print(coef(summary(lm_fit)))
    r2 <- summary(lm_fit)$r.squared
    adjr2 <- summary(lm_fit)$adj.r.squared
    cat(sprintf("  ▶ R-squared: %.3f\n", r2))
    cat(sprintf("  ▶ Adjusted R-squared: %.3f\n", adjr2))
  }
}

# 21c. Execute H5 regressions (ALL moderation checks) -----------------------------

# Age moderation (H5a–H5d)
run_mod(consumer_buying_score ~ social_media_score + age_code + sm_age, card_df, "H5a: SocialMedia × Age → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + age_code + email_age, card_df, "H5b: EmailMarketing × Age → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + age_code + cm_age, card_df, "H5c: ContentMarketing × Age → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + age_code + mm_age, card_df, "H5d: MobileMarketing × Age → ConsumerBuying")

# Sex moderation (H5e–H5h)
run_mod(consumer_buying_score ~ social_media_score + sex_code + sm_sex, card_df, "H5e: SocialMedia × Sex → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + sex_code + email_sex, card_df, "H5f: EmailMarketing × Sex → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + sex_code + cm_sex, card_df, "H5g: ContentMarketing × Sex → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + sex_code + mm_sex, card_df, "H5h: MobileMarketing × Sex → ConsumerBuying")

# Civil status moderation (H5i–H5l)
run_mod(consumer_buying_score ~ social_media_score + civil_status_code + sm_civil, card_df, "H5i: SocialMedia × CivilStatus → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + civil_status_code + email_civil, card_df, "H5j: EmailMarketing × CivilStatus → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + civil_status_code + cm_civil, card_df, "H5k: ContentMarketing × CivilStatus → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + civil_status_code + mm_civil, card_df, "H5l: MobileMarketing × CivilStatus → ConsumerBuying")

# Education moderation (H5m–H5p)
run_mod(consumer_buying_score ~ social_media_score + education_code + sm_edu, card_df, "H5m: SocialMedia × Education → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + education_code + email_edu, card_df, "H5n: EmailMarketing × Education → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + education_code + cm_edu, card_df, "H5o: ContentMarketing × Education → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + education_code + mm_edu, card_df, "H5p: MobileMarketing × Education → ConsumerBuying")

# Income moderation (H5q–H5t)
run_mod(consumer_buying_score ~ social_media_score + income_code + sm_income, card_df, "H5q: SocialMedia × Income → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + income_code + email_income, card_df, "H5r: EmailMarketing × Income → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + income_code + cm_income, card_df, "H5s: ContentMarketing × Income → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + income_code + mm_income, card_df, "H5t: MobileMarketing × Income → ConsumerBuying")

# Residence moderation (H5u–H5x)
run_mod(consumer_buying_score ~ social_media_score + residence_code + sm_res, card_df, "H5u: SocialMedia × Residence → ConsumerBuying")
run_mod(consumer_buying_score ~ email_marketing_score + residence_code + email_res, card_df, "H5v: EmailMarketing × Residence → ConsumerBuying")
run_mod(consumer_buying_score ~ content_marketing_score + residence_code + cm_res, card_df, "H5w: ContentMarketing × Residence → ConsumerBuying")
run_mod(consumer_buying_score ~ mobile_marketing_score + residence_code + mm_res, card_df, "H5x: MobileMarketing × Residence → ConsumerBuying")
