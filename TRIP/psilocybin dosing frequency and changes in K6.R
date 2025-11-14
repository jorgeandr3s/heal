# Load required libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(lme4)
library(ggplot2)

# Load data
data_merged <- read_csv("TRIP/merged_data.csv")

# Apply to selected columns
dose_vars <- c("past12m_microdose_count",
               "past12m_light_dose_count",
               "past12m_medium_dose_count",
               "past12m_strong_dose_count",
               "past12m_hero_dose_count",
               "past12m_unknown_dose_count")


convert_dose_count <- function(x) {
  recode(x,
         "0 (Not in the past twelve months)" = 0,
         "1 time" = 1, "2 times" = 2, "3 times" = 3, "4 times" = 4,
         "5 times" = 5, "6 times" = 6, "7 times" = 7, "8 times" = 8,
         "9 times" = 9, "10 times" = 10, "11 times" = 11, "12 times" = 12,
         "14 times" = 14, "15 times" = 15, "17 times" = 17, "18 times" = 18,
         "20 times" = 20, "21 times" = 21, "25 times" = 25, "26 times" = 26,
         "29 times" = 29, "30 times" = 30, "32 times" = 32, "36 times" = 36,
         "40 times" = 40, "42 times" = 42, "45 times" = 45, "47 times" = 47,
         "50 or more times" = 50,
         .default = 0)
}

k6_items <- c("k6_nervous", "k6_hopeless", "k6_restless", "k6_depressed", "k6_effort", "k6_worthless")

convert_k6_response <- function(x) {
  recode(x,
         "None of the time" = 0,
         "A little of the time" = 1,
         "Some of the time" = 2,
         "Most of the time" = 3,
         "All of the time" = 4,
         .default = 0)
}

data_merged <- data_merged %>%
  #mutate(across(all_of(dose_vars), ~str_extract(.,"\\d+"))) %>% # alternative
  mutate(across(all_of(dose_vars), ~convert_dose_count(.))) %>%
  mutate(across(all_of(k6_items), ~convert_k6_response(.)))

data_merged$past12m_any_count <- rowSums(data_merged[, dose_vars], na.rm = TRUE)

# Apply to selected columns
dose_vars <- c("past12m_microdose_count",
               "past12m_light_dose_count",
               "past12m_medium_dose_count",
               "past12m_strong_dose_count",
               "past12m_hero_dose_count",
               "past12m_unknown_dose_count",
               "past12m_any_count")


data_merged <- data_merged %>%
  mutate(across(all_of(dose_vars), ~round(ifelse(. > quantile(., 0.99, na.rm = TRUE),
                                                 quantile(., 0.99, na.rm = TRUE),
                                                 .))))

data_merged$k6_score <- rowSums(data_merged[, c("k6_nervous", "k6_hopeless", "k6_restless", "k6_depressed", "k6_effort", "k6_worthless")], na.rm = TRUE)

# Ensure email_address is a factor
data_merged$email_address <- as.factor(data_merged$email_address)

#################################################################################################### 
#Dosing Descriptives
#################################################################################################### 
# Loop over all dose variables and print descriptives
for (var in dose_vars) {
  cat("\n==========================================\n")
  cat("Descriptive statistics for:", var, "\n")
  cat("==========================================\n")
  
  # Summary
  print(summary(data_merged[[var]]))
  
  # Standard deviation
  cat("Standard Deviation:", sd(data_merged[[var]], na.rm = TRUE), "\n")
  
  # Frequency table
  print(table(data_merged[[var]], useNA = "ifany"))
}

#################################################################################################### 
#Histograms for each dose-variable
#################################################################################################### 
# Create plot
# Reshape and factor
data_long <- data_merged %>%
  dplyr::select(email_address, k6_score, all_of(dose_vars)) %>%
  pivot_longer(cols = all_of(dose_vars), 
               names_to = "dose_type", 
               values_to = "dose_frequency") %>%
  mutate(dose_type = factor(dose_type, levels = dose_vars)) %>%
  drop_na(k6_score, dose_frequency)

################################################################################
# (,")
data_merged$survey_year <- substr(data_merged$end_date,1,4)

library(plm)

# Convert to pdata.frame
pdata <- pdata.frame(data_merged, index = c("email_address", "survey_year"))

variance_results <- data.frame(dose=NA,
                               total_variance=NA,
                               between_person=NA,
                               between_time=NA)


                          
for (i in dose_vars){
  variance_results[which(dose_vars==i),] <- 
    c(i, 
      summary(pdata[[i]])[1],
      summary(pdata[[i]])[2],
      summary(pdata[[i]])[3])
  }

variance_results <- variance_results %>% mutate(across(2:4, ~ as.numeric(.)))
variance_results$between_person_prop <- variance_results$between_person / variance_results$total_variance
variance_results$between_time_prop <- variance_results$between_time / variance_results$total_variance


data_merged <- data_merged %>% mutate(
  max_dose=factor(case_when(
    !is.na(past12m_unknown_dose_count) & past12m_unknown_dose_count >= 1 ~ NA,
    past12m_hero_dose_count>0 ~ "Hero",
    past12m_strong_dose_count>0 ~ "Strong",
    past12m_medium_dose_count>0 ~ "Medium",
    past12m_light_dose_count>0 ~ "Light",
    past12m_microdose_count>0 ~ "Microdose",
    TRUE ~ "None"
  ), levels = c("None","Microdose", "Light", "Medium", "Strong", "Hero"), ordered = T)
)

emails_with_unknown_doses <- data_merged %>% filter(is.na(max_dose)) %>% 
  select(email_address) %>% droplevels.data.frame(.) %>%
  unique() %>% unlist() %>% as.character()

emails_with_known_doses <- as.character(unique(data_merged$email_address))[!as.character(unique(data_merged$email_address)) %in% emails_with_unknown_doses]

library(ggridges)


ggplot(data=data_merged %>% filter(email_address %in% emails_with_known_doses[1:56]), 
       aes(x = factor(survey_year, levels = c("2023","2024","2025")), 
                      y = max_dose, group = email_address)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "black") +
  facet_wrap(~ email_address) +
  theme_minimal() 

ggplot(data=data_merged %>% filter(email_address %in% emails_with_known_doses[57:112]), 
       aes(x = factor(survey_year, levels = c("2023","2024","2025")), 
           y = max_dose, group = email_address)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "black") +
  facet_wrap(~ email_address) +
  theme_minimal() 

ggplot(data=data_merged %>% filter(email_address %in% emails_with_known_doses[113:168]), 
       aes(x = factor(survey_year, levels = c("2023","2024","2025")), 
           y = max_dose, group = email_address)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "black") +
  facet_wrap(~ email_address) +
  theme_minimal() 

workable_doses <- data_merged %>% filter(email_address %in% emails_with_known_doses) %>% 
  select(email_address, survey_year,max_dose)

is_lodes_form(workable_doses,
              key = "survey_year", value = "max_dose", id = "email_address")

max_dose_alluvia <- to_alluvia_form(workable_doses,
                                    key = "survey_year", value = "max_dose", id = "email_address")
names(max_dose_alluvia) <- c("ID","Year_1","Year_2","Year_3")
max_dose_alluvia$sample <- factor("TRIP participants")
gg <- ggplot(max_dose_alluvia,
             aes(axis1 = Year_1, axis2 = Year_2, axis3 = Year_3))

gg +
  geom_alluvium(aes(fill = sample), width = 2/5, discern = TRUE) +
  geom_stratum(width = 2/5, discern = TRUE) +
  geom_text(stat = "stratum", discern = TRUE, aes(label = after_stat(stratum)))

ggplot(workable_doses %>% group_by(survey_year, max_dose) %>%
         arrange(max_dose) %>%  ungroup(),
       aes(x = survey_year,
           stratum = max_dose,
           alluvium = email_address,
           fill = max_dose,
           label = max_dose,
           order = max_dose)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback", color = "darkgray") +
  geom_stratum() +
  ggtitle("Changes in maximum dose by year")



# (",)
################################################################################


# Load libraries
library(cowplot)
library(ggplot2)

# Custom blue gradient fill (light to dark)
dose_fill_colors <- c(
  "past12m_microdose_count" = "#c6dbef",
  "past12m_light_dose_count" = "#9ecae1",
  "past12m_medium_dose_count" = "#6baed6",
  "past12m_strong_dose_count" = "#3182bd",
  "past12m_hero_dose_count" = "#08519c",
  "past12m_unknown_dose_count" = "#969696",
  "past12m_any_count" = "#fb6a4a"
)

# Get maximum value across all dose variables for shared x-axis
max_x <- max(unlist(data_merged[dose_vars]), na.rm = TRUE)

# Create histograms with fixed x-axis and variable y-axis
dose_histograms <- lapply(dose_vars, function(var) {
  ggplot(data_merged, aes(x = var)) +
    geom_histogram(binwidth = 1, fill = dose_fill_colors[[var]], color = "black") +
    labs(
      title = var,
      x = "Number of Uses (Capped at 99th Percentile)",
      y = "Count"
    ) +
    coord_cartesian(xlim = c(0, max_x)) +  # Fix x-axis only
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
})

# Combine and display plots
cowplot::plot_grid(plotlist = dose_histograms, ncol = 2, align = "h")



################################################ Aim 1################################################ 
#Scatterplot for K6, by Frequency of Dosings
################################################ Aim 1################################################ 

# Plot
# Define custom color palette
custom_colors <- c(
  "past12m_microdose_count" = "#c6dbef",   # light blue
  "past12m_light_dose_count"     = "#9ecae1",
  "past12m_medium_dose_count"    = "#6baed6",
  "past12m_strong_dose_count"    = "#3182bd",   # dark blue
  "past12m_hero_dose_count"      = "#08519c",   # very dark blue
  "past12m_unknown_dose_count"   = "#969696",   # gray
  "past12m_any_count"   = "#fb6a4a"    # reddish for contrast
)

# Full plot with custom colors
main_plot <- ggplot(data_long, aes(x = dose_frequency, y = k6_score, color = dose_type)) +
  geom_jitter(alpha = 0.05, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Relationship Between Psilocybin Dose Frequency and Psychological Distress",
    x = "Frequency of Use (Past 12 Months)",
    y = "K6 Score (Distress)",
    color = "Dose Type",
    caption = "lm smoothed curves for each dose type"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    plot.margin = margin(10, 10, 30, 10)
  )

# Zoomed-in inset with white background and dotted line at y = 7.5
zoom_plot <- ggplot(data_long, aes(x = dose_frequency, y = k6_score, color = dose_type)) +
  geom_jitter(alpha = 0.05, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = 7.5, linetype = "dashed", color = "red", linewidth = 1) +
  scale_color_manual(values = custom_colors) +
  coord_cartesian(xlim = c(0, 10)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(x = NULL, y = NULL, title = NULL, caption = NULL)

# Combine main and zoomed plots
combined_plot <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(zoom_plot, x = 0.5, y = 0.50, width = 0.45, height = 0.45)

# Display combined plot
print(combined_plot)

#################################################################################################### 
# Modelling
#################################################################################################### 

# Load required packages
library(pscl)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(broom.mixed)
library(broom)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Function to check overdispersion (variance/mean ratio)
check_overdispersion <- function(var) {
  mean_val <- mean(var, na.rm = TRUE)
  var_val <- var(var, na.rm = TRUE)
  return(var_val / mean_val)
}

# Function to fit LMM, ZIP, ZINB and extract summary stats + Vuong test
fit_models <- function(var, data_merged) {
  df <- data_merged %>% filter(!is.na(.data[[var]]), !is.na(k6_score))
  odr <- check_overdispersion(df[[var]])
  
  # Linear Mixed Model
  lmm_formula <- as.formula(paste("k6_score ~", var, "+ (1 | email_address)"))
  lmm <- lmer(lmm_formula, data = df)
  lmm_tidy <- tidy(lmm) %>%
    filter(term == var) %>%
    mutate(model = "LMM")
  
  # ZIP model
  zip_model <- tryCatch(
    zeroinfl(as.formula(paste(var, "~ k6_score")), data = df, dist = "poisson"),
    error = function(e) NULL
  )
  
  zip_tidy <- if (!is.null(zip_model)) {
    coef <- summary(zip_model)$coefficients$count["k6_score", ]
    tibble(
      term = "k6_score",
      estimate = coef["Estimate"],
      std.error = coef["Std. Error"],
      statistic = coef["z value"],
      p.value = coef["Pr(>|z|)"],
      model = "ZIP"
    )
  } else {
    tibble(term = "k6_score", estimate = NA, std.error = NA,
           statistic = NA, p.value = NA, model = "ZIP")
  }
  
  # ZINB model
  zinb_model <- tryCatch(
    zeroinfl(as.formula(paste(var, "~ k6_score")), data = df, dist = "negbin"),
    error = function(e) NULL
  )
  
  zinb_tidy <- if (!is.null(zinb_model)) {
    coef <- summary(zinb_model)$coefficients$count["k6_score", ]
    tibble(
      term = "k6_score",
      estimate = coef["Estimate"],
      std.error = coef["Std. Error"],
      statistic = coef["z value"],
      p.value = coef["Pr(>|z|)"],
      model = "ZINB"
    )
  } else {
    tibble(term = "k6_score", estimate = NA, std.error = NA,
           statistic = NA, p.value = NA, model = "ZINB")
  }
  
  # Vuong test Z-statistic
  # Vuong test Z-statistic (robust version)
  # Vuong test Z-statistic (parsed from printed output)
  vuong_z <- NA_real_
  
  if (!is.null(zip_model) && !is.null(zinb_model)) {
    vuong_output <- tryCatch(capture.output(vuong(zip_model, zinb_model)), error = function(e) NULL)
    
    if (!is.null(vuong_output)) {
      # Look for line with AIC-corrected z-stat
      line <- grep("AIC-corrected", vuong_output, value = TRUE)
      if (length(line) == 1) {
        # Extract the numeric z-stat from that line
        z_val <- stringr::str_extract(line, "-?\\d+\\.\\d+")
        if (!is.na(z_val)) vuong_z <- round(as.numeric(z_val), 2)
      }
    }
  }
  
  # Combine all results
  combined <- bind_rows(lmm_tidy, zip_tidy, zinb_tidy) %>%
    mutate(
      dose_var = var,
      overdispersion = round(odr, 2),
      vuong_z = vuong_z
    )
  
  return(combined)
}

# Apply to all dose variables
all_results <- map_dfr(dose_vars, fit_models, data_merged = data_merged)

# Create model comparison summary table
summary_table <- all_results %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2),
    p.value = round(p.value, 3),
    summary = paste0("Est=", estimate, ", SE=", std.error, ", stat=", statistic, ", p=", p.value)
  ) %>%
  dplyr::select(dose_var, model, summary) %>%
  pivot_wider(names_from = model, values_from = summary)

# Display main summary table
print(summary_table, width = Inf)

# Create Vuong Z + Overdispersion summary table
vuong_overdisp_table <- all_results %>%
  dplyr::select(dose_var, overdispersion, vuong_z) %>%
  distinct() %>%
  arrange(dose_var) %>%
  rename(
    `Dose Variable` = dose_var,
    `Overdispersion Ratio` = overdispersion,
    `Vuong Z (ZIP vs ZINB)` = vuong_z
  )

# Display diagnostic summary table
print(vuong_overdisp_table)

#################################################################################################### 
# K6 Score as outcome, Dose Frequency as exposure
# Using Negative Binomial GLMM with random intercepts (glmmTMB)
#################################################################################################### 

# Load required libraries
library(glmmTMB)
library(broom.mixed)
library(purrr)
library(dplyr)
library(tidyr)

# Function to fit NB mixed model using glmmTMB (K6 ~ dose + (1 | participant))
fit_nb_glmmTMB <- function(var, data) {
  df <- data %>% filter(!is.na(.data[[var]]), !is.na(k6_score))
  
  nb_formula <- as.formula(paste("k6_score ~", var, "+ (1 | email_address)"))
  
  model <- tryCatch(
    glmmTMB(nb_formula, family = nbinom2, data = df),
    error = function(e) NULL
  )
  
  if (!is.null(model)) {
    tidy_res <- broom.mixed::tidy(model) %>%
      filter(component == "cond", term == var) %>%
      mutate(model = "NB_glmmTMB", dose_var = var)
  } else {
    tidy_res <- tibble(term = var, estimate = NA, std.error = NA,
                       statistic = NA, p.value = NA, model = "NB_glmmTMB",
                       dose_var = var)
  }
  
  return(tidy_res)
}

# Run model across all dose variables (excluding past12m_any_count for now)
nb_within_results <- map_dfr(setdiff(dose_vars, "past12m_any_count"), fit_nb_glmmTMB, data = data_merged)

# Fit past12m_any_count model separately
nb_any_model <- glmmTMB(k6_score ~ past12m_any_count + (1 | email_address),
                        family = nbinom2, data = data_merged)

nb_any_summary <- tidy(nb_any_model) %>%
  filter(component == "cond", term == "past12m_any_count") %>%
  mutate(
    dose_var = "past12m_any_count",
    model = "NB_glmmTMB",
    summary = paste0("Est=", round(estimate, 4), 
                     ", SE=", round(std.error, 4), 
                     ", stat=", round(statistic, 4), 
                     ", p=", round(p.value, 4))
  ) %>%
  dplyr::select(dose_var, model, summary)

# Combine all model results
nb_within_results_summary <- nb_within_results %>%
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 4),
    p.value = round(p.value, 4),
    summary = paste0("Est=", estimate, ", SE=", std.error,
                     ", stat=", statistic, ", p=", p.value)
  ) %>%
  dplyr::select(dose_var, model, summary) %>%
  bind_rows(nb_any_summary)

# Print final results table
print(nb_within_results_summary, width = Inf)

#################################################################################################### 
# ZINB GLMMs with Convergence Controls and NaN Handling
#################################################################################################### 

model_micro <- glmmTMB(
  k6_score ~ past12m_microdose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_micro)


model_light <- glmmTMB(
  k6_score ~ past12m_light_dose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_light)


model_medium <- glmmTMB(
  k6_score ~ past12m_medium_dose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_medium)

model_strong <- glmmTMB(
  k6_score ~ past12m_strong_dose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_strong)

model_hero <- glmmTMB(
  k6_score ~ past12m_hero_dose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_hero)

model_unknown <- glmmTMB(
  k6_score ~ past12m_unknown_dose_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_unknown)


model_any <- glmmTMB(
  k6_score ~ past12m_any_count + (1 | email_address),
  ziformula = ~1,
  family = nbinom2,
  data = data_merged,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1e5, eval.max = 1e5),
    profile = TRUE
  )
)
summary(model_any)

#################################################################################################### 
# Explore Patterns / Thresshold Effects - Tree Regression
#################################################################################################### 


library(rpart)
library(rpart.plot)
# Define the full vector of variables to keep
vars_to_keep <- c("k6_score", dose_vars)

# Select only those columns and drop missing values
tree_data <- data_merged %>%
  dplyr::select(all_of(vars_to_keep)) %>%
  tidyr::drop_na()

# Fit the regression tree
dose_tree_model <- rpart(
  k6_score ~ .,
  data = tree_data,
  method = "anova",
  control = rpart.control(minsplit = 10, cp = 0.001)
)

summary(dose_tree_model)


#################################################################################################### 
# Clustering
#################################################################################################### 

# Prepare clustering data (standardized to ensure fair distance measures)
dose_data <- data_merged %>%
  dplyr::select(all_of(dose_vars)) %>%
  drop_na() %>%
  scale()  # Normalize for k-means

# Use elbow method
fviz_nbclust(dose_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k", x = "Number of Clusters", y = "Total Within Sum of Squares")

# Use average silhouette method
fviz_nbclust(dose_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k", x = "Number of Clusters", y = "Average Silhouette Width")

# Use gap statistic (slower, but robust)
set.seed(123)
gap_stat <- clusGap(dose_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(title = "Gap Statistic for Optimal k")
# Prepare clustering data
dose_data <- data_merged %>%
  select(all_of(dose_vars)) %>%
  drop_na() %>%
  scale()

# Range of k to test
k_range <- 1:10

# 1. Elbow method (WSS)
wss_values <- map_dbl(k_range, function(k) {
  kmeans(dose_data, centers = k, nstart = 25)$tot.withinss
})

wss_table <- tibble(
  k = k_range,
  Total_Within_SS = round(wss_values, 2)
)

cat("=== Elbow Method: Total Within-Cluster Sum of Squares ===\n")
print(wss_table)

# 2. Silhouette method
library(cluster)
sil_widths <- map_dbl(k_range[-1], function(k) {
  pam(dose_data, k = k)$silinfo$avg.width
})

sil_table <- tibble(
  k = k_range[-1],
  Avg_Silhouette_Width = round(sil_widths, 4)
)

cat("\n=== Silhouette Method: Average Silhouette Width ===\n")
print(sil_table)

# 3. Gap statistic
set.seed(123)
gap_stat <- clusGap(dose_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

gap_table <- as_tibble(gap_stat$Tab) %>%
  mutate(k = 1:10) %>%
  select(k, gap = gap, SE.sim = SE.sim) %>%
  rename(Gap_Statistic = gap, Std_Error = SE.sim) %>%
  mutate(Gap_Upper = Gap_Statistic + Std_Error)

cat("\n=== Gap Statistic (with 1-SE rule) ===\n")
print(gap_table)

#Elbow - There's a visible "elbow" around k = 3 to 4, but WSS continues dropping steadily, suggesting moderate structure without a sharp cutoff.
# Silhouette  Interpretation: k = 2 shows the clearest separation between clusters by far. All other solutions are <0.4, indicating weaker or overlapping groupings.
# Gap Statistic Interpretation: Gap statistic increases steadily and peaks at k = 10, with no clear 1-SE dropoff — suggests incremental improvement rather than a clear “best” cluster solution.
#Clear evidence for 2, but not loving it. 

#Attempting Poisson mixture modeling
# Load required library
library(flexmix)
library(dplyr)

# Define your dose variables if not already defined
dose_vars <- c("past12m_microdose_count",
               "past12m_light_dose_count",
               "past12m_medium_dose_count",
               "past12m_strong_dose_count",
               "past12m_hero_dose_count",
               "past12m_unknown_dose_count")

# Step 1: Prepare user-only data (filtering and dropping NAs)
users_only_data <- data_merged %>%
  filter(past12m_any_count > 0) %>%
  drop_na(all_of(dose_vars))

# Step 2: Fit Poisson mixture models for k = 1 to 5
set.seed(123)
models <- lapply(1:5, function(k) {
  flexmix(as.matrix(users_only_data[dose_vars]) ~ 1, 
          k = k, 
          model = FLXMCmvpois(), 
          control = list(verbose = 0))
})

# Step 3: Compare models using BIC
bic_values <- sapply(models, BIC)
print(bic_values)

# Step 4: Select the best model (lowest BIC)
best_model <- models[[which.min(bic_values)]]
print(best_model)

# Step 5: Extract cluster assignments (only for users used in modeling)
cluster_assignments <- clusters(best_model)

# Step 6: Add cluster labels to the user-only data
users_only <- users_only_data %>%
  mutate(poisson_cluster = factor(cluster_assignments))

# Step 7: (Optional) Merge cluster assignments into the full dataset
data_with_clusters <- data_merged %>%
  left_join(
    users_only %>% select(email_address, poisson_cluster),
    by = "email_address"
  )

# View expected Poisson means for each cluster
lambda_estimates <- parameters(best_model)
print(lambda_estimates)

# Format for plotting
lambda_df <- as.data.frame(lambda_estimates)
lambda_df$dose_type <- rownames(lambda_df)
lambda_long <- lambda_df %>%
  pivot_longer(-dose_type, names_to = "cluster", values_to = "mean_count")

# Plot
ggplot(lambda_long, aes(x = dose_type, y = mean_count, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Expected Psilocybin Dose Frequencies by Cluster",
       x = "Dose Type", y = "Expected Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Descriptive statistics
users_only %>%
  group_by(poisson_cluster) %>%
  summarise(
    n = n(),
    mean_k6 = mean(k6_score, na.rm = TRUE),
    sd_k6 = sd(k6_score, na.rm = TRUE)
  )

# ANOVA
anova_model <- aov(k6_score ~ poisson_cluster, data = users_only)
summary(anova_model)

# Pairwise comparisons
library(emmeans)
emmeans(anova_model, pairwise ~ poisson_cluster)

library(glmmTMB)
glmm <- glmmTMB(k6_score ~ poisson_cluster + (1 | email_address), 
                family = nbinom2, data = users_only)
summary(glmm)

ggplot(users_only, aes(x = poisson_cluster, y = k6_score, fill = poisson_cluster)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Psychological Distress (K6) by Poisson Cluster",
       x = "Cluster", y = "K6 Score") +
  scale_fill_brewer(palette = "Set1")

#################################################################################################### 
# Does Motive for Use Matter?
#################################################################################################### 

motive_vars <- c(
  "motive_enhance_senses", "motive_feel_good", "motive_socialize",
  "motive_mental_health", "motive_relieve_boredom", "motive_spiritual",
  "motive_self_improvement", "motive_self_understanding", "motive_other"
)

data_merged <- data_merged %>%
  mutate(across(all_of(motive_vars), ~na_if(., "4"))) %>%
  mutate(across(all_of(motive_vars), ~na_if(., "5"))) %>%
  mutate(across(all_of(motive_vars), ~na_if(., "3"))) %>%
  mutate(across(all_of(motive_vars), ~factor(., levels = c("Never or Rarely", "Sometimes", "Often or Always"))))

sapply(data_merged[motive_vars], function(x) table(x, useNA = "ifany"))

# Recode motives: keep only valid string responses, set others (numeric etc.) to NA
data_merged <- data_merged %>%
  mutate(across(all_of(motive_vars), ~na_if(., "4"))) %>%
  mutate(across(all_of(motive_vars), ~na_if(., "5"))) %>%
  mutate(across(all_of(motive_vars), ~factor(., levels = c("Never or Rarely", "Sometimes", "Often or Always"))))

for (var in motive_vars) {
  cat("\n==========", var, "vs. K6 ==========\n")
  f <- as.formula(paste("k6_score ~", var))
  print(summary(aov(f, data = data_merged)))
}

for (dose in dose_vars) {
  for (motive in motive_vars) {
    cat("\n==========", dose, "vs.", motive, "==========\n")
    f <- as.formula(paste(dose, "~", motive))
    print(summary(aov(f, data = data_merged)))
  }
}

for (var in motive_vars) {
  cat("\n========== Motive:", var, "vs. Poisson Cluster ==========\n")
  print(table(data_with_clusters[[var]], data_with_clusters$poisson_cluster, useNA = "ifany"))
  print(chisq.test(table(data_with_clusters[[var]], data_with_clusters$poisson_cluster)))
}
#################################################################################################### 
# Does the relationship between Poisson dose-use cluster and K6 distress vary by level of mental health motive endorsement?
#################################################################################################### 

# Ensure poisson_cluster is a factor
data_with_clusters$poisson_cluster <- factor(data_with_clusters$poisson_cluster)

# Define levels of motive
motive_levels <- c("Never or Rarely", "Sometimes", "Often or Always")

# Loop through each motive level and run the model
for (level in motive_levels) {
  cat("\n=====================================\n")
  cat("Mental Health Motive Level:", level, "\n")
  cat("=====================================\n")
  
  # Subset the data
  subset_data <- data_with_clusters %>%
    filter(motive_mental_health == level, !is.na(poisson_cluster), !is.na(k6_score))
  
  # Run ANOVA model
  model <- aov(k6_score ~ poisson_cluster, data = subset_data)
  print(summary(model))
  
  # Optional: Pairwise comparisons
  if (nlevels(subset_data$poisson_cluster) > 2) {
    cat("\nPairwise comparisons (Tukey):\n")
    print(emmeans::emmeans(model, pairwise ~ poisson_cluster))
  }
}

aov(k6_score ~ poisson_cluster * motive_mental_health, data = data_with_clusters)

#################################################################################################### 
# Motive seems to matter; but does it shape patterns of use?
#################################################################################################### 

# Ensure motive_mental_health is a factor
data_with_clusters$motive_mental_health <- factor(
  data_with_clusters$motive_mental_health,
  levels = c("Never or Rarely", "Sometimes", "Often or Always")
)

# List of dose variables
dose_vars <- c("past12m_microdose_count", "past12m_light_dose_count",
               "past12m_medium_dose_count", "past12m_strong_dose_count",
               "past12m_hero_dose_count", "past12m_unknown_dose_count")

# Loop through and run ANOVA
for (dose in dose_vars) {
  cat("\n==========", dose, "~ motive_mental_health ==========\n")
  f <- as.formula(paste(dose, "~ motive_mental_health"))
  print(summary(aov(f, data = data_with_clusters)))
}

# Filter to complete cases
cluster_table_data <- data_with_clusters %>%
  filter(!is.na(motive_mental_health), !is.na(poisson_cluster))

# Cross-tab and test
cluster_table <- table(cluster_table_data$motive_mental_health, cluster_table_data$poisson_cluster)
print(cluster_table)
print(chisq.test(cluster_table))

#################################################################################################### 
# It does shape patterns of use so; lets see if those who indiciated using for mental health reasons actually saw decreases in K6 scores. 
#################################################################################################### 

lapply(wave1[k6_items], unique)

# Convert and summarize K6 items for both waves  
wave1 <- wave1 %>%
  mutate(across(all_of(k6_items), ~ recode(as.character(.), 
                                           "None of the time" = 0,
                                           "A little of the time" = 1,
                                           "Some of the time" = 2,
                                           "Most of the time" = 3,
                                           "All of the time" = 4,
                                           .default = NA_real_))) %>%
  mutate(k6_score_wave1 = rowSums(across(all_of(k6_items)), na.rm = TRUE))

wave2 <- wave2 %>%
  mutate(across(all_of(k6_items), ~ recode(as.character(.), 
                                           "None of the time" = 0,
                                           "A little of the time" = 1,
                                           "Some of the time" = 2,
                                           "Most of the time" = 3,
                                           "All of the time" = 4,
                                           .default = NA_real_))) %>%
  mutate(k6_score_wave2 = rowSums(across(all_of(k6_items)), na.rm = TRUE))

# Merge wave data and motive information from the first wave
data_longitudinal <- wave1 %>%
  select(email_address, motive_mental_health, k6_score_wave1) %>%
  inner_join(wave2 %>% select(email_address, k6_score_wave2), by = "email_address") %>%
  mutate(k6_change = k6_score_wave2 - k6_score_wave1) %>%
  filter(!is.na(motive_mental_health), !is.na(k6_score_wave1), !is.na(k6_score_wave2))

# Ensure motive is a factor
data_longitudinal$motive_mental_health <- factor(
  data_longitudinal$motive_mental_health,
  levels = c("Never or Rarely", "Sometimes", "Often or Always")
)
data_complete <- data_longitudinal %>%
  filter(!is.na(motive_mental_health), !is.na(k6_score_wave1), !is.na(k6_change))

# Fit the full model
model <- lm(k6_change ~ motive_mental_health + k6_score_wave1, data = data_complete)
summary(model)

# Fit the null model using the exact same dataset
model_null <- lm(k6_change ~ k6_score_wave1, data = data_complete)

# Compare models
anova(model_null, model)

# Descriptive statistics for K6 change by mental health motive
data_longitudinal %>%
  group_by(motive_mental_health) %>%
  summarise(
    mean_k6_change = mean(k6_change, na.rm = TRUE),
    sd_k6_change = sd(k6_change, na.rm = TRUE),
    n = n()
  )


##################################################################################################
# K6 Scores plots
#################################################################################################### 
library(ggplot2)
library(patchwork)  # For arranging plots

# Ensure needed variables are present
plot_data <- data_wide %>%
  filter(!is.na(k6_score_wave1), !is.na(k6_score_wave2)) %>%
  mutate(k6_change = k6_score_wave2 - k6_score_wave1)

# Plot 1: K6 Wave 1
p1 <- ggplot(plot_data, aes(x = k6_score_wave1)) +
  geom_histogram(binwidth = 1, fill = "#3182bd", color = "white") +
  labs(title = "K6 Score at Wave 1", x = "K6 Score", y = "Count") +
  theme_minimal()

# Plot 2: K6 Wave 2
p2 <- ggplot(plot_data, aes(x = k6_score_wave2)) +
  geom_histogram(binwidth = 1, fill = "#9ecae1", color = "white") +
  labs(title = "K6 Score at Wave 2", x = "K6 Score", y = "Count") +
  theme_minimal()

# Plot 3: Change in K6
p3 <- ggplot(plot_data, aes(x = k6_change)) +
  geom_histogram(binwidth = 1, fill = "#fb6a4a", color = "white") +
  labs(title = "Change in K6 (Wave 2 − Wave 1)", x = "K6 Change", y = "Count") +
  theme_minimal()

# Combine using patchwork
p1 + p2 + p3 + plot_layout(ncol = 3)





