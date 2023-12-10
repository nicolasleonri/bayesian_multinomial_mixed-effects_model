############ CONFIGURATION ###########

library(readxl)
library(brms)
library(parallelly)
library(remotes)
# Package to deal with results (its not an official CRAN package yet. Needs package "remotes")
#remotes::install_github('m-clark/mixedup')
library(mixedup)
library(tidyr)
library(dplyr)

############ PROCESSING ###########

# Read and format data from Excel file
# Adjust the file path and sheet index as needed
df <- read_excel(
  path = "data/distance.dataset.xlsx",
  sheet = 1,
  col_names = TRUE,
  col_types = c("numeric", "text", "text", "numeric")
)

# Clean up the 'structure' column by removing line breaks
df$structure <- gsub("\\r\\n", "", df$structure)

# Extract numeric values from the 'question' column
df$question <- as.numeric(gsub("[^0-9]", "", df$question))

# Convert 'structure', 'question', and 'participant' columns to factors
df$structure <- factor(df$structure)
df$question <- factor(df$question)
df$participant <- factor(df$participant)

# Display a summary of the processed dataframe
summary(df)

# Then we get the categories we want to observe
target_values <- c(unique(df$structure))

# And we create binary columns of them if they were found or not
df$overt_connective <-
  ifelse(df$structure %in% target_values[1], 1, 0)
df$elliptical <- ifelse(df$structure %in% target_values[2], 1, 0)
df$juxtaposition <- ifelse(df$structure %in% target_values[3], 1, 0)

# We factor them, as they're categorical values
df$overt_connective <- factor(df$overt_connective)
df$elliptical <- factor(df$elliptical)
df$juxtaposition <- factor(df$juxtaposition)

# And we observe them
summary(df)

# NOTE:
# We have 1 numeric value (distance), 4 categorical values (structure, that we
# won't use anymore; overt_connective; elliptical and juxtaposition) as well as
# two more numeric values (questions and participants) that we will analyze as
# random values. This means we have to analyze how influential (or not) these
# are in our analysis.

############ MULTINOMIAL MODEL ###########

# Gets number of cores available in the machine
nc_cores <- availableCores()

# Fit a multinomial model using brms
fit_multinomial_model = brm(
  # Note: the brms package does a categorical model, but by adding
  # the random effect (1 | x) we are making it multinomial.
  structure ~ distance + (1 | question) + (1 | participant), 
  data  = df,
  family = categorical(link=logit, refcat = NA),
  # Number of available cores
  cores = nc_cores,
  iter = 100, #standard: 2000. Reduce for faster processing.
  warmup = 10, #standard: 1000. Reduce for faster processing.
  prior = c(
    set_prior("normal(0, 1)", dpar = "muelliptical"),
    set_prior("normal(0, 2)", dpar = "mujuxtaposition"),
    set_prior("normal(0, 3)", dpar = "muovertconnective")
  ),
  sample_prior = "yes",
  thin = 5,
  control = list(adapt_delta = 0.99),
  # saves all information
  save_pars = save_pars(group = TRUE, all = TRUE),
)

# Display a summary of the fitted model
summary(fit_multinomial_model)

# Display random effects for intercepts
ranef(fit_multinomial_model)

# Model checking with posterior predictive checks
pp_check(fit_multinomial_model)

# Extract random effects into a nice table and saves it
results <- mixedup::extract_random_effects(fit_multinomial_model) 
ce <- conditional_effects(fit_multinomial_model, categorical = TRUE)
ce <- as.data.frame(ce[[1]])
write.csv(ce, file = "results/results_conditional_effects.csv", row.names = FALSE)

# Visualize conditional effects
conditional_effects(fit_multinomial_model, categorical = TRUE, plot = FALSE)

# To get a table that includes the probability of each category, first we pivot the data
prob_table <- ce %>%
  pivot_wider(
    id_cols = c("distance", "cond__"),
    names_from = "cats__",
    values_from = "estimate__"
  ) %>%
  select(-cond__) %>%
  mutate(across(-c(distance), ~ifelse(is.na(.), 0, .)))  # Replace NAs with 0
# Rename the columns
colnames(prob_table) <- c("distance", "elliptical", "juxtaposition", "overt_connective")
# Saves it
write.csv(prob_table, file = "results/prob_table.csv", row.names = FALSE)

############ RESULTS AFTER LOGISTIC REGRESSION ###########

# ELLIPTICAL:
summary(lm(distance ~ elliptical, data = prob_table))
# P-value:
#Coefficient:

# JUXTAPOSITION:
summary(lm(distance ~ juxtaposition, data = prob_table))
# P-value:
#Coefficient:

# OVERT CONNECTIVE:
summary(lm(distance ~ overt_connective, data = prob_table))
# P-value:
#Coefficient:

############ REFERENCES ###########

# https://m-clark.github.io/mixed-models-with-R/bayesian.html
# https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf
# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html