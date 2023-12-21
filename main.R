############ CONFIGURATION ###########

library(readxl)
library(brms)
library(parallelly)
library(remotes)
# Package to deal with results ( not an official CRAN package yet. Needs package "remotes")
#remotes::install_github('m-clark/mixedup')
library(mixedup)
library(tidyr)
library(dplyr)
library(posterior)

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

# NOTE:
# We have 1 numeric value (distance), 1 categorical value (structure) as well as
# two more categorical values (questions and participants) that we will have as
# random values.

############ MULTINOMIAL MODEL ###########

# Gets number of cores available in the machine
nc_cores <- availableCores()

# Fit a multinomial model using brms
fit_multinomial_model = brm(
  # Note: a categorical distribution is a form of multinomial distribution that
  # models the outcome of n experiments, where the outcome of each trial 
  # has a categorical distribution. In this case, it describes the possible 
  # results of a random variable (distance) that can take on one of K possible 
  # categories (structure), with the probability of each category separately 
  # specified. See: https://en.wikipedia.org/wiki/Categorical_distribution#cite_note-minka-2
  structure ~ distance + (1 | question) + (1 | participant), 
  data  = df,
  family = categorical(link=logit, refcat = NA),
  # Number of available cores
  cores = nc_cores,
  iter = 2000, #standard: 2000. Reduce for faster processing.
  warmup = 1000, #standard: 1000. Reduce for faster processing.
  tuniprior = c(
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

# Display posterior mean
posterior_means <- posterior_samples(fit_multinomial_model)$mean
draws <- as_draws_array(fit_multinomial_model)
results_posterior_means <- summarise_draws(draws, default_summary_measures())
# Posterior mean with distance (elliptical): -0.204 
# Posterior mean with distance (juxtaposition): -0.0994
# Posterior mean with distance (overt connective): -0.0956

# Posterior mean with intercept (elliptical): 2,39
# Posterior mean with intercept (juxtaposition): 1,62
# Posterior mean with intercept (overt connective): 2,35

# Display random effects for intercepts
ranef(fit_multinomial_model)
posterior_mean_alpha <- mean(posterior_samples[, "alpha"])
posterior_mean_beta <- mean(posterior_samples[, "beta"])
posterior_mean_sigma <- mean(posterior_samples[, "sigma"])

# Model checking with posterior predictive checks
pp_check(fit_multinomial_model) # shows dens_overlay plot by default

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
summary(lm(elliptical ~ distance, data = prob_table))
plot(prob_table$distance, prob_table$elliptical, ylim=c(0,1))
lm_model <- lm(prob_table$elliptical ~ prob_table$distance)
abline(lm_model, col = "blue")
# P-value: < 2.16e-09
#Coefficient: -0.0007504

# JUXTAPOSITION:
summary(lm(juxtaposition ~ distance, data = prob_table))
plot(prob_table$distance, prob_table$juxtaposition, ylim=c(0,1))
lm_model <- lm(prob_table$juxtaposition ~ prob_table$distance)
abline(lm_model, col = "blue")
# P-value: < 2e-16
#Coefficient: -0.0004410

# OVERT CONNECTIVE:
summary(lm(overt_connective ~ distance, data = prob_table))
plot(prob_table$distance, prob_table$overt_connective, ylim=c(0,1))
lm_model <- lm(prob_table$overt_connective ~ prob_table$distance)
abline(lm_model, col = "blue")
# P-value: <2e-16
#Coefficient: 0.0012242

 


############ REFERENCES ###########

# https://m-clark.github.io/mixed-models-with-R/bayesian.html
# https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf
# https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html