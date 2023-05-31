library(tidyverse)

set.seed(123)  # For reproducibility

# Simulate data for 150 people
n_people <- 150

# Simulate binary predictor variable
predictor <- as.factor(rep(c("low","high"), times = 27000/2))

# Simulate reaction times
reaction_times <- rnorm(n_people * 180, mean = 0.5, sd = 0.1)  # Assuming a mean of 0.5 seconds

# Simulate likert scale responses
likert_responses <- factor(sample(1:7, n_people * 180, replace = TRUE), ordered = TRUE)

# Create a data frame combining the simulated data
simulated_data <- tibble(
  person_id = rep(1:n_people, each = 180),  # Repeat person IDs for each trial
  item = as.factor(rep(1:180, times = n_people)),  # Generate item numbers
  predictor,  # Repeat predictor values for each trial
  reaction_time = reaction_times,  # Reaction times
  likert_response = likert_responses  # Likert scale responses
)

# Write the simulated data to a CSV file
write_csv(simulated_data, "simulated_data.csv")

library(lmerTest)
library(buildmer)
library(ordinal)

model_granRT <- buildclmm(as.factor(reaction_times) ~ predictor +
                           (1 + predictor | person_id) +
                           (1 + predictor | item),
                         data = simulated_data)









