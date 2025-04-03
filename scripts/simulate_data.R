# Load necessary packages
library(dplyr)
library(tibble)
library(readr)

set.seed(38)  # for reproducibility

# Number of observations
n <- 1000

# Simulate variables
Age <- round(rnorm(n, mean = 60, sd = 15))
Age <- pmin(pmax(Age, 18), 90)  # clip to 18–90

BMI <- round(rnorm(n, mean = 27, sd = 5), 1)
BMI <- pmin(pmax(BMI, 15), 50)

ASA_Class <- factor(sample(1:5, n, replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.15, 0.05)))

Procedure_Types <- factor(c("Lap Chole", "Colectomy", "Hernia Repair", "Appendectomy", "Gastrectomy"))
Procedure_Type <- sample(Procedure_Types, n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1))

Surgery_Duration_Minutes <- round(rlnorm(n, meanlog = log(90), sdlog = 0.4))

Estimated_Blood_Loss <- round(rlnorm(n, meanlog = log(100), sdlog = 0.7))
Estimated_Blood_Loss <- pmin(Estimated_Blood_Loss, 2000)  # cap at 2000 mL

Intraoperative_Events <- factor(rbinom(n, 1, prob = 0.15))

# Logistic model for complication risk
log_odds <- -3.5 + 
  0.03 * (Age - 60) + 
  0.05 * (BMI - 27) + 
  0.4 * (ASA_Class - 2) +
  0.005 * (Surgery_Duration_Minutes - 90) +
  0.002 * (Estimated_Blood_Loss - 100) +
  1.0 * Intraoperative_Events

prob_Complication <- 1 / (1 + exp(-log_odds))
Complication_30d <- rbinom(n, 1, prob = prob_Complication)

# Combine into data frame
surgical_data <- tibble(
  Age,
  BMI,
  ASA_Class,
  Procedure_Type,
  Surgery_Duration_Minutes,
  Estimated_Blood_Loss,
  Intraoperative_Events,
  Complication_30d
)

# Save to CSV
write_csv(surgical_data, "data/simulated_surgical_data.csv")
