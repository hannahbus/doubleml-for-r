# Test Script for Multiple Treatments

setwd("~/Documents/Research/climate_risk_beliefs/dml_r/doubleml-for-r/R/")

# Load the package dependencies
dependencies <- tools::package_dependencies("DoubleML", recursive = TRUE)

# Extract the names of the dependencies
dependency_names <- unlist(dependencies)

# Load each package using lapply
for (dep in dependency_names){
  library(dep, character.only = TRUE)
}

library(dplyr)

# Load code, which I manipulated manually.

source("double_ml.R")
source("double_ml_data.R")
source("helper.R")
source("double_ml_irme.R")

set.seed(123)

# Generate data ----
n <- 9000  # Number of values to generate

# Treatment Assignment
numbers <- runif(n)
treat <- ifelse(numbers < 1/3, 0, ifelse(numbers < 2/3, 1, 2))

# Group Assignment 
numbers <- runif(n)
group <- ifelse(numbers < 1/2, 0, 1)

# Error
error <- rnorm(n)

# Outcome generation

y <-  ifelse(treat == 1, 1, 0) * ifelse(group == 1, 1, 0) * -1 + 
  ifelse(treat == 2, 1, 0) * ifelse(group == 1, 1, 0) * 2 +
  ifelse(treat == 1, 1, 0) * ifelse(group == 0, 1, 0) * 2 +
  ifelse(treat == 2, 1, 0) * ifelse(group == 0, 1, 0) * 1 +
    rep(3, n) + error

# Check conditional means if computation has been correct

df <- data.frame(cbind(y, treat, group))
df %>%
  group_by(treat, group) %>%
  summarise(n = mean(y))

# Everything is correct

# Do estimation and test if functions as defined work. ----

# Transform treatment variable

df <- df %>%
       mutate(d1 = ifelse(treat == 2, 1, 0),
         d = ifelse(treat == 1, 1, 0),
         d0 = ifelse(treat == 0, 1, 0))


# Declare DML data.frame

df_dml <- double_ml_data_from_data_frame(df = df,
                               y = "y",
                               d = c("d", "d1"),
                               x_cols = "group")

# Initialize ML learners for the nuisance parameters.

ml_l <- lrn("regr.ranger", num.trees = 500, min.node.size = 2,
             max.depth = 5)
ml_m <- lrn("classif.ranger", num.trees = 500, min.node.size = 2,
             max.depth = 5)

# Instantiate class of IRME

doubleml_irm_manual <- DoubleMLIRME$new(df_dml, ml_l, ml_m)

doubleml_irm_manual$fit()
doubleml_irm_manual$summary()

# Fit 

# Compute Standard Errors :)
