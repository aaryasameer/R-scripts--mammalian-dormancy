###using GLM to predict Diet (CHO) based on TYPE, mass and climate
library(nnet)   # for multinomial logistic regression as i have 3 categorical variables- CHO
library(dplyr)  
library(readxl)

##loading dataset
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")

##preparing data
glm_data <- dormant_data %>%
  select(TrophicLevel, Body_mass_new, bio1_mean, TYPE, Order_new) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),   # the outcome
    TYPE = factor(TYPE),                  
    across(c(Body_mass_new, bio1_mean), as.numeric)  # numeric predictors
  ) %>%
  na.omit()  # remove na

#scaling the data to be accurate
glm_data$Body_mass_scaled <- scale(glm_data$Body_mass_new)
glm_data$bio1_scaled <- scale(glm_data$bio1_mean)


#running the model
model <- multinom(TrophicLevel ~ Body_mass_scaled + bio1_scaled + TYPE, data = glm_data)

summary(model)
exp(coef(model))   ##Interpretation = For every 1-unit increase in [predictor], the odds of being [Herbivore or Omnivore] (vs Carnivore) change by [OR - 1] × 100%”

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values
z
summary(model)$standard.errors

###removing body mass cause its p values suck
##preparing data
glm_data <- dormant_data %>%
  select(TrophicLevel, bio1_mean, TYPE, Order_new) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),   # the outcome
    TYPE = factor(TYPE),                  
    across(c(bio1_mean), as.numeric)  # numeric predictors
  ) %>%
  na.omit()  # remove na

#scaling the data to be accurate
glm_data$bio1_scaled <- scale(glm_data$bio1_mean)


#running the model
model <- multinom(TrophicLevel ~ + bio1_scaled + TYPE, data = glm_data)

summary(model)
exp(coef(model))   ##Interpretation = For every 1-unit increase in [predictor], the odds of being [Herbivore or Omnivore] (vs Carnivore) change by [OR - 1] × 100%”

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values


###BIO2
##preparing data
glm_data <- dormant_data %>%
  select(TrophicLevel, bio2_mean, TYPE, Order_new) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),   # the outcome
    TYPE = factor(TYPE),                  
    across(c(bio2_mean), as.numeric)  # numeric predictors
  ) %>%
  na.omit()  # remove na

#scaling the data to be accurate
glm_data$bio2_scaled <- scale(glm_data$bio2_mean)


#running the model
model <- multinom(TrophicLevel ~ + bio2_scaled + TYPE, data = glm_data)

summary(model)
exp(coef(model))   ##Interpretation = For every 1-unit increase in [predictor], the odds of being [Herbivore or Omnivore] (vs Carnivore) change by [OR - 1] × 100%”

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

###BIO3
##preparing data
glm_data <- dormant_data %>%
  select(TrophicLevel, bio3_mean, TYPE, Order_new) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),   # the outcome
    TYPE = factor(TYPE),                  
    across(c(bio3_mean), as.numeric)  # numeric predictors
  ) %>%
  na.omit()  # remove na

#scaling the data to be accurate
glm_data$bio3_scaled <- scale(glm_data$bio3_mean)


#running the model
model <- multinom(TrophicLevel ~ + bio3_scaled + TYPE, data = glm_data)

summary(model)
exp(coef(model))   ##Interpretation = For every 1-unit increase in [predictor], the odds of being [Herbivore or Omnivore] (vs Carnivore) change by [OR - 1] × 100%”

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

##BIO4
##preparing data
glm_data <- dormant_data %>%
  select(TrophicLevel, bio4_mean, TYPE, Order_new) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),   # the outcome
    TYPE = factor(TYPE),                  
    across(c(bio4_mean), as.numeric)  # numeric predictors
  ) %>%
  na.omit()  # remove na

#scaling the data to be accurate
glm_data$bio4_scaled <- scale(glm_data$bio4_mean)


#running the model
model <- multinom(TrophicLevel ~ + bio4_scaled + TYPE, data = glm_data)

summary(model)
exp(coef(model))   ##Interpretation = For every 1-unit increase in [predictor], the odds of being [Herbivore or Omnivore] (vs Carnivore) change by [OR - 1] × 100%”

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

###BIO1 + BIO4 multivariate model
glm_data <- dormant_data %>%
  select(TrophicLevel, TYPE, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

model_combined <- multinom(TrophicLevel ~ bio1_scaled + bio4_scaled + TYPE, data = glm_data)

summary(model_combined)
exp(coef(model_combined))  # Odds ratios

z <- summary(model_combined)$coefficients / summary(model_combined)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

#####INTERACTIONS########
##BIO1 interactions
#running the model
model <- multinom(TrophicLevel ~ + bio1_scaled*TYPE, data = glm_data)

glm_data <- dormant_data %>%
  select(TrophicLevel, TYPE, bio1_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean))
  ) %>%
  na.omit()

summary(model)
exp(coef(model))  # Odds ratios

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

#####INTERACTIONS########
##BIO4 interactions
library(nnet)

glm_data <- dormant_data %>%
  select(TrophicLevel, TYPE, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),
    TYPE = factor(TYPE),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

model <- multinom(TrophicLevel ~ bio4_scaled * TYPE, data = glm_data)

summary(model)
exp(coef(model))

z <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
print(p_values)

####INTERACTIONS######
#bio1 + bio4
library(nnet)

# Prepare data
glm_data <- dormant_data %>%
  select(TrophicLevel, TYPE, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

# Fit multinomial model with interactions
model_interaction <- multinom(TrophicLevel ~ bio1_scaled * TYPE + bio4_scaled * TYPE, data = glm_data)

# Summary
summary(model_interaction)

# Odds ratios
exp(coef(model_interaction))

# P-values
z <- summary(model_interaction)$coefficients / summary(model_interaction)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
print(p_values)

###Using GLM to predict carnivory or herbivory, related to torpor type and climate
library(dplyr)
library(nnet)

# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio1_scaled * TYPE + bio4_scaled * TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

null_model <- glm(TrophicLevel ~ 1, data = glm_data, family = binomial())
AIC(null_model)


plot(Effect(c("bio1_scaled", "TYPE"), model_bin), 
     multiline = TRUE, 
     ci.style = "bands",
     main = "Effect of Temperature and Torpor Type on Herbivory",
     ylab = "Probability of Herbivory",
     xlab = "Annual Mean Temperature (scaled)")

plot(Effect(c("bio4_scaled", "TYPE"), model_bin), 
     multiline = TRUE, 
     ci.style = "bands",
     main = "Effect of Seasonality and Torpor Type on Herbivory",
     ylab = "Probability of Herbivory",
     xlab = "Temperature Seasonality (scaled)")


####INTERACTIONS######
#bio1 + bio4
library(nnet)

# Prepare data
glm_data <- dormant_data %>%
  select(TrophicLevel, TYPE, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel),
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

# Fit multinomial model with interactions
model_interaction <- multinom(TrophicLevel ~ bio1_scaled * TYPE + bio4_scaled * TYPE, data = glm_data)

# Summary
summary(model_interaction)

# Odds ratios
exp(coef(model_interaction))

# P-values
z <- summary(model_interaction)$coefficients / summary(model_interaction)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
print(p_values)

####bio3 + bio2 
library(dplyr)
library(nnet)

# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio3_mean, bio2_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio2_scaled = scale(as.numeric(bio2_mean)),
    bio3_scaled = scale(as.numeric(bio3_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio3_scaled * TYPE + bio2_scaled*TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

null_model <- glm(TrophicLevel ~ 1, data = glm_data, family = binomial())
AIC(null_model)

####bio3 + bio2 + bio1 + bio4
library(dplyr)
library(nnet)

# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio3_mean, bio2_mean, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio2_scaled = scale(as.numeric(bio2_mean)),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean)),
    bio3_scaled = scale(as.numeric(bio3_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio3_scaled * TYPE + bio2_scaled*TYPE + bio1_scaled*TYPE + bio4_scaled*TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

null_model <- glm(TrophicLevel ~ 1, data = glm_data, family = binomial())
AIC(null_model)

###binomial regression of each of the bio variables with herbivory
#bio1 
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio1_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean))
  ) %>%
  na.omit()
model_bin <- glm(TrophicLevel ~ bio1_scaled * TYPE,
                 data = glm_data,
                 family = binomial())
summary(model_bin)
exp(coef(model_bin))
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values
####bio2
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio2_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio2_scaled = scale(as.numeric(bio2_mean))
  ) %>%
  na.omit()
model_bin <- glm(TrophicLevel ~ bio2_scaled * TYPE,
                 data = glm_data,
                 family = binomial())
summary(model_bin)
exp(coef(model_bin))
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values
####bio3
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio3_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio3_scaled = scale(as.numeric(bio3_mean))
  ) %>%
  na.omit()
model_bin <- glm(TrophicLevel ~ bio3_scaled * TYPE,
                 data = glm_data,
                 family = binomial())
summary(model_bin)
exp(coef(model_bin))
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values
###bio4
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()
model_bin <- glm(TrophicLevel ~ bio4_scaled * TYPE,
                 data = glm_data,
                 family = binomial())
summary(model_bin)
exp(coef(model_bin))
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

####bio3 + bio1 + bio4
library(dplyr)
library(nnet)

# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio3_mean, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean)),
    bio3_scaled = scale(as.numeric(bio3_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio3_scaled * TYPE + bio1_scaled*TYPE + bio4_scaled*TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

null_model <- glm(TrophicLevel ~ 1, data = glm_data, family = binomial())
AIC(null_model)

####BIO1 + BIO4
# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio1_mean, bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio1_scaled*TYPE + bio4_scaled*TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values

####BIO1 + BIO3
# Prepare data: keep only Carnivore and Herbivore
glm_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, bio1_mean, bio3_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Set reference
    TYPE = factor(TYPE),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio3_scaled = scale(as.numeric(bio3_mean))
  ) %>%
  na.omit()

# Run logistic regression with interactions
model_bin <- glm(TrophicLevel ~ bio1_scaled*TYPE + bio3_scaled*TYPE,
                 data = glm_data,
                 family = binomial())

# Summary
summary(model_bin)

# Odds ratios
exp(coef(model_bin))

# P-values
z <- coef(summary(model_bin))[, "Estimate"] / coef(summary(model_bin))[, "Std. Error"]
p_values <- 2 * (1 - pnorm(abs(z)))
p_values
