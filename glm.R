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

####bio3 + bio2 + bio1 + bio4 = model10######################################
library(dplyr)
library(nnet)

# Prepare data: keep only Carnivore and Herbivore
model10_data <- dormant_data %>%
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
model10 <- glm(TrophicLevel ~ bio3_scaled * TYPE + bio2_scaled*TYPE + bio1_scaled*TYPE + bio4_scaled*TYPE,
                 data = model10_data,
                 family = binomial())

# Summary
summary(model10)

# Odds ratios
exp(coef(model10))

# P-values
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

####BIO1 + BIO3, model9
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
model9 <- glm(TrophicLevel ~ bio1_scaled*TYPE + bio3_scaled*TYPE,
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


####GLM PLOTS

library(effects)
library(ggplot2)

###model4
model4 <- glm(TrophicLevel ~ bio1_scaled * TYPE, 
                 data = glm_data,
                 family = binomial())

eff <- Effect(c("bio1_scaled", "TYPE"), model4)

eff_df <- as.data.frame(eff)

# Plot DT and HIB separately
ggplot(eff_df, aes(x = bio1_scaled, y = fit, color = TYPE)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    title = "Probability of Herbivory vs BIO1 (Temperature)",
    x = "BIO1 (Annual Mean Temperature, scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("DT" = "#0072B2", "HIB" = "#D55E00")) +
  scale_fill_manual(values = c("DT" = "#0072B2", "HIB" = "#D55E00"))

###model1
library(effects)
library(ggplot2)

eff1 <- Effect(c("bio1_scaled", "TYPE"), model1)
eff4 <- Effect(c("bio4_scaled", "TYPE"), model1)

eff1_df <- as.data.frame(eff1)
eff4_df <- as.data.frame(eff4)

# Plot for bio1
p1 <- ggplot(eff1_df, aes(x = bio1_scaled, y = fit, color = TYPE)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    title = "Effect of Temperature (BIO1) × Torpor Type",
    x = "BIO1 (scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()

# Plot for bio4
p2 <- ggplot(eff4_df, aes(x = bio4_scaled, y = fit, color = TYPE)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    title = "Effect of Seasonality (BIO4) × Torpor Type",
    x = "BIO4 (scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()

# Print 
print(p1)
print(p2)
library(patchwork)
p1 + p2  

###model9 (bio1 + bio3)
library(effects)
library(ggplot2)

eff1 <- Effect(c("bio1_scaled", "TYPE"), model10)
eff3 <- Effect(c("bio3_scaled", "TYPE"), model10)
eff2 <- Effect(c("bio2_scaled", "TYPE"), model10)

eff1_df <- as.data.frame(eff1)
eff3_df <- as.data.frame(eff3)

# Plot for bio1
p1 <- ggplot(eff1_df, aes(x = bio1_scaled, y = fit, color = TYPE)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    x = "BIO1 (scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()

# Plot for bio3
p2 <- ggplot(eff3_df, aes(x = bio3_scaled, y = fit, color = TYPE)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    x = "BIO3(scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()

# Print 
print(p1)
print(p2)
library(patchwork)
p1 + p2  



##########ecological variables######################
#latitude
glm_data_lat <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, lat_study_loc) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Carnivore = 0, Herbivore = 1
    TYPE = factor(TYPE),
    lat_study_loc = as.numeric(lat_study_loc),
    Latitude_scaled = scale(lat_study_loc)
  ) %>%
  na.omit()

model_lat <- glm(TrophicLevel ~ Latitude_scaled * TYPE,
                 data = glm_data_lat,
                 family = binomial())

summary(model_lat)
exp(coef(model_lat))

p_values <- summary(model_lat)$coefficients[,4]
p_values

#################NPP##########################
glm_data_npp <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, NPP) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),
    TYPE = factor(TYPE),
    NPP = as.numeric(NPP),
    NPP_scaled = scale(NPP)
  ) %>%
  na.omit()

model_npp <- glm(TrophicLevel ~ NPP_scaled * TYPE,
                 data = glm_data_npp,
                 family = binomial())

summary(model_npp)
exp(coef(model_npp))    
p_values <- summary(model_npp)$coefficients[, 4]
p_values

###NPP and type and bio

glm_data_bio_npp <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, NPP, bio1_mean,bio2_mean,bio3_mean,bio4_mean) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Carnivore = 0, Herbivore = 1
    TYPE = factor(TYPE),
    NPP_scaled = scale(as.numeric(NPP)),
    bio1_scaled = scale(as.numeric(bio1_mean)),
    bio2_scaled = scale(as.numeric(bio2_mean)),
    bio3_scaled = scale(as.numeric(bio3_mean)),
    bio4_scaled = scale(as.numeric(bio4_mean))
  ) %>%
  na.omit()

model_npp_bio1 <- glm(TrophicLevel ~ NPP_scaled * TYPE + bio1_scaled * TYPE + bio2_scaled * TYPE + bio3_scaled * TYPE + bio4_scaled * TYPE,
                      data = glm_data_bio_npp,
                      family = binomial())
summary(model_npp_bio1)
AIC(model_npp_bio1)
summary(model_npp_bio1)$coefficients[,4]


###latitude and npp with TYPE
glm_data_lat_npp <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, lat_study_loc, NPP) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),  # Carnivore = 0, Herbivore = 1
    TYPE = factor(TYPE),
    Latitude_scaled = scale(as.numeric(lat_study_loc)),
    NPP_scaled = scale(as.numeric(NPP))
  ) %>%
  na.omit()
model_lat_npp <- glm(TrophicLevel ~ Latitude_scaled * TYPE + NPP_scaled * TYPE,
                     data = glm_data_lat_npp,
                     family = binomial())
summary(model_lat_npp)
exp(coef(model_lat_npp))
p_values <- summary(model_lat_npp)$coefficients[, 4]
p_values

library(effects)
library(ggplot2)

eff_npp <- Effect(c("NPP_scaled", "TYPE"), model_lat_npp)

eff_npp_df <- as.data.frame(eff_npp)

p_npp <- ggplot(eff_npp_df, aes(x = NPP_scaled, y = fit, color = TYPE)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = TYPE), alpha = 0.2, color = NA) +
  labs(
    x = "Net Primary Productivity (scaled)",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()

print(p_npp)

#######cz
glm_data_zone <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore")) %>%
  select(TrophicLevel, TYPE, Climate_zone) %>%
  mutate(
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),
    TYPE = factor(TYPE),
    Climate_zone = factor(Climate_zone, levels = c("polar", "temperate", "subtropical", "tropical"))  # you can change the order if needed
  ) %>%
  na.omit()

model_zone <- glm(TrophicLevel ~ Climate_zone * TYPE,
                  data = glm_data_zone,
                  family = binomial())

summary(model_zone)
exp(coef(model_zone))  # odds ratios
AIC(model_zone)

library(effects)
library(ggplot2)

eff_zone <- Effect(c("Climate_zone", "TYPE"), model_zone)
eff_zone_df <- as.data.frame(eff_zone)

ggplot(eff_zone_df, aes(x = Climate_zone, y = fit, fill = TYPE)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(0.7), width = 0.25) +
  labs(
    title = "Effect of Climate Zone and TYPE on Herbivory Probability",
    x = "Climate Zone",
    y = "Predicted Probability of Herbivory"
  ) +
  theme_minimal()



library(dplyr)
library(ggplot2)

# Clean and prepare
zone_bar_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore"),
         Climate_zone %in% c("polar", "temperate", "tropical", "subtropical")) %>%
  mutate(
    TYPE = factor(TYPE),
    TrophicLevel = factor(TrophicLevel, levels = c("Carnivore", "Herbivore")),
    Climate_zone = factor(tolower(Climate_zone), 
                          levels = c("polar", "temperate", "tropical", "subtropical"))
  )

ggplot(zone_bar_data, aes(x = TYPE, fill = TrophicLevel)) +
  geom_bar(position = "stack") +
  facet_wrap(~ Climate_zone, nrow = 1) +
  labs(
    title = "TrophicLevel Distribution by TYPE across Climate Zones",
    x = "Dormancy Type",
    y = "Number of Species",
    fill = "Trophic Level"
  ) +
  theme_minimal(base_size = 12)

