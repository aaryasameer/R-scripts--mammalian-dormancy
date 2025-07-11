#05/06/25- Aarya 

###diet composition data of mammals from dormant mammal dataset

data <- read.csv("C:/Users/AARYA/Downloads/final_cleaned_dataset.csv")

dataa <- data %>% 
  filter(!is.na(Daily_activity))

library(ggplot2)
ggplot(dataa, aes(x = Daily_activity, fill = TYPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Daily activity", y = "Count", fill = "Torpor Type",
       title = "Counts of Torpor Strategy by Activity Pattern") +
  theme_minimal()
 
sum(is.na(data$Daily_activity))
sum (!is.na(data$Daily_activity))

##includes non dormant mammal species as well:
##is the trend similar for dormant as well as non-dormant mammals?

library(readxl)
library(dplyr)

data1 <- read_excel("C:\\Users\\AARYA\\Downloads\\trait_data_imputed_copy.xlsx", sheet = "Sheet2")

data_cleaned <- data1 %>%
  filter(!is.na(Activity_Cycle))

library(ggplot2)
ggplot(data_cleaned, aes(x= Activity_Cycle, fill= TYPE))+
  geom_bar(position = "dodge") +
  labs(x = "Activity pattern", y = "count", fill = "TYPE", title= "Counts of Torpor Strategy by Activity Pattern")+
  theme_minimal()

torpor_counts_diurnal <- data_cleaned %>%
  filter(Activity_Cycle == "diurnal", TYPE %in% c("DT", "HIB")) %>%
  count(TYPE)

print(torpor_counts_diurnal)

table(data1$Activity_Cycle)

##16/06/25

library(readxl)
library(dplyr)


data1 <- read_excel("C:\\Users\\AARYA\\Downloads\\trait_data_imputed_copy.xlsx", sheet = "Sheet1")

##population of dormant vs normothermic animals

data_cleaned <- data1 %>%
  filter(!is.na(activity_cycle), !is.na(hibernation_torpor)) %>%
  mutate(activity_cycle = case_when(
    activity_cycle == 1 ~ "Nocturnal",
    activity_cycle == 2 ~ "Cathemeral/Crepuscular",
    activity_cycle == 3 ~ "Diurnal",
    TRUE ~ as.character(activity_cycle)
  ))

library(ggplot2)
ggplot(data_cleaned, aes(x = activity_cycle, fill = factor(hibernation_torpor))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "darkgreen"),
                    labels = c("Non Dormant", "Dormant"),
                    name = "Dormancy Status") +
  labs(x = "Activity Pattern",
       y = "Count",
       title = "Torpor Counts by Activity Pattern") +
  theme_minimal()


b <- data_cleaned %>%
  filter(activity_cycle %in% c("Nocturnal","Cathemeral/Crepuscular","Diurnal")) %>%
  group_by(activity_cycle, hibernation_torpor) %>%
  summarise(Count = n(), .groups = "drop")
print(b)


percentage_dormant <- data_cleaned %>%
  group_by(activity_cycle) %>%
  summarise(
    Total = n(),
    Dormant = sum(hibernation_torpor == 1),
    Percent_Dormant = round((Dormant / Total) * 100, 2)
  )

print(percentage_dormant)




##Bootstrapping with replacement- for testing the accuracy of the spread of dormancy rate with respect to activity pattern

library(readxl)
library(dplyr)

data1 <- read_excel("C:\\Users\\AARYA\\Downloads\\trait_data_imputed_copy.xlsx", sheet = "Sheet1")


data_cleaned <- data1 %>%
  filter(!is.na(activity_cycle), !is.na(TYPE)) %>%
  mutate(Dormant = ifelse(TYPE %in% c("DT", "HIB", "BO"), 1, 0),
         activity_cycle = case_when(
           activity_cycle == 1 ~ "Nocturnal",
           activity_cycle == 2 ~ "Cathemeral/Crepuscular",
           activity_cycle == 3 ~ "Diurnal",
           TRUE ~ as.character(activity_cycle)
         )) %>%
  filter(!is.na(activity_cycle))  


bootstrap_dormancy <- function(df, n_iterations = 1000, sample_size = 300) {
  results <- data.frame()
  
  for (i in 1:n_iterations) {
    
    sampled <- df %>%
      group_by(activity_cycle) %>%
      sample_n(size = sample_size, replace = TRUE) %>%
      ungroup()
    
    summary <- sampled %>%
      group_by(activity_cycle) %>%
      summarise(Dormancy_rate = mean(Dormant), .groups = "drop") %>%
      mutate(Iteration = i)
    
    results <- bind_rows(results, summary)
  }
  
  return(results)
}


set.seed(123)
bootstrap_results <- bootstrap_dormancy(data_cleaned, n_iterations = 1000, sample_size = 300)


library(ggplot2)
ggplot(bootstrap_results, aes(x = Dormancy_rate, colour = activity_cycle)) +
  geom_density(alpha = 0.5) +
  labs(title = "Bootstrapped Dormancy Proportions by Activity Pattern",
       x = "Dormancy Rate", y = "Density") +
  theme_minimal()

##confidence interval
bootstrap_results %>%
  group_by(activity_cycle) %>%
  summarise(
    lower_95 = quantile(Dormancy_rate, 0.025),
    upper_95 = quantile(Dormancy_rate, 0.975),
    median_rate = median(Dormancy_rate)
  )


##Bootstrap 2- fixed mistakes 

library(readxl)
library(dplyr)

data1 <- read_excel("C:\\Users\\AARYA\\Downloads\\trait_data_imputed_copy.xlsx", sheet = "Sheet1")


data_cleaned <- data1 %>%
  filter(!is.na(activity_cycle), !is.na(hibernation_torpor)) %>%
  mutate(Dormant = as.numeric(hibernation_torpor),
         activity_cycle = case_when(
           activity_cycle == 1 ~ "Nocturnal",
           activity_cycle == 2 ~ "Cathemeral/Crepuscular",
           activity_cycle == 3 ~ "Diurnal",
           TRUE ~ as.character(activity_cycle)
         ) 
         )%>%
  filter(!is.na(activity_cycle))  

 colnames(data1)        

bootstrap_dormancy <- function(df, n_iterations = 1000, sample_size = 300) {
  results <- data.frame()
  
  for (i in 1:n_iterations) {
    
    sampled <- df %>%
      group_by(activity_cycle) %>%
      sample_n(size = sample_size, replace = TRUE) %>%
      ungroup()
    
    summary <- sampled %>%
      group_by(activity_cycle) %>%
      summarise(Dormancy_rate = mean(Dormant), .groups = "drop") %>%
      mutate(Iteration = i)
    
    results <- bind_rows(results, summary)
  }
  
  return(results)
}


set.seed(123)
bootstrap_results <- bootstrap_dormancy(data_cleaned, n_iterations = 1000, sample_size = 300)


library(ggplot2)
ggplot(bootstrap_results, aes(x = Dormancy_rate, fill = activity_cycle)) +
  geom_density(alpha = 0.5) +
  labs(title = "Bootstrapped Dormancy Proportions by Activity Pattern",
       x = "Dormancy Rate", y = "Density") +
  theme_minimal()


##Chi square test
table <- table(data_cleaned$activity_cycle, data_cleaned$Dormant)
View(table)
chisq.test(table)

##binary logistic regression
model <- glm(Dormant ~ activity_cycle, data = data_cleaned, family = "binomial")
summary(model)
exp(-4.2195) #odds of dormancy for baseline group = 0.014 (low odds)
exp(1.7096)  ##diurnals are 5.52x more likely to show dormancy as compared to baseline
exp(2.7622)  ##nocturnals are 15.83x more likely to show dormancy as compared to baseline














