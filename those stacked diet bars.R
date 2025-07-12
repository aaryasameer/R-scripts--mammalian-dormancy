### 04/07/25 

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)


full_data <- read_excel("C:\\Users\\AARYA\\Downloads\\Combined_MammalDIET_and_MammalDIET2_dataset.xlsx", sheet = "Sheet1")
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")


plot1_data <- full_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore", "Omnivore")) %>%
  group_by(TrophicLevel) %>%
  summarise(Count = n(), .groups = "drop")

p1 <- ggplot(plot1_data, aes(x = "All Mammals", y = Count, fill = TrophicLevel)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  labs(title = "All Mammals", x = NULL, y = "Species Count") +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 11),
    axis.title.y = element_text(size = 12)
  )

plot2_data <- dormant_data %>%
  filter(TrophicLevel %in% c("Carnivore", "Herbivore", "Omnivore"),
         TYPE %in% c("DT", "HIB")) %>%
  group_by(TYPE, TrophicLevel) %>%
  summarise(Count = n(), .groups = "drop")

p2 <- ggplot(plot2_data, aes(x = TYPE, y = Count, fill = TrophicLevel)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  labs(title = "Dormant Mammals", x = "Torpor Type", y = NULL) +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 11),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

final_plot <- p1 + p2 + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "bottom")
print(final_plot)


ggsave("C:/Users/AARYA/Downloads/clean_trophic_dormancy_plot.png", final_plot, width = 10, height = 6, dpi = 300)


### 04/07/25 - Carnivory
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)


full_data <- read_excel("C:\\Users\\AARYA\\Downloads\\Combined_MammalDIET_and_MammalDIET2_dataset.xlsx", sheet = "Sheet1")
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")


get_carnivory_type <- function(mammal, insect) {
  mammal <- ifelse(is.na(mammal), 0, mammal)
  insect <- ifelse(is.na(insect), 0, insect)
  if (mammal == 1 & insect == 1) return("Mammal + Insect")
  else if (mammal == 1) return("MammalEater")
  else if (insect == 1) return("Insectivore")
  else return(NA)
}


full_data$CarnivoryType <- mapply(get_carnivory_type, full_data$MammalEater, full_data$Insectivore)
dormant_data$CarnivoryType <- mapply(get_carnivory_type, dormant_data$MammalEater, dormant_data$Insectivore)


plot1_data <- full_data %>%
  filter(!is.na(CarnivoryType)) %>%
  group_by(CarnivoryType) %>%
  summarise(Count = n(), .groups = "drop")

p1 <- ggplot(plot1_data, aes(x = "All Mammals", y = Count, fill = CarnivoryType)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(title = "All Mammals by Carnivory Type", x = NULL, y = "Count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set2")


plot2_data <- dormant_data %>%
  filter(!is.na(CarnivoryType), TYPE %in% c("DT", "HIB")) %>%
  group_by(TYPE, CarnivoryType) %>%
  summarise(Count = n(), .groups = "drop")

p2 <- ggplot(plot2_data, aes(x = TYPE, y = Count, fill = CarnivoryType)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(title = "Dormant Mammals by Carnivory Type", x = "Torpor Type", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_fill_brewer(palette = "Set2")


final_plot <- p1 / p2 + plot_layout(heights = c(1, 1), guides = "collect")
print(final_plot)
ggsave("C:/Users/AARYA/Downloads/carnivory_clean_vertical.png", final_plot, width = 8, height = 10, dpi = 300)



### 04/07/25 - Herbivory Stacked Bar Graph ###

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

full_data <- read_excel("C:\\Users\\AARYA\\Downloads\\Combined_MammalDIET_and_MammalDIET2_dataset.xlsx", sheet = "Sheet1")
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")


extract_herbivory_type <- function(frug, gran, foli) {
  labels <- c()
  if (!is.na(frug) && frug == 1) labels <- c(labels, "Frugivore")
  if (!is.na(gran) && gran == 1) labels <- c(labels, "Granivore")
  if (!is.na(foli) && foli == 1) labels <- c(labels, "Folivore")
  if (length(labels) == 0) return(NA)
  return(paste(labels, collapse = ", "))
}

# Apply to both datasets
full_data$HerbivoryType <- mapply(extract_herbivory_type,
                                  full_data$Frugivore,
                                  full_data$Granivore,
                                  full_data$Folivore)

dormant_data$HerbivoryType <- mapply(extract_herbivory_type,
                                     dormant_data$Frugivore,
                                     dormant_data$Granivore,
                                     dormant_data$Folivore)


herb_colors <- c(
  "Folivore" = "#1f78b4",                              # blue
  "Frugivore" = "#33a02c",                             # green
  "Granivore" = "#fb9a99",                             # pink
  "Frugivore, Folivore" = "#a6cee3",                   # light blue
  "Frugivore, Granivore" = "#e31a1c",                  # red
  "Granivore, Folivore" = "#b15928",                   # brown
  "Frugivore, Granivore, Folivore" = "#6a3d9a"         # purple
)

# Set levels explicitly
herb_levels <- names(herb_colors)

# ========== PLOT 1: All herbivores ==========
plot1_data <- full_data %>%
  filter(TrophicLevel == "Herbivore", !is.na(HerbivoryType)) %>%
  group_by(HerbivoryType) %>%
  summarise(Count = n(), .groups = "drop")

plot1_data$HerbivoryType <- factor(plot1_data$HerbivoryType, levels = herb_levels)

p1 <- ggplot(plot1_data, aes(x = "All Herbivores", y = Count, fill = HerbivoryType)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  labs(title = "All Herbivores", x = NULL, y = "Count") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = herb_colors) +
  theme(legend.position = "bottom")

# ========== PLOT 2: Dormant herbivores ==========
plot2_data <- dormant_data %>%
  filter(TrophicLevel == "Herbivore", TYPE %in% c("DT", "HIB"), !is.na(HerbivoryType)) %>%
  group_by(TYPE, HerbivoryType) %>%
  summarise(Count = n(), .groups = "drop")

plot2_data$HerbivoryType <- factor(plot2_data$HerbivoryType, levels = herb_levels)

p2 <- ggplot(plot2_data, aes(x = TYPE, y = Count, fill = HerbivoryType)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black") +
  labs(title = "Dormant Herbivores", x = "Torpor Type", y = "Count") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = herb_colors) +
  theme(legend.position = "bottom")

# ========== COMBINE PLOTS ==========
final_plot <- p1 + p2 + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "bottom")
print(final_plot)

# Optional: Save the plot
ggsave("C:/Users/AARYA/Downloads/herbivory_cleaned_plot.png", final_plot, width = 12, height = 6, dpi = 300)


###imsotired
# === 05/07/25 — Omnivore Diet Combinations: All vs Dormant (Final Clean) ===

library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)

full_data <- read_excel("C:\\Users\\AARYA\\Downloads\\Combined_MammalDIET_and_MammalDIET2_dataset.xlsx", sheet = "Sheet1")
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")

subdiet_cols <- c("MammalEater", "Insectivore", "Frugivore", "Granivore", "Folivore")

for (col in subdiet_cols) {
  full_data[[col]] <- suppressWarnings(as.numeric(full_data[[col]]))
  dormant_data[[col]] <- suppressWarnings(as.numeric(dormant_data[[col]]))
}


valid_combos <- c(
  "Folivore + Insectivore",
  "Frugivore + Insectivore",
  "Folivore + Frugivore + Insectivore",
  "Frugivore + Granivore + Insectivore",
  "Folivore + Frugivore + Granivore + Insectivore",
  "Granivore + Insectivore",
  "Folivore + Granivore + Insectivore"
)


get_filtered_combos <- function(df, group_label) {
  df %>%
    filter(TrophicLevel == "Omnivore") %>%
    rowwise() %>%
    mutate(DietCombo = {
      present <- subdiet_cols[which(c_across(all_of(subdiet_cols)) == 1)]
      combo <- if (length(present) == 0) NA else paste(sort(present), collapse = " + ")
      if (combo %in% valid_combos) combo else NA
    }) %>%
    ungroup() %>%
    filter(!is.na(DietCombo)) %>%
    mutate(Group = group_label)
}


all_omnivores <- get_filtered_combos(full_data, "All Omnivores")
dormant_dt     <- get_filtered_combos(dormant_data %>% filter(TYPE == "DT"), "DT")
dormant_hib    <- get_filtered_combos(dormant_data %>% filter(TYPE == "HIB"), "HIB")



common_cols <- intersect(names(all_omnivores), names(dormant_dt))  # or dormant_hib
for (col in common_cols) {
  all_omnivores[[col]] <- as.character(all_omnivores[[col]])
  dormant_dt[[col]] <- as.character(dormant_dt[[col]])
  dormant_hib[[col]] <- as.character(dormant_hib[[col]])
}


combined <- bind_rows(all_omnivores, dormant_dt, dormant_hib)


plot_data <- combined %>%
  group_by(Group, DietCombo) %>%
  summarise(Count = n(), .groups = "drop")


p <- ggplot(plot_data, aes(x = Group, y = Count, fill = DietCombo)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.95) +  # wider bars
  labs(title = "Omnivorous Mammals by Diet Combination",
       x = NULL,
       y = "Number of Species",
       fill = "Diet Combination") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",  # move legend
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12)
  ) +
  scale_fill_viridis_d(option = "C") +
  facet_wrap(~ Group, scales = "free_y")

print(p)
ggsave("Omnivore_DietCombo_Faceted_LegendBottom.png", plot = p, width = 12, height = 8, dpi = 300)


###chi sq. test
full_data <- read_excel("C:\\Users\\AARYA\\Downloads\\Combined_MammalDIET_and_MammalDIET2_dataset.xlsx", sheet = "Sheet1")
dormant_data <- read_excel("C:\\Users\\AARYA\\Downloads\\kissling_modified_dataset.xlsx")


total_counts <- full_data %>%
  count(TrophicLevel) %>%
  rename(Total = n)


dormant_counts <- dormant_data %>%
  count(TrophicLevel) %>%
  rename(Dormant = n)


merged_counts <- left_join(total_counts, dormant_counts, by = "TrophicLevel") %>%
  mutate(Dormant = replace_na(Dormant, 0)) %>%
  mutate(ProportionDormant = Dormant / Total)


merged_counts <- merged_counts %>%
  mutate(NonDormant = Total - Dormant)


tbl <- merged_counts %>%
  select(TrophicLevel, Dormant, NonDormant) %>%
  column_to_rownames("TrophicLevel") %>%
  as.matrix() %>%
  t()


tbl_clean <- tbl[, c("Carnivore", "Herbivore", "Omnivore")]
chisq.test(tbl_clean)

tbl_clean["Dormant", ] / colSums(tbl_clean)

result <- chisq.test(tbl_clean)
result$residuals

library(ggplot2)
library(tidyr)


res_df <- as.data.frame(as.table(result$residuals))

ggplot(res_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(Freq, 2)), size = 5) +
  labs(x = "Diet", y = "Dormancy", fill = "Residual",
       title = "Chi-Square Residuals: Diet vs Dormancy") +
  theme_minimal()








# Strict folivores
strict_folivores <- dormant_data %>%
  filter(Folivore == 1,
         (is.na(Granivore) | Granivore != 1),
         (is.na(Frugivore) | Frugivore != 1))

# Strict granivores
strict_granivores <- dormant_data %>%
  filter(Granivore == 1,
         (is.na(Folivore) | Folivore != 1),
         (is.na(Frugivore) | Frugivore != 1))

# Strict frugivores
strict_frugivores <- dormant_data %>%
  filter(Frugivore == 1,
         (is.na(Folivore) | Folivore != 1),
         (is.na(Granivore) | Granivore != 1))

# Print counts
cat("Strict Folivores:", nrow(strict_folivores), "\n")
cat("Strict Granivores:", nrow(strict_granivores), "\n")
cat("Strict Frugivores:", nrow(strict_frugivores), "\n")


# Strict folivores
strict_folivores <- full_data %>%
  filter(Folivore == 1,
         (is.na(Granivore) | Granivore != 1),
         (is.na(Frugivore) | Frugivore != 1))

# Strict granivores
strict_granivores <- full_data %>%
  filter(Granivore == 1,
         (is.na(Folivore) | Folivore != 1),
         (is.na(Frugivore) | Frugivore != 1))

# Strict frugivores
strict_frugivores <- full_data %>%
  filter(Frugivore == 1,
         (is.na(Folivore) | Folivore != 1),
         (is.na(Granivore) | Granivore != 1))

# Print counts
cat("Strict Folivores:", nrow(strict_folivores), "\n")
cat("Strict Granivores:", nrow(strict_granivores), "\n")
cat("Strict Frugivores:", nrow(strict_frugivores), "\n")



strict_insectivores <- dormant_data %>%
  filter(Insectivore == 1,
         (is.na(MammalEater) | MammalEater != 1))

strict_mammaleaters <- dormant_data %>%
  filter(MammalEater == 1,
         (is.na(Insectivore) | Insectivore != 1))

cat("Strict Insectivores:", nrow(strict_insectivores), "\n")
cat("Strict Mammal Eaters:", nrow(strict_mammaleaters), "\n")



strict_insectivores <- full_data %>%
  filter(Insectivore == 1,
         (is.na(MammalEater) | MammalEater != 1))

strict_mammaleaters <- full_data %>%
  filter(MammalEater == 1,
         (is.na(Insectivore) | Insectivore != 1))

cat("Strict Insectivores:", nrow(strict_insectivores), "\n")
cat("Strict Mammal Eaters:", nrow(strict_mammaleaters), "\n")





clean_data <- dormant_data %>%
  select(TYPE, TrophicLevel, Order_new, Body_mass_new, bio1_mean, bio2_mean, bio4_mean) %>%
  mutate(
    across(c(Body_mass_new, bio1_mean, bio2_mean, bio4_mean), as.numeric),  # numeric only
    across(c(TYPE, TrophicLevel, Order_new), as.factor)  # traits as factors
  ) %>%
  na.omit()

numeric_data <- clean_data[, c("Body_mass_new", "bio1_mean", "bio2_mean", "bio4_mean")]


numeric_data <- scale(clean_data[, c("Body_mass_new", "bio1_mean", "bio2_mean", "bio4_mean")])


set.seed(123)
clusters <- kmeans(scaled_data, centers = 2)


clean_data$Cluster <- as.factor(clusters$cluster)

ggplot(clean_data, aes(x = bio1_mean / 10, y = Body_mass_new, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_log10() +
  labs(
    title = "K-means Clustering of Dormant Mammals",
    x = "Annual Mean Temperature (°C)",
    y = "Body Mass (g, log scale)"
  ) +
  theme_minimal()

table(clean_data$Cluster)  

table(clean_data$Cluster, clean_data$TYPE)

table(clean_data$Cluster, clean_data$TrophicLevel)

table(clean_data$Cluster, clean_data$Order_new)  

##bio2
ggplot(clean_data, aes(x = bio2_mean / 10, y = Body_mass_new, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_log10() +
  labs(
    title = "K-means Clustering of Dormant Mammals",
    x = "Diurnal range",
    y = "Body Mass (g, log scale)"
  ) +
  theme_minimal()

table(clean_data$Cluster)  

table(clean_data$Cluster, clean_data$TYPE)

table(clean_data$Cluster, clean_data$TrophicLevel)

table(clean_data$Cluster, clean_data$Order_new)  

##bio4
ggplot(clean_data, aes(x = bio4_mean / 10, y = Body_mass_new, color = Cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_y_log10() +
  labs(
    title = "K-means Clustering of Dormant Mammals",
    x = "Seasonality",
    y = "Body mass"
  ) +
  theme_minimal()

##carnivores sorted into vertebrates and invertebrates
library(dplyr)
library(ggplot2)

process_carnivores <- function(data, label) {
  data %>%
    filter(TrophicLevel == "Carnivore") %>%
    filter(Vertebrate %in% 0:1, Invertebrate %in% 0:1) %>%  # Keep only 0/1
    mutate(CarnivoreType = case_when(
      Vertebrate == 1 & Invertebrate == 0 ~ "Vertebrate",
      Vertebrate == 0 & Invertebrate == 1 ~ "Invertebrate",
      Vertebrate == 1 & Invertebrate == 1 ~ "Vertebrate + Invertebrate",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(CarnivoreType)) %>%
    mutate(Group = label)
}


# All mammals: no TYPE info
carn_all <- process_carnivores(full_data, "All Mammals") %>%
  mutate(TYPE = "All") %>%  # Add dummy type to match dormant
  select(CarnivoreType, Group, TYPE)

carn_dormant <- process_carnivores(dormant_data, "Dormant Mammals") %>%
  select(CarnivoreType, Group, TYPE)


carn_combined <- bind_rows(carn_all, carn_dormant)

library(ggplot2)


ggplot(carn_combined, aes(x = TYPE, fill = CarnivoreType)) +
  geom_bar(width = 0.7, color = "black") +
  facet_wrap(~ Group, scales = "free_y", labeller = label_wrap_gen(width = 15)) +
  labs(
    title = "Carnivore Diet Composition: All vs Dormant Mammals",
    x = NULL,
    y = "Number of Species",
    fill = "Carnivore Type"
  ) +
  theme_minimal(base_size = 14) 
  




