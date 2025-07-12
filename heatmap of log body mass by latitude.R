library(readxl)
library(dplyr)

file1 <- read_excel("C:/Users/AARYA/Downloads/final_cleaned_dataset.xlsx")  # has Species_SHP
file2 <- read_excel("C:/Users/AARYA/Downloads/geometry range mammals_copy.xlsx", sheet = "Sheet1")  # has SCI_NAME

file1$Species_SHP <- trimws(tolower(file1$Species_SHP))
file2$SCI_NAME    <- trimws(tolower(file2$SCI_NAME))

merged_data <- left_join(file1, file2, by = c("Species_SHP" = "SCI_NAME"))
str(merged_data$Body_mass_new)
merged_data$Body_mass_new <- as.numeric(merged_data$Body_mass_new)


data_binned <- merged_data %>%
  filter(!is.na(mid_lat), !is.na(Body_mass_new), !is.na(TYPE), !is.na(Daily_activity), Body_mass_new > 0) %>%
  mutate(
    Latitude_bin = cut(mid_lat, breaks = seq(-90, 90, by = 10), include.lowest = TRUE),
    Log_BM = log10(Body_mass_new)
  )


heatmap_data <- data_binned %>%
  group_by(Latitude_bin, TYPE, Daily_activity) %>%
  summarise(mean_BM = mean(Log_BM, na.rm = TRUE), .groups = "drop")


library(ggplot2)
library(viridis)

ggplot(heatmap_data, aes(x = Latitude_bin, y = TYPE, fill = mean_BM)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~ Daily_activity, ncol = 2) +
  scale_fill_viridis(
    name = "Log10(Body Mass)",
    option = "plasma",
    limits = c(0.5, 6),  
    na.value = "grey"
  ) +
  labs(
    title = "Heatmap of Log Body Mass by Latitude and Torpor Type",
    x = "Latitude(°)",
    y = "Torpor Type"
  ) +
  coord_fixed() +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey90", color = "NA")
  )


