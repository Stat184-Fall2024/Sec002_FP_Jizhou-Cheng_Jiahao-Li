# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the datasets
marvel <- read.csv("~/Downloads/marvel-wikia-data.csv")
dc <- read.csv("~/Downloads/dc-wikia-data.csv")

# Standardize column names to lowercase
names(marvel) <- tolower(names(marvel))
names(dc) <- tolower(names(dc))

# Combine datasets and clean the 'eye' column
combined_data <- bind_rows(
  marvel %>% mutate(company = "Marvel"),
  dc %>% mutate(company = "DC")
) %>%
  filter(!is.na(eye) & eye != "") # Remove missing and blank values

# Find the most common eye color across both companies
global_eye <- combined_data %>%
  group_by(eye) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1)

# Print the most common eye color
print(global_eye)

# Create a non-raw data table with percentages
eye_distribution <- combined_data %>%
  group_by(company, eye) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))

# Print the non-raw data table
print(eye_distribution)

# View the table in RStudio's Viewer for interactive exploration
View(eye_distribution)

# Create a bar chart with a formatted title
eye_color_name <- as.character(global_eye$eye[1])
ggplot(eye_distribution, aes(x = eye, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = paste("Characters with the Most Common Eye Color:", eye_color_name, "in Marvel and DC"),
    x = "Eye Color",
    y = "Count"
  ) +
  theme_minimal()

