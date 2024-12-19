# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the datasets
marvel <- read.csv("~/Downloads/marvel-wikia-data.csv")
dc <- read.csv("~/Downloads/dc-wikia-data.csv")

# Standardize column names to lowercase
names(marvel) <- tolower(names(marvel))
names(dc) <- tolower(names(dc))

# Combine datasets and clean the 'align' column
combined_data <- bind_rows(
  marvel %>% mutate(company = "Marvel"),
  dc %>% mutate(company = "DC")
) %>%
  filter(!is.na(align) & align != "") # Remove missing and blank values

# Find the most common alignment across both companies
global_align <- combined_data %>%
  group_by(align) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1)

# Print the most common alignment
print(global_align)

# Create a non-raw data table with percentages
align_distribution <- combined_data %>%
  group_by(company, align) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))

# Print the non-raw data table
print(align_distribution)

# View the table in RStudio's Viewer for interactive exploration
View(align_distribution)

# Create a bar chart with a formatted title
align_name <- as.character(global_align$align[1])
ggplot(align_distribution, aes(x = align, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = paste("Characters with the Most Common Alignment:", align_name, "in Marvel and DC"),
    x = "Alignment",
    y = "Count"
  ) +
  theme_minimal()

