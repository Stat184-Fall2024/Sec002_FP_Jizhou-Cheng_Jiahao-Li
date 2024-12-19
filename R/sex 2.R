# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the datasets
marvel <- read.csv("~/Downloads/marvel-wikia-data.csv")
dc <- read.csv("~/Downloads/dc-wikia-data.csv")

# Standardize column names to lowercase
names(marvel) <- tolower(names(marvel))
names(dc) <- tolower(names(dc))

# Combine datasets and clean the 'sex' column
combined_data <- bind_rows(
  marvel %>% mutate(company = "Marvel"),
  dc %>% mutate(company = "DC")
) %>%
  filter(!is.na(sex) & sex != "") # Remove missing and blank values

# Find the most common gender across both companies
global_sex <- combined_data %>%
  group_by(sex) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1)

# Print the most common gender
print(global_sex)

# Create a non-raw data table with percentages
sex_distribution <- combined_data %>%
  group_by(company, sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  arrange(desc(percentage))

# Print the non-raw data table
print(sex_distribution)

# View the table in RStudio's Viewer for interactive exploration
View(sex_distribution)

# Create a bar chart with a formatted title
ggplot(sex_distribution, aes(x = sex, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(0.9)) +
  labs(
    title = paste("Characters with the Most Common Gender:", global_sex$sex[1], "in Marvel and DC"),
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal()

