---
title: "Comparison of Marvel and DC Characters"
author: "Team Members: [Jizhou Cheng, Jiahao Li]"
date: "2024-12-18"
format: pdf
appendix: true
---

# Introduction

This report aims to compare Marvel and DC characters based on three key attributes: **eye color**, **alignment**, and **gender**. The purpose of this analysis is to identify patterns and differences between the two companies' character designs, providing insights into their creative strategies and diversity.

We used publicly available data sources, cleaned the raw data, and visualized the results through tables and charts. Statistical tests were performed to validate the observed differences.

# Purpose

The purpose of this study was to analyze differences in the eye color, alignment, and gender distributions of Marvel and DC characters to identify patterns and trends in their character designs. This analysis aims to uncover:

-   Differences in creative approaches between the two companies.
-   Insights into character diversity and design trends.
-   Key aspects that make each company's characters unique.

# Data Description

The dataset contains the following columns:

-   **`page_id`**: Unique ID for the character's page.
-   **`name`**: Name of the character.
-   **`eye`**: Eye color of the character.
-   **`align`**: Alignment (Good, Neutral, Evil).
-   **`sex`**: Gender of the character (Male, Female, etc.).
-   **`gsm`**: Gender or sexual minority status.
-   **`alive`**: Character status (alive or deceased).
-   **`company`**: Data source (Marvel or DC).

# Methods

## Data Import and Cleaning

The raw data files were cleaned and processed as follows:

```{r, echo=TRUE, message=FALSE, warning=FALSE}
# Load necessary packages
library(dplyr)
library(ggplot2)
library(knitr)

# Import raw data
marvel <- read.csv("~/Downloads/marvel-wikia-data.csv")
dc <- read.csv("~/Downloads/dc-wikia-data.csv")

# Standardize column names
names(marvel) <- tolower(trimws(names(marvel)))
names(dc) <- tolower(trimws(names(dc)))

# Combine datasets
combined_data <- bind_rows(
  marvel %>% mutate(company = "Marvel"),
  dc %>% mutate(company = "DC")
)

# Remove missing or blank values
cleaned_data <- combined_data %>%
  filter(!is.na(name) & name != "") %>%
  mutate(
    eye = tolower(trimws(eye)),
    align = tolower(trimws(align)),
    sex = tolower(trimws(sex))
  )

# Save cleaned data to CSV
write.csv(cleaned_data, "~/Downloads/cleaned-comic-characters.csv", row.names = FALSE)
```

## Variables of Interest

The three attributes analyzed were: - **Eye Color**: Distribution and comparison of eye colors across Marvel and DC characters. - **Alignment**: Analysis of alignments (Good, Neutral, Evil) for characters in each company. - **Gender**: Examination of gender distributions.

# Results

## Eye Color Distribution

### Table: Eye Color Distribution

```{r, echo=FALSE, message=FALSE, warning=FALSE}
eye_distribution <- cleaned_data %>%
  group_by(company, eye) %>%
  summarise(count = n(), .groups = "drop")

# Table
knitr::kable(eye_distribution, caption = "Eye Color Distribution by Company")
```

### Chart: Characters with the Most Common Eye Color: Blue Eyes in Marvel and DC

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Plot eye color with labels
ggplot(eye_distribution, aes(x = eye, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Marvel and DC Characters: Blue Eyes Are Most Common", x = "Eye Color", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Alignment Distribution

### Table: Alignment Distribution

```{r, echo=FALSE, message=FALSE, warning=FALSE}
align_distribution <- cleaned_data %>%
  group_by(company, align) %>%
  summarise(count = n(), .groups = "drop")

# Table
knitr::kable(align_distribution, caption = "Alignment Distribution by Company")
```

### Chart: Characters with the Most Common Alignment: Good Characters in Marvel and DC

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Plot alignment with labels
ggplot(align_distribution, aes(x = align, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Marvel and DC Characters: Good Characters Are Most Common", x = "Alignment", y = "Count")
```

### Chi-Square Test for Alignment

To determine whether the alignment distributions between Marvel and DC are statistically different, we performed a Chi-Square Test.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Contingency table for alignment and company
align_contingency <- xtabs(~ align + company, data = cleaned_data)

# Perform chi-square test
align_test <- chisq.test(align_contingency)

# Print results
align_test

# Check expected values
align_test$expected
```

**Results**: - **Chi-Square Statistic**: ( X\^2 = 604.86 )\
- **Degrees of Freedom**: 4\
- **p-value**: ( \< 2.2e-16 )

**Conclusion**: Since the p-value is extremely small, we reject the null hypothesis. The alignment distributions between Marvel and DC characters are statistically significant.

------------------------------------------------------------------------

## Gender Distribution

### Table: Gender Distribution

```{r, echo=FALSE, message=FALSE, warning=FALSE}
gender_distribution <- cleaned_data %>%
  group_by(company, sex) %>%
  summarise(count = n(), .groups = "drop")

# Table
knitr::kable(gender_distribution, caption = "Gender Distribution by Company")
```

### Chart: Characters with the Most Common Gender: Male Characters in Marvel and DC

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Plot gender distribution with labels
ggplot(gender_distribution, aes(x = sex, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Marvel and DC Characters: Male Characters Are Most Common", x = "Gender", y = "Count")
```

# Summary and Conclusions

### Key Observations

1.  **Eye Color**:
    -   Marvel characters exhibit a wider variety of eye colors, including gold and red.
    -   DC characters primarily feature traditional eye colors like blue and brown.
2.  **Alignment**:
    -   Marvel has more neutral characters compared to DC.
    -   DC focuses on clear distinctions between good and bad characters.
3.  **Gender**:
    -   Both companies are male-dominated.
    -   Marvel features slightly more female and non-traditional gender characters.

### Conclusion

Marvel demonstrates a greater emphasis on diversity in eye color, alignment, and gender, while DC tends to focus on traditional traits and roles. These differences reflect distinct creative approaches and target audiences.

# References

-   Data source: [FiveThirtyEight Comic Characters](https://github.com/fivethirtyeight/data/tree/master/comic-characters)
-   Marvel data: [Marvel Wikia](https://marvel.wikia.com/)
-   DC data: [DC Wikia](https://dc.wikia.com/)

# Appendix

```{r codeAppend, ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE}
```
