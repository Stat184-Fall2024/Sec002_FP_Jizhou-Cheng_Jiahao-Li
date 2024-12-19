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
  geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.5, size = 2) +
  labs(title = "Characters with the Most Common Eye Color: Blue Eyes in Marvel and DC", x = "Eye Color", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### Chi-Square Test for Eye Color

To determine whether the eye color distributions between Marvel and DC are statistically different, we performed a Chi-Square Test.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Contingency table for eye color and company
eye_contingency <- xtabs(~ eye + company, data = cleaned_data)

# Perform chi-square test
eye_test <- chisq.test(eye_contingency)

# Print results
eye_test

# Check expected values
eye_test$expected
```

#### **Results:**

-   **Chi-Square Statistic**: 387.13

-   **Degrees of Freedom**: 26

-   **p-value**: \< 2.2e-16

#### **Conclusion:**

The p-value is significantly less than 0.05, indicating that the differences in eye color distributions between Marvel and DC are statistically significant. This suggests that the two companies have distinct creative strategies for character eye color, with Marvel showing a broader range of unique eye colors compared to DC.

------------------------------------------------------------------------

## Alignment Distribution

### Table: Alignment Distribution

```{r, echo=FALSE, message=FALSE, warning=FALSE}
align_distribution <- cleaned_data %>%
  group_by(company, align) %>%
  summarise(count = n(), .groups = "drop")

# Table
knitr::kable(align_distribution, caption = "Alignment Distribution by Company")
```

### Chart: Characters with the Most Common Alignment: Bad Characters in Marvel and DC

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Plot alignment with labels
ggplot(align_distribution, aes(x = align, y = count, fill = company)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Characters with the Most Common Alignment: Bad Characters in Marvel and DC", x = "Alignment", y = "Count")
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

#### **Results:**

-   **Chi-Square Statistic**: 604.86

-   **Degrees of Freedom**: 4

-   **p-value**: \< 2.2e-16

#### **Conclusion:**

The alignment distribution also shows significant differences between Marvel and DC characters. Marvel tends to have more characters with neutral and diverse alignments, while DC shows a stronger focus on traditional "good" and "evil" characters.

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
  geom_text(aes(label = count), position = position_dodge(width = 0.6), vjust = -0.5, size = 3) +
  labs(title = "Characters with the Most Common Gender: Male Characters in Marvel and DC", x = "Gender", y = "Count")
```

### Chi-Square Test for Gender

To determine whether the gender distributions between Marvel and DC are statistically different, we performed a Chi-Square Test.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# Contingency table for gender and company
gender_contingency <- xtabs(~ sex + company, data = cleaned_data)

# Perform chi-square test
gender_test <- chisq.test(gender_contingency)

# Print results
gender_test

# Check expected values
gender_test$expected
```

#### **Results:**

-   **Chi-Square Statistic**: 255.67

-   **Degrees of Freedom**: 6

-   **p-value**: \< 2.2e-16

#### **Conclusion:**

The gender distribution analysis reveals significant statistical differences between Marvel and DC. While both companies have a predominance of male characters, Marvel includes slightly more non-binary and diverse gender representations than DC.

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

-   Data source: [FiveThirtyEight Comic Characters](https://github.com/fivethirtyeight/data/tree/master/comic-characters).
-   Marvel data: [Marvel Wikia](https://marvel.wikia.com/)
-   DC data: [DC Wikia](https://dc.wikia.com/)

---

## Contact
If you have any questions or are interested in collaboration, please contact me:

- **Email**:Jizhou Cheng:[jxc6668@psu.edu](mailto:jxc6668@psu.edu)

