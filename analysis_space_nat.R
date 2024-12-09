# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the dataset
data <- read.csv("space_nat.csv")

# Clean and tidy data (melt into long format)
tidy_data <- data %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response)) # Remove missing responses

# Summarize the data for plotting
summary_data <- tidy_data %>%
  group_by(Question) %>%
  summarise(
    Mean = mean(Response, na.rm = TRUE),
    SE = sd(Response, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Question = str_replace_all(Question, "_", " "), # Clean up question labels
    Question = str_to_title(Question) # Capitalize first letters for aesthetics
  )

# Create the plot
ggplot(summary_data, aes(x = reorder(Question, Mean), y = Mean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  coord_flip() + # Flip coordinates for better readability
  labs(
    title = "Perceived Purpose of NASA/Space Commercialization",
    x = "Question",
    y = "Mean Response",
    caption = "Data source: NASA Survey"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
