# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the dataset
data <- read.csv("space_nat.csv")

# Clean and tidy data (melt into long format)
tidy_data <- data %>%
  pivot_longer(cols = -c(gender, age), names_to = "Question", values_to = "Response") %>%
  filter(!is.na(Response)) # Remove missing responses

# Custom labels for questions
custom_labels <- c(
  "planets" = "Exploration of Planets",
  "materials" = "Development of New Materials",
  "research" = "Advancing Scientific Research",
  "tech" = "Technology Innovation",
  "asteroids" = "Asteroid Mining",
  "climate" = "Climate Change Monitoring",
  "moon" = "Moon Exploration",
  "mars" = "Mars Colonization",
  "safe travel" = "Ensuring Safe Space Travel",
  "peace" = "Promoting Peaceful Cooperation",
  "weapons" = "Defense and Security"
)

# Summarize data for overall mean
overall_summary <- tidy_data %>%
  group_by(Question) %>%
  summarise(
    Mean = mean(Response, na.rm = TRUE),
    SE = sd(Response, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Question = custom_labels[Question],
    Question = reorder(Question, Mean)
  )

# Summarize by gender
gender_summary <- tidy_data %>%
  group_by(Question, gender) %>%
  summarise(
    Mean = mean(Response, na.rm = TRUE),
    SE = sd(Response, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(Question = custom_labels[Question])

# Summarize by age group
age_summary <- tidy_data %>%
  group_by(Question, age) %>%
  summarise(
    Mean = mean(Response, na.rm = TRUE),
    SE = sd(Response, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(Question = custom_labels[Question])

# Overall aggregate plot
aggregate_plot <- ggplot(overall_summary, aes(x = Question, y = Mean)) +
  geom_col(fill = "gray50", color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, color = "black") +
  coord_flip() +
  labs(
    title = "Overall Public Perceptions of NASA and Space Commercialization",
    x = "Purpose",
    y = "Mean Response",
    caption = "Data source: NASA Survey"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray70")
  )

# Plot by gender
gender_plot <- ggplot(gender_summary, aes(x = Question, y = Mean, fill = gender)) +
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = position_dodge(0.9), width = 0.2) +
  coord_flip() +
  labs(
    title = "Public Perceptions by Gender",
    x = "Purpose",
    y = "Mean Response",
    caption = "Data source: NASA Survey"
  ) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  theme_minimal(base_size = 15)

# Plot by age group
age_plot <- ggplot(age_summary, aes(x = Question, y = Mean, fill = age)) +
  geom_col(position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = position_dodge(0.9), width = 0.2) +
  coord_flip() +
  labs(
    title = "Public Perceptions by Age Group",
    x = "Purpose",
    y = "Mean Response",
    caption = "Data source: NASA Survey"
  ) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  theme_minimal(base_size = 15)

# Print all plots
print(aggregate_plot)
print(gender_plot)
print(age_plot)
