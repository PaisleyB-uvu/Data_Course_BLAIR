library(tidyverse)
utah_tidy <- utah_religions_clean %>%
  pivot_longer(
    cols = -c(County, Pop_2010, Religious, Non_Religious), 
    names_to = "Religion",
    values_to = "Adherents"
  ) %>%
  mutate(
    Denomination_Proportion = Adherents / Pop_2010,
    Non_Religious_Proportion = Non_Religious / Pop_2010
  )
ggplot(utah_tidy, aes(x = Religion, y = Denomination_Proportion, fill = Religion)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Denomination Proportions Across Utah Counties",
    x = "Religious Group",
    y = "Proportion of County Population"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
ggplot(utah_tidy, aes(x = Pop_2010, y = Denomination_Proportion, color = Religion)) +
  geom_point(alpha = 0.8, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.6) +
  scale_x_log10(labels = scales::comma) + 
  facet_wrap(~Religion, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "County Population vs. Proportion of Specific Religious Groups",
    subtitle = "Dotted line shows general trend line (Population on Log10 Scale)",
    x = "County Population (Log Scale)",
    y = "Proportion of County Population"
  ) +
  theme(legend.position = "none")
q1_correlations <- utah_tidy %>%
  group_by(Religion) %>%
  summarize(
    Correlation_with_Population = cor(Pop_2010, Denomination_Proportion, use = "complete.obs"),
    .groups = 'drop'
  )
