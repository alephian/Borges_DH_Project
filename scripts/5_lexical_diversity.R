library(tidyverse)
library(tidytext)
library(readxl)

df <- read_excel("Borges_Cleaned_Final.xlsx")

# Calculate Type-Token Ratio (TTR)
lexical_diversity <- df %>%
  unnest_tokens(word, text) %>%
  group_by(year) %>%
  summarize(
    total_words = n(),
    unique_words = n_distinct(word),
    TTR = unique_words / total_words 
  )

# Plot Lexical Diversity over time
ggplot(lexical_diversity, aes(x = year, y = TTR)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point() +
  labs(title = "Borges' Lexical Diversity (Type-Token Ratio) Over Time",
       subtitle = "A downward trend suggests vocabulary simplification",
       x = "Year", y = "Lexical Diversity (TTR)") +
  theme_minimal()

