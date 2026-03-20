library(tidyverse)
library(tidytext)
library(readxl)

df <- read_excel("Borges_Cleaned_Final.xlsx")
stories_df <- df %>% 
  filter(str_detect(tolower(genre), "story|cuento|fiction|ficción|short"))

# Unnest by sentence to calculate length and punctuation
sentences_df <- stories_df %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(
    word_count = str_count(sentence, "\\w+"),
    punct_count = str_count(sentence, "[[:punct:]]")
  )

# Average words and punctuation per sentence by year (normalization)
density_by_year <- sentences_df %>%
  group_by(year) %>%
  summarize(
    avg_words = mean(word_count, na.rm = TRUE),
    avg_punct = mean(punct_count, na.rm = TRUE)
  )

# Plot
ggplot(density_by_year, aes(x = year)) +
  geom_line(aes(y = avg_words, color = "Words per Sentence"), linewidth = 1) +
  geom_line(aes(y = avg_punct * 5, color = "Punctuation per Sentence (Scaled x5)"), linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./5, name = "Avg Punctuation")) +
  labs(title = "Information Density in Borges' Stories Over Time",
       x = "Year", y = "Avg Words per Sentence") +
  theme_minimal()
