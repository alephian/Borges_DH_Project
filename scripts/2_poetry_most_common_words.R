
library(tidyverse)
library(tidytext)
library(readxl)

df <- read_excel("Borges_Cleaned_Final.xlsx")

# spanish stopwords
es_stopwords <- data.frame(word = tm::stopwords("spanish"))

# custom stopwords
custom_stopwords <- data.frame(word = c(  "morell", "lussich", "wenceslao", "kenningar", "hákim", "tichborne", 
                                          "bouvard", "illán", "bogle", "pécuchet", 
                                          "hawthorne", "dahlmann", "lónnrot", "otálora", "tlón", "menard", "damián", "wakefield", "aureliano","irala","ferrari","francesca","beatriz","contesté", "golem", "espinosa", "uriarte", "duncan", "orton","bill", "deán", "bahadir", "evaristo","monk","kid","billy","bandeira","zahir","emma", "ruheberg","hladik","funes" ,"azevedo","unwin", "abulcásim", "abenjacán", "loque", "paracelso","buddha","winthrop","simurgh", "encontramos","eguren", "cristián",
                                          "bahadur","runeberg","hladik","treviranus","pén","kilpatrick","dunraven","panonia","albert","loewenthal", "huang","zimmerman","thorpe", "silveira","hladík","ts'ui","uqbar", "masoller", "shih", "quain", "madden", "bojari", "zimmerman","rufino", "paolo", "einarsson", "siddhartha", "villari", "yarmolinskyi", "nathaniel", "daneri","teodelina", "harold", "allaby", "zimmermann", "twirl", "hengist","judas",
                                          "yarmolinsky", "roemerstadt", "farach", "biathanatos", "droctulft", "esuna", "recabarren", "hasidim", "erik", "zunz", "villar", "ashe", "moon", "jerusalem", "gryphius", "averroes", "bojarí", "ryan", "bachelier", "caedmon","aleph", "franz","histriones", "hrón", "agregó", "viterbo", "jaromir","soñará","josafat","luciano","fermín","si", "vez", "aqui","dos", "aquí", "aquel"))

# Combine both stop words
all_stopwords <- bind_rows(es_stopwords, custom_stopwords)

# filter for poetry, divide between eras, tokenize, and count instances
poem_word_counts <- df %>%
  filter(genre == "Poem") %>% # Adjust if your excel says "poetry" or "poem"
  mutate(era = ifelse(year < 1955, "Early (Pre-1955)", "Late / Blind (Post-1955)")) %>%
  unnest_tokens(word, text) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(era, word, sort = TRUE)

# Get the top 15 most common words for each era
top_words <- poem_word_counts %>%
  group_by(era) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, era))

# plot
ggplot(top_words, aes(x = word, y = n, fill = era)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~era, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_manual(values = c("Early (Pre-1955)" = "steelblue", "Late / Blind (Post-1955)" = "darkred")) +
  labs(title = "Borges' Poetry: Most Common Words Before and After Blindness",
       subtitle = "Measuring raw word frequency",
       x = "Most Common Words", y = "Frequency (Raw Count)") +
  theme_minimal()
