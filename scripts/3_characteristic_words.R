
library(tidyverse)
library(tidytext)
library(quanteda)

borges_clean <- readRDS("borges_clean.rds")

# tokenization
borges_words <- borges_clean %>%
  unnest_tokens(word, text)

# load the standard Spanish stopwords
spanish_stopwords <- stopwords("es")

# removal of proper names and last names that arised while loading the plots to get a better collection of measurable thematic change 
custom_stopwords <- c(
  "morell", "lussich", "wenceslao", "kenningar", "hákim", "tichborne", 
  "bouvard", "illán", "bogle", "pécuchet", 
  "hawthorne", "dahlmann", "lónnrot", "otálora", "tlón", "menard", "damián", "wakefield", "aureliano","irala","ferrari","francesca","beatriz","contesté", "golem", "espinosa", "uriarte", "duncan", "orton","bill", "deán", "bahadir", "evaristo","monk","kid","billy","bandeira","zahir","emma", "ruheberg","hladik","funes" ,"azevedo","unwin", "abulcásim", "abenjacán", "loque", "paracelso","buddha","winthrop","simurgh", "encontramos","eguren", "cristián",
  "bahadur","runeberg","hladik","treviranus","pén","kilpatrick","dunraven","panonia","albert","loewenthal", "huang","zimmerman","thorpe", "silveira","hladík","ts'ui","uqbar", "masoller", "shih", "quain", "madden", "bojari", "zimmerman","rufino", "paolo", "einarsson", "siddhartha", "villari", "yarmolinskyi", "nathaniel", "daneri","teodelina", "harold", "allaby", "zimmermann", "twirl", "hengist",
  "yarmolinsky", "roemerstadt", "farach", "biathanatos", "droctulft", "esuna", "recabarren", "hasidim", "erik", "zunz", "villar", "ashe", "moon", "jerusalem", "gryphius", "averroes", "bojarí", "ryan", "bachelier", "caedmon", "franz","histriones", "hrón", "agregó", "viterbo", "jaromir","soñará","josafat","luciano","fermín", "hexágonos","orbis","servicial", "consideración", "kotsuké", "mississippi",
  "casamiento", "roger", "determinados", "suké", "film", "atender", "bartolomé", "peligrosa", "vicente", "decímetro", "faguet", "diferentes", "guapos", "heard", "jué", "milímetro", "newman", "pirata", "increíbles", "sorteos", "tahafut"
  )

borges_words_clean <- borges_words %>%
  # Remove standard spanish words along the custom list
  filter(!word %in% spanish_stopwords) %>%
  filter(!word %in% custom_stopwords) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(nchar(word) > 2)

# count of the the words per era
era_words <- borges_words_clean %>%
  count(era, word, sort = TRUE)

# the higher the tf_idf score, the more UNIQUE that word is to that specific era.
era_tf_idf <- era_words %>%
  bind_tf_idf(word, era, n)

# get the top 10 most characteristic words per era
top_characteristic_words <- era_tf_idf %>%
  group_by(era) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, era))

# plotting
ggplot(top_characteristic_words, aes(x = tf_idf, y = word, fill = era)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~era, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Borges's Characteristic Words by Era (TF-IDF)",
    subtitle = "Words that uniquely define each period of his writing",
    x = "TF-IDF Score (Uniqueness)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  )

