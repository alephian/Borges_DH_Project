library(dplyr)
library(stringr)

# exception list for paratexts
micro_stories <- c(
  "ARGUMENTUM ORNITHOLOGICUM", "EL IDOLATRADO HOMBRE MUERTO", 
  "DIÁLOGO SOBRE UN DIÁLOGO", "EL CAUTIVO", "MUSEO", "DEL RIGOR EN LA CIENCIA",
  "BORGES Y YO", "EVERYTHING AND NOTHING", "LOS DOS REYES Y LOS DOS LABERINTOS",
  "EL SIMULACRO", "EL TESTIGO", "PARÁBOLA DEL PALACIO",
  "PARÁBOLA DE CERVANTES Y DE QUIJOTE", "INFERNO, I, 32",
  "MUTACIONES", "EL PUÑAL", "LEYENDA", "EPISODIO DEL ENEMIGO",
  "UNA ROSA AMARILLA", "LOS ENIGMAS", "MARTÍN FIERRO",
  "DREAMTIGERS", "DIÁLOGO DE MUERTOS", "LA TRAMA"
)

# Read the raw text file
lines_raw <- readLines("jorge_luis_borges_corpus_master.txt", encoding = "UTF-8")

# 3. Build the dataset, fix and label
corpus_df <- tibble(raw_line = lines_raw) %>%
  filter(str_trim(raw_line) != "") %>%
  
  mutate(is_title = str_detect(raw_line, "^[^a-záéíóúñü0-9]+$") & 
           str_detect(raw_line, "[A-ZÁÉÍÓÚÑ]")) %>%
  
  mutate(doc_id = cumsum(is_title)) %>%
  group_by(doc_id) %>%
  summarize(
    title = str_trim(first(raw_line)),
    text = paste(raw_line[-1], collapse = " ") 
  ) %>%
  ungroup() %>%
  
  mutate(text = str_replace_all(text, "-\\s+", "")) %>%
  mutate(text = str_replace_all(text, "\\s+", " ")) %>%
  filter(text != "") %>%
  
mutate(
  char_count = nchar(text),
  is_paratext = str_detect(title, "PRÓLOGO|PROLOGO|EPÍLOGO|EPILOGO|NOTAS|NOTA|DECLARACIÓN"),
  
  genre = case_when(
    is_paratext == TRUE ~ "Paratext",
    title %in% micro_stories ~ "Story", # R checks the list first!
    char_count < 1500 ~ "Poem",
    TRUE ~ "Story"
  )
) %>%
  select(-is_paratext)

# Save the perfectly labeled file
write.csv(corpus_df, "borges_corpus_needs_years.csv", row.names = FALSE, fileEncoding = "UTF-8")
#read the corrected excel file with corresponding years and titles
df <- read_excel("borges_corpus_needs_years.csv")


df_final <- df %>%
  filter(!is.na(text)) %>%
  
  mutate(
    text_clean = str_trim(text) %>% str_replace_all("\\s+", " "),
    char_count_R = nchar(text_clean)
  ) %>%

  select(-text, -char_count, -Columna1)

# 5. save to begin with stylometry
write.csv(df_final, "Borges_Master_Clean.csv", row.names = FALSE)
