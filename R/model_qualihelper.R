## ---- model-quali

# Laden der Modelle
load("R/data/models30_40_60.rdata")

# Beispiel für das Modell mit k = 30
# Erstellen eines Datensatzes mit den typischsten Dokumenten
top_docs = tidy(m30, "gamma") %>% 
  arrange(desc(gamma)) %>% 
  group_by(topic) %>% 
  slice(1:20) %>% 
  mutate(rank = 1:20) %>% 
  left_join(mutate(select(out$meta, txt), document = 1:n())) %>% 
  mutate(out = paste0(round(gamma, 2), ": ", txt))

# Ausgabe der typischen Feature für ein Topic (hier Topic 1)
m30 %>% 
  labelTopics(n = 10, topics = 1)
# Ausgabe der typischen Texte für ein Topic (hier Topic 1)
top_docs %>%
  filter(topic == 1) %>% 
  filter(rank %in% 1:10) %>% 
  .$out %>% 
  str_squish() %>% 
  cat(sep = "\n\n")

# Datensatz für Topic Labels
# Bis Topic 30 weiterführen
m30_topic_labels = tribble(
  ~topic, ~label,
  1,      "Alter von Babies",
  2,      "label B",
  3,      "label C"
)
