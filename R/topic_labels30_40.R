# Qualitativer Modellvergleich mit Hilfe der Funktion complete_labels()

# load results
source("R/complete_labels.R")
load("R/data/models30_40.rdata")

# Aus topic_labels.R
m30_topic_labels = tribble(
  ~topic, ~label,
  1,      "Alter von Babies",
  2,      "Reaktion auf Impfung (MMR)",
  3,      "Zweifel und Unischerheit",
  4,      "Familienkommunikation",
  5,      "Entwicklung über die Zeit (in D)",
  6,      "Rolle der Ärzte",
  7,      "Informationsquellen (Stiko)",
  8,      "Fieber",
  9,      "Schutz von / Gefahr für andere/n Kinder",
  10,     "Verschiedene Kombi-Impfstoffe",
  11,     "Nebenwirkungen von Impfungen",
  12,     "Großes Risiko, schwere gesundheitliche Konsequenzen",
  13,     "Urlaub mit Baby, Umgang mit Baby",
  14,     "Ansteckende Krankheiten",
  15,     "Danke für Antworten",
  16,     "Grippe-Impfung",
  17,     "Meinungen und Informationen",
  18,     "Diskussion",
  19,     "Schlafen und (nachts) trinken von Babies",
  20,     "Beziehungsprobleme",
  21,     "Nestschutz",
  22,     "Impfschutz in der Schwangerschaft (insb. Röteln)",
  23,     "UNKLAR [1]",
  24,     "Frühe Kombi-Impfungen",
  25,     "Arbeiten und Berufsverbot",
  26,     "Außernandersetzungen",
  27,     "Zeitliche Abstände",
  28,     "Evidenzlage in der Impfdebatte",
  29,     "Meta-Kommunikation",
  30,     "UNKLAR [2]"
)

# Labels von k = 30 auf andere bis k = 35 übertragen, als CSV ausgeben
# Liste der Modelle für Input
blb = mget(paste0("m", 30:35))
complete_labels(.reference_labels = m30_topic_labels,
                .stm_list = blb, csv = TRUE, n_features = 10, threshold = 6)

# laden der aktualisierten k = 35 als Referenz
m35_topic_labels = read_csv("labels_k35.csv") %>% 
  select(-n_termoverlap)
# Liste der Modelle für Input
blb = mget(paste0("m", 35:40))
# 35 --> 36, ..., 40
complete_labels(.reference_labels = m35_topic_labels,
                .stm_list = blb, csv = TRUE, n_features = 10, threshold = 6)


# Skript zum Durchgehen der Topics (alternativ mit {stminsights})
# Erstellen eines Datensatzes mit den typischsten Dokumenten
top_docs = tidy(m37, "gamma") %>% 
  arrange(desc(gamma)) %>% 
  group_by(topic) %>% 
  slice(1:20) %>% 
  mutate(rank = 1:20) %>% 
  left_join(mutate(select(out$meta, txt), document = 1:n())) %>% 
  mutate(out = paste0(round(gamma, 2), ": ", txt))

# Ausgabe der typischen Feature für ein Topic (hier Topic 1)
TO = 18

m37 %>% 
  labelTopics(n = 10, topics = 1:37)
# Ausgabe der typischen Texte für ein Topic (hier Topic 1)
top_docs %>%
  filter(topic == TO) %>% 
  filter(rank %in% 1:10) %>% 
  .$out %>% 
  str_squish() %>% 
  cat(sep = "\n\n")
