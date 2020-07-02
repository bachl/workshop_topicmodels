# Labels für die Modelle mit k=30 und k=60
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
TO = 30

m30 %>% 
  labelTopics(n = 10, topics = TO)
# Ausgabe der typischen Texte für ein Topic (hier Topic 1)
top_docs %>%
  filter(topic == TO) %>% 
  filter(rank %in% 1:10) %>% 
  .$out %>% 
  str_squish() %>% 
  cat(sep = "\n\n")

# Datensatz für Topic Labels
# Bis Topic 30 weiterführen
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
# Allgemeiner Eindruck: Noch ein paar "vermischte" Topics, ein paar mehr Topics könnten sinnvoll sein


# Beispiel für das Modell mit k = 60
# Erstellen eines Datensatzes mit den typischsten Dokumenten
top_docs = tidy(m60, "gamma") %>% 
  arrange(desc(gamma)) %>% 
  group_by(topic) %>% 
  slice(1:20) %>% 
  mutate(rank = 1:20) %>% 
  left_join(mutate(select(out$meta, txt), document = 1:n())) %>% 
  mutate(out = paste0(round(gamma, 2), ": ", txt))

# Ausgabe der typischen Feature für ein Topic (hier Topic 1)
TO = 34

m60 %>% 
  labelTopics(n = 10, topics = TO)
# Ausgabe der typischen Texte für ein Topic (hier Topic 1)
top_docs %>%
  filter(topic == TO) %>% 
  filter(rank %in% 1:10) %>% 
  .$out %>% 
  str_squish() %>% 
  cat(sep = "\n\n")

# Datensatz für Topic Labels
# Bis Topic 60 weiterführen
m60_topic_labels = tribble(
  ~topic, ~label,
  1,      "Alter bei frühkindlichen Impfungen",
  2,      "Reaktion auf Impfung",
  3,      "Zweifel und Unischerheit",
  4,      "UNKLAR [1]",
  # 4,      "Familienkommunikation",
  5,      "Entwicklung über die Zeit",
  6,      "Rolle der (Kinder)Ärzte und Hebammen",
  7,      "Informationsquellen (insb. Hirte)",
  8,      "Fieber [Aber ohne *Fieber*]",
  9,      "Verantwortung der Eltern für die Impfentscheidung",
  # 9,      "Schutz von / Gefahr für andere/n Kinder",
  10,     "Vetragen von Impfungen",
  # 10,     "Verschiedene Kombi-Impfstoffe",
  11,     "Nebenwirkungen von Impfungen",
  12,     "Schuldzuweisungen",
  # 12,     "Großes Risiko, schwere gesundheitliche Konsequenzen",
  13,     "Streit",
  14,     "Schlimmere und weniger schlimme Verläufe und Reaktionen",
  # 14,     "Ansteckende Krankheiten",
  15,     "Ausdrücken / Vermitteln von Beruhigung",
  # 15,     "Danke für Antworten",
  16,     "Grippe-Impfung",
  17,     "Meinungen und Informationen",
  18,     "UNKLAR [2]",
  19,     "Urlaub mit Baby, Umgang mit Baby",
  # 19,     "Schlafen und (nachts) trinken von Babies",
  20,     "Beziehungsprobleme",
  21,     "UNKLAR [3]",
  # 21,     "Nestschutz",
  22,     "UNKLAR [4]",
  # 22,     "Impfschutz in der Schwangerschaft (insb. Röteln)",
  23,     "Reaktionen an der Haut",
  24,     "Meningokokken",
  # 24,     "Frühe Kombi-Impfungen",
  25,     "Kontakt mit / unter Kindenr",
  # 25,     "Arbeiten und Berufsverbot",
  26,     "Rat des Kinderarzts",
  # 26,     "Außernandersetzungen",
  27,     "Zeitliche Abstände der Impfungen",
  28,     "Evidenzlage in der Impfdebatte",
  29,     "UNKLAR [5]",
  # 29,     "Meta-Kommunikation",
  30,     "Bluttests",
  31,     "Fieber [mit Nennung]",
  32,     "UNKLAR [6]",
  33,     "UNKLAR [7]",
  34,     ""
)
# Ab hier schon recht klar, dass 60 Topics zu feingliedrig sind
# zu viele nicht in Hinblick auf Forschungsinteresse zu interpretierende Topics


# Dann probieren wir mal k = 40
# Erstellen eines Datensatzes mit den typischsten Dokumenten
top_docs = tidy(m40, "gamma") %>% 
  arrange(desc(gamma)) %>% 
  group_by(topic) %>% 
  slice(1:20) %>% 
  mutate(rank = 1:20) %>% 
  left_join(mutate(select(out$meta, txt), document = 1:n())) %>% 
  mutate(out = paste0(round(gamma, 2), ": ", txt))

# Ausgabe der typischen Feature für ein Topic (hier Topic 1)
TO = 40
m40 %>% 
  labelTopics(n = 10, topics = TO)
# Ausgabe der typischen Texte für ein Topic (hier Topic 1)
top_docs %>%
  filter(topic == TO) %>% 
  filter(rank %in% 1:10) %>% 
  .$out %>% 
  str_squish() %>% 
  cat(sep = "\n\n")

# Datensatz für Topic Labels
m40_topic_labels = tribble(
  ~topic, ~label,
  1,      "Alter bei frühkindlichen Impfungen",
  2,      "Reaktion auf Impfung (v.a. MMR)",
  3,      "Zweifel und Unsicherheit",
  4,      "Familienkommunikation",
  5,      "Entwicklung über die Zeit (in D)",
  6,      "Fragen und Vermutungen",
  7,      "Informationsquellen (insb. Hirte)",
  8,      "Maßnahmen gegen Fieber",
  9,      "Gefahr von ungeimpften Kindern",
  10,     "Bestehender Impfschutz",
  11,     "Nebenwirkungen von Impfungen",
  12,     "Großes Risiko, schwere gesundheitliche Konsequenzen",
  13,     "Urlaub mit Baby, Umgang mit Baby",
  14,     "Infekte und Fieber",
  15,     "Danke für Antworten",
  16,     "Grippe-Impfung",
  17,     "Meinungen und Informationen",
  18,     "Diskussion",
  19,     "(Keine) Sorgen machen",
  20,     "Beziehungsprobleme",
  21,     "Nestschutz",
  22,     "Impfschutz in der Schwangerschaft (insb. Röteln)",
  23,     "(Kinder)Arzt und Hautprobleme",
  24,     "Frühe Kombi-Impfungen",
  25,     "Arbeiten und Berufsverbot",
  26,     "Beratung (auch durch Ärzte)",
  27,     "Zeitliche Abstände",
  28,     "Evidenzlage in der Impfdebatte",
  29,     "Meta-Kommunikation",
  30,     "Bluttests",
  31,     "Leichte Reaktionen auf Impfung",
  32,     "Meta-Kommunikation 2",
  33,     "Gute Besserung",
  34,     "Liebe Grüße",
  35,     "Zecken und FSME",
  36,     "Emojis",
  37,     "Schwere Kinderkrankheiten und ihre Folgen",
  38,     "Mutter und Kind",
  39,     "Nachts trinken und schlafen",
  40,     "UNKLAR [1]"
)
# Allgemeiner Eindruck: Zwischen 30 und 40 Topics könnte eine gute Lösung sein; evtl. auch etwas mehr als 40 Topics, dann aber vermutlich auch wieder etwas mehr irrelevante / nicht interpretierbare Topics
