## ---- oolong-intro
m37 = read_rds("R/data/model37.rds")
out = read_rds("R/data/out.rds")

# Erstellen eines Tests 
m37_oolong = create_oolong(
  input_model = m37, # Modell, das wir testen wollen
  input_corpus = out$meta$txt, # Korpus, auf dem Modell basiert; können wir aus "out" für stminsights nehmen
  use_frex_words = TRUE, # FREX-Features in beiden Tests nutzen
  n_top_terms = 5, # Zahl der korrekten Features im word intrusion test
  difficulty = 0.5, # Schwierigkeit des word intrusion tests; 0.5 = frexweight, das wir zur Interpretation genutzt haben
  bottom_terms_percentile = 0.4, # Definition der intruder words; hier: haben theta < 0.4
  n_topiclabel_words = 10, # Zahl der Features, die als Label im topic intrusion test angezeigt werden
  n_top_topics = 2, # Zahl der besten Topics, die für ein Dokument gezeigt werden
  exact_n = 5 # Zahl der Dokumente für topic intrusion test (alternativ frac für Anteil); in echtem Test mehr Dokumente codieren, hier nur 5, damit Demo nicht so lange dauert
                           )

# Ausführen der Tests; Durchführen interaktiv in Viewer
m37_oolong$do_word_intrusion_test()
m37_oolong$do_topic_intrusion_test()
# Beenden des Tests
m37_oolong$lock()
# Test-Ergebnisse
m37_oolong_res = m37_oolong %>% summarise_oolong()
m37_oolong_res
