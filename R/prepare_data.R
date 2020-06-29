## ---- prepare-data

# Erstellen des Korpus
crps = d %>% 
  mutate(txt = post, # Duplizieren des Post-Texts für Meta-Daten
         date_num = as.numeric(postdate) - max(as.numeric(postdate))) %>% # Numerische Datumsvariable
  corpus(text_field = "post") # Erstellen des Korpus
crps %>% 
  summary(n = 1)

# Stoppwörter
custom_stopwords = c("ab", "aber", "ach", "all", "alle", "allem", "allen", "aller", "alles", "als", "also", "am", "an", "andere", "anderen", "anderes", "anders", "auch", "auf", "aufs", "aus", "bei", "beim", "bin", "bis", "bist", "bzw", "da", "dabei", "dadurch", "daher", "dahin", "damit", "dann", "das", "dass", "daß", "dazu", "dein", "deine", "deinem", "deinen", "deiner", "dem", "den", "denen", "denn", "dennoch", "der", "deren", "des", "deshalb", "deswegen", "dich", "die", "dies", "diese", "diesem", "diesen", "dieser", "dieses", "dir", "doch", "dort", "dran", "drauf", "drin", "drüber", "du", "durch", "durchaus", "eh", "ein", "eine", "einem", "einen", "einer", "eines", "einige", "einigen", "einiges", "einmal", "er", "es", "etc", "etwas", "euch", "euer", "eure", "euren", "für", "fürs", "gegen", "gehabt", "getan", "gewesen", "geworden", "hab", "habe", "haben", "habt", "halt", "hast", "hat", "hatte", "hätte", "hatten", "hätten", "her", "hier", "hin", "hinter", "ich", "ihm", "ihn", "ihnen", "ihr", "ihre", "ihrem", "ihren", "ihrer", "im", "in", "ins", "is", "ist", "ja", "je", "jede", "jedem", "jeden", "jeder", "jedes", "jetzt", "kann", "kannst", "kein", "keine", "keinem", "keinen", "keiner", "können", "könnt", "konnte", "könnte", "könnten", "mach", "mache", "machen", "machst", "macht", "mal", "man", "manche", "mein", "meine", "meinem", "meinen", "meiner", "meines", "mich", "mir", "mit", "muss", "müssen", "musst", "musste", "müsste", "mussten", "na", "nach", "nachdem", "naja", "ne", "nein", "nem", "nen", "ner", "nicht", "nichts", "nix", "noch", "nun", "nur", "ob", "oder", "ohne", "ok", "okay", "raus", "rein", "rum", "schon", "sehr", "sei", "seid", "sein", "seine", "seinem", "seinen", "seiner", "selber", "selbst", "sich", "sie", "sind", "so", "solche", "solchen", "soll", "sollen", "sollte", "sollten", "solltest", "somit", "sondern", "sonst", "sowas", "soweit", "tun", "tut", "über", "um", "und", "uns", "unser", "unsere", "unserem", "unseren", "unserer", "unter", "usw", "viel", "viele", "vielen", "vieles", "vom", "von", "vor", "war", "wäre", "waren", "wären", "wars", "was", "weder", "weg", "wegen", "weil", "weiter", "weitere", "welche", "welchen", "welcher", "welches", "wenn", "wenns", "wer", "werd", "werde", "werden", "wie", "wieder", "wieso", "will", "willst", "wir", "wird", "wirst", "wo", "wobei", "wollen", "wollte", "wollten", "worden", "wurde", "würde", "wurden", "würden", "z.b", "zb", "zu", "zum", "zur", "zwar", "zwischen")

# Kombinationen von Wörtern
relevant_ngrams = dictionary(list(
  "trotz_impfung" = "trotz impfung",
  "grippe_impfen" = "grippe impfen",
  "mmr_impfung" = "mmr impfung",
  "hepatitis_b" = "hepatitis b",
  "gut_vertragen" = "gut vertragen",
  "6fach_impfung" = "6fach impfung",
  "6_fach" = "6 fach",
  "6_fach_impfung" = "6 fach impfung",
  "meningokokken_b" = "meningokokken b",
  "gute_besserung" = "gute besserung",
  "6-fach_impfung" = "6-fach impfung",
  "erhöhte_temperatur" = "erhöhte temperatur",
  "kein_fieber" = "kein fieber",
  "kein_problem" = "kein problem",
  "keine_ahnung" = "keine ahnung",
  "keine_impfung" = "keine impfung",
  "nicht_geimpft" = "nicht geimpft",
  "nicht_impfen" = "nicht impfen",
  "nicht_zu_impfen" = "nicht zu impfen",
  "selbst_entscheiden" = "selbst entscheiden"
))

# Erstellen einer Dokument-Feature-Matrix aus dem Korpus
impf_dfm = crps %>% 
  dfm(stem = FALSE, tolower = TRUE, remove_punct = TRUE,
      remove = custom_stopwords,
      remove_url = TRUE, verbose = TRUE,
      thesaurus = relevant_ngrams)
impf_dfm

# Pruning
impf_dfm = impf_dfm %>%
  dfm_trim(max_docfreq = 0.99, min_docfreq = 0.005, docfreq_type = "prop")
impf_dfm

# Überblick: Die häufigsten Features im Korpus
impf_dfm %>% 
  colSums() %>% 
  enframe() %>% 
  arrange(desc(value)) %>% 
  slice(1:20) %>% 
  kable()
# als (beliebte, wenn auch nur mittel informative) Wordcloud
impf_dfm %>% 
  textplot_wordcloud()

# Umwandeln der dfm in das Format für stm
impf_stm = impf_dfm %>% 
  quanteda::convert(to = "stm")
