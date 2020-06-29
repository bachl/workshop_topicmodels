## ---- model-compare

# 1) Modelle mit K = 10, ..., K = 100
# Modelle schätzen bzw. laden
if (file.exists("R/data/models10_100.rds")) {
# Schneller: Modelle aus LMS laden
  many_models = read_rds("R/data/models10_100.rds")
} else {
# Vorsicht: Schätzen dauert auf meinem MacBook Pro 2020 i9 32 GB RAM 10 Minuten
  Ks = seq(10, 100, by = 10)
  tic()
  plan(multiprocess(workers = 10))
  many_models = tibble(K = Ks) %>%
    mutate(topic_model = future_map(K, ~stm(documents = impf_stm$documents, 
                                            vocab = impf_stm$vocab,
                                            data = impf_stm$meta,
                                            init.type = "Spectral",
                                            K = ., verbose = FALSE),
                                    .progress = TRUE))
  plan(sequential)
  toc()
  saveRDS(many_models, "R/data/models10_100.rds")  
}

# Quantitative Indikatoren der Modellqualität berechnen
# Inspired by https://juliasilge.com/blog/evaluating-stm/
if (file.exists("R/data/eval10_100.rds")) {
  # Schneller: Modellevaluation aus LMS laden
  model_eval = read_rds("R/data/eval10_100.rds")
} else {
  # Evaluation dauert auf meinem MacBook Pro 2020 i9 32 GB RAM 24 Sekunden
  tic()
  heldout = make.heldout(documents = impf_stm$documents, vocab = impf_stm$vocab)
  plan(multiprocess(workers = 10))
  model_eval = many_models %>% 
    mutate(exclusivity = future_map(topic_model, exclusivity),
           semantic_coherence = future_map(topic_model, semanticCoherence, impf_stm$documents),
           eval_heldout = future_map(topic_model, eval.heldout, heldout$missing),
           residual = future_map(topic_model, checkResiduals, impf_stm$documents),
           residuals = future_map_dbl(residual, "dispersion"),
           held_out_likelihood = future_map_dbl(eval_heldout, "expected.heldout")) %>% 
    select(-topic_model)
  plan(sequential)
  toc()
  saveRDS(model_eval, "R/data/eval10_100.rds")  
}

# Exklusivität und Semantische Kohärenz (Mean, Median)
model_eval %>% 
  select(K, Exclusivity = exclusivity, Coherence = semantic_coherence) %>% 
  mutate_at(-1, .funs = list(Mean = ~map_dbl(.x, mean), Median = ~map_dbl(.x, median))) %>% 
  select_if(negate(is.list)) %>% 
  gather(metric, value, -K) %>% 
  separate(metric, c("measure", "metric"), "_") %>% 
  spread(measure, value) %>% 
  ggplot(aes(Coherence, Exclusivity, label = K)) + geom_text() + facet_wrap("metric")

# Exklusivität und Semantische Kohärenz von k = 40, 50
model_eval %>% 
  filter(K %in% c(40, 50)) %>% 
  select(K, Exclusivity = exclusivity, Coherence = semantic_coherence) %>% 
  unnest(c(Exclusivity, Coherence)) %>% 
  ggplot(aes(Coherence, Exclusivity, color = factor(K))) + geom_point()

# Held-out-likelihood und multinomiale Residuen
model_eval %>% 
  select(K, `Held-out likelihood (higher is better)` = held_out_likelihood, `Multinomial dispersion of residuals (lower is better)` = residuals) %>% 
  gather(measure, value, -K) %>% 
  ggplot(aes(K, value, label = K)) + geom_line() + geom_text() + facet_wrap("measure", scales = "free_y", ncol = 1) + labs(x = "Number of topics (K)", y = NULL)

# Speichern der Modelle mit 30 und 40 Topics für qualitative Analyse
# Benennung und Datenstruktur für stminsights
out = impf_stm
m30 = many_models$topic_model[[3]]
m60 = many_models$topic_model[[6]]
# save(out, m30, m60, file = "R/data/models30_60.rdata")
