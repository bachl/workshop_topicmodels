## ---- model-compare2

# 3) Modelle mit K = 31, ..., K = 39
# Modelle schätzen bzw. laden
if (file.exists("R/data/models31_39.rds")) {
  # Schneller: Modelle aus LMS laden
  many_models = read_rds("R/data/models31_39.rds")
} else {
  # Vorsicht: Schätzen dauert auf meinem MacBook Pro 2020 i9 32 GB RAM 3 Minuten
  Ks = 31:39
  tic()
  plan(multiprocess(workers = 9))
  many_models = tibble(K = Ks) %>%
    mutate(topic_model = future_map(K, ~stm(documents = impf_stm$documents, 
                                            vocab = impf_stm$vocab,
                                            data = impf_stm$meta,
                                            init.type = "Spectral",
                                            K = ., verbose = FALSE),
                                    .progress = TRUE))
  plan(sequential)
  toc()
  saveRDS(many_models, "R/data/models31_39.rds")  
}

# Quantitative Indikatoren der Modellqualität berechnen
# Inspired by https://juliasilge.com/blog/evaluating-stm/
if (file.exists("R/data/eval31_39.rds")) {
  # Schneller: Modellevaluation aus LMS laden
  model_eval = read_rds("R/data/eval31_39.rds")
} else {
  # Evaluation dauert auf meinem MacBook Pro 2020 i9 32 GB RAM 24 Sekunden
  tic()
  heldout = make.heldout(documents = impf_stm$documents, vocab = impf_stm$vocab)
  plan(multiprocess(workers = 9))
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
  saveRDS(model_eval, "R/data/eval31_39.rds")  
}

# Zum Vergleich: Evaluation der vorherigen Modelle
model_eval_old = read_rds("R/data/eval10_100.rds")

# Exklusivität und Semantische Kohärenz (Mean, Median)
model_eval %>% 
  bind_rows(model_eval_old) %>% 
  filter(K >= 30 & K <= 40) %>% 
  select(K, Exclusivity = exclusivity, Coherence = semantic_coherence) %>% 
  mutate_at(-1, .funs = list(Mean = ~map_dbl(.x, mean), Median = ~map_dbl(.x, median))) %>% 
  select_if(negate(is.list)) %>% 
  gather(metric, value, -K) %>% 
  separate(metric, c("measure", "metric"), "_") %>% 
  spread(measure, value) %>% 
  ggplot(aes(Coherence, Exclusivity, label = K)) + geom_text() + facet_wrap("metric")

# Held-out-likelihood und multinomiale Residuen
model_eval %>% 
  bind_rows(model_eval_old) %>% 
  filter(K >= 30 & K <= 40) %>% 
  select(K, `Held-out likelihood (higher is better)` = held_out_likelihood, `Multinomial dispersion of residuals (lower is better)` = residuals) %>% 
  gather(measure, value, -K) %>% 
  ggplot(aes(K, value, label = K)) + geom_line() + geom_text() + facet_wrap("measure", scales = "free_y", ncol = 1) + labs(x = "Number of topics (K)", y = NULL)

# Speichern der Modelle mit 30 bis 40 Topics für qualitative Analyse
# Benennung und Datenstruktur für stminsights
many_models_old = read_rds("R/data/models10_100.rds")
out = impf_stm
m30 = many_models_old$topic_model[[3]]
m31 = many_models$topic_model[[1]]
m32 = many_models$topic_model[[2]]
m33 = many_models$topic_model[[3]]
m34 = many_models$topic_model[[4]]
m35 = many_models$topic_model[[5]]
m36 = many_models$topic_model[[6]]
m37 = many_models$topic_model[[7]]
m38 = many_models$topic_model[[8]]
m39 = many_models$topic_model[[9]]
m40 = many_models_old$topic_model[[4]]
save(out, m30, m31, m32, m33, m34, m35, m36, m37, m38, m39, m40, file = "R/data/models30_40.rdata")
