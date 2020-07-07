## ---- prevalence-plot

# Data
m37 = read_rds("R/data/model37.rds")
m37_labels = read_csv("R/data/labels_k37.csv")
m37_labels = m37_labels %>% 
  mutate(label = str_c(topic, label, sep = ": "))

# Prävelenz der Topics
m37_gamma = m37 %>% 
  tidy("gamma")

# Frex Terms
m37_frex = m37 %>% 
  labelTopics(n = 10) %>% 
  pluck("frex") %>% 
  as_tibble(.name_repair = ~ paste0("r", 1:10)) %>% 
  mutate(topic = 1:n()) %>% 
  gather(rank, frex_term, -topic) %>% 
  group_by(topic) %>% 
  summarise(frex_terms = paste0(frex_term, collapse = ", "))

# Plot
m37_gamma %>% 
  group_by(topic) %>% 
  summarise(P = mean(gamma),
            n = sum(gamma)) %>% 
  left_join(m37_labels) %>% 
  left_join(m37_frex) %>% 
  mutate(label = reorder(label, P)) %>% 
  ggplot(aes(P, label, label = frex_terms)) + geom_bar(stat = "identity") + scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 0.1)) + geom_text(hjust = 0, nudge_x = 5e-04, size = 3) + labs(x = "Anteil des Topics am gesamten Korpus", y = NULL)
