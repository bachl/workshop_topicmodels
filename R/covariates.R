## ---- covariates

# Data
m37 = read_rds("R/data/model37.rds")
m37_labels = read_csv("R/data/labels_k37.csv")
m37_labels = m37_labels %>% 
  mutate(label = str_c(topic, label, sep = ": "))
out = read_rds("R/data/out.rds")


# Effect Objekt
if (file.exists("R/data/m37_effects.rds")) {
  # Laden der Effekt-Schätzung
  m37_effects = read_rds("R/data/m37_effects.rds")
} else {
  # Effekt-Schätzung
  m37_effects = m37 %>% 
    estimateEffect(1:37 ~ s(date_num), stmobj = ., metadata = out$meta)  
  saveRDS(m37_effects, "R/data/m37_effects.rds")
}

# Daten für Plot
eff_plot_data = m37_effects %>% 
  get_effects(variable = "date_num", type = "continuous") %>% 
  mutate(topic = as.integer(as.character(topic)),
         date = as_date(value + max(as.numeric(out$meta$postdate)))) %>% 
  left_join(m37_labels) %>% 
  mutate(label = reorder(label, topic, mean))

# Plot: Alle Topics über die Zeit
plt1 = eff_plot_data %>% 
  ggplot(aes(date, proportion, ymin = lower, ymax = upper)) + geom_ribbon(alpha = 0.5) + geom_line() + facet_wrap("label", ncol = 2) + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.08))

# Plot: Grippe und Evidenz
plt2 = eff_plot_data %>% 
  filter(topic %in% c(16, 28)) %>% 
  ggplot(aes(date, proportion, ymin = lower, ymax = upper, group = label)) + geom_ribbon(alpha = 0.5) + geom_line(aes(color = label)) + scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.08))
