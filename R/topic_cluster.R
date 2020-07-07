## ---- topic-cluster

# Data
m37 = read_rds("R/data/model37.rds")
m37_labels = read_csv("R/data/labels_k37.csv")
m37_labels = m37_labels %>% 
  mutate(label = str_c(topic, label, sep = ": "))


# Hierarchische CLusteranalyse des gemeinsamen Auftretens von Topics in Dokumenten
m37_dist = textmineR::CalcHellingerDist(m37$theta, by_rows = FALSE)
clusters = hclust(as.dist(m37_dist), "ward.D2")
clusters$labels = m37_labels$label
# Dendogramm
ggdendro::ggdendrogram(clusters, rotate = TRUE) + labs(title = NULL) + scale_y_continuous(limits = c(0,1), expand = c(0,0), name = "Distanz") + theme(axis.text.y = element_text(size = 10, colour = "black"), axis.title = element_text(size = 14, face = "bold")) 

# Plot mit 7 Topic-Clustern
m37_gamma %>% 
  group_by(topic) %>% 
  summarise(P = mean(gamma),
            n = sum(gamma)) %>% 
  mutate(grp = case_when(topic %in% c(21, 10, 11, 7, 16) ~ 1,
                         topic %in% c(27, 1, 2, 35, 24) ~ 2,
                         topic %in% c(31, 8, 19) ~ 3,
                         topic %in% c(34, 15, 30, 23, 6, 33, 36) ~ 4,
                         topic %in% c(28, 17, 12, 9, 18) ~ 5,
                         topic %in% c(32, 29, 37, 5, 3, 13, 14, 26, 20, 4) ~ 6,
                         topic %in% c(25, 22) ~ 7)) %>% 
  left_join(m37_labels) %>% 
  left_join(m37_frex) %>% 
  mutate(label = reorder(label, P)) %>% 
  ggplot(aes(P, label, label = frex_terms, fill = factor(grp))) + geom_bar(stat = "identity", show.legend = FALSE) + scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 0.1)) + geom_text(hjust = 0, nudge_x = 5e-04, size = 3) + labs(x = "Anteil des Topics am gesamten Korpus", y = NULL) + facet_grid(grp ~ ., scales = "free_y", space = "free_y")
