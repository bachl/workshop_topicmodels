## ---- read-data

# Laden der Daten
d = read_rds("R/data/example_data.rds")
d %>% 
  print(n = 5)

d %>% 
  mutate(ym = round_date(postdate, "month")) %>% 
  count(ym) %>% 
  ggplot(aes(ym, n)) + geom_line()

d %>% 
  pull("wc") %>% 
  summary()

