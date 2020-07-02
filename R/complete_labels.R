complete_labels = function(.reference_labels, .stm_list, n_features = 10, threshold = 7, csv = FALSE) {
  
  frx = .stm_list %>% map_dfr(~{
    labelTopics(model = .x, n = n_features) %>% 
      .$frex %>% 
      as_tibble(.name_repair = ~ paste0("f", 1:n_features)) %>% 
      mutate(topic = 1:n()) %>% 
      gather(rank, frex_term, -topic) %>% 
      mutate(rank = as.integer(str_sub(rank, start = 2)),
             K = as.integer(.x$settings$dim$K),
             Ktopic = K * 100 + topic) %>% 
      select(K, Ktopic, topic, frex_term) %>% 
      group_by(K, topic, Ktopic) %>% 
      summarise(frex_terms = list(frex_term)) %>% 
      ungroup()
  }
  )
  
  frx_ref = frx %>% 
    filter(K == max(.reference_labels$topic))
  
  frx_more = frx %>% 
    filter(K != max(.reference_labels$topic))
  
  Ks = frx_more %>% 
    pull("K") %>% 
    unique()
  
  frx_match = Ks %>% 
    map_dfr(~{
      expand_grid(
        from = frx_ref %>% 
          pull(Ktopic),
        to = frx_more %>% 
          filter(K == .x) %>% 
          pull(Ktopic)
      )
    }) %>% 
    left_join(rename(frx_ref, from_terms = frex_terms), by = c("from" = "Ktopic")) %>% 
    left_join(rename(frx_more, to_terms = frex_terms), by = c("to" = "Ktopic")) %>% 
    rowwise() %>% 
    mutate(n_termoverlap = length(intersect(from_terms, to_terms))) %>% 
    ungroup() %>% 
    filter(n_termoverlap >= threshold) %>% 
    left_join(.reference_labels, by = c("topic.x" = "topic"))
  
  lbls = Ks %>% 
    map(~{
      tibble(topic = 1:.x) %>% 
        left_join(
          frx_match %>% 
            filter(K.y == .x) %>% 
            select(topic = topic.y, label, n_termoverlap)
          , by = "topic")
    })
  
  if (csv) {
    lbls %>% 
      append(list(.reference_labels)) %>% 
      walk(~write_csv(.x, path = paste0("labels_k", max(.x$topic), ".csv")))
  } else {
    return(lbls %>% append(list(.reference_labels)))
  }
}
