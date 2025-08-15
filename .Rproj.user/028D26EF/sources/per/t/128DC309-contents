# TIDY DATASET ####
tidy_bigrams <- jamaica_df %>%
  unnest_tokens(bigram, Text, token="ngrams", n=2) %>%
  filter(!is.na(bigram)) %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% all_stop_words$word) %>%
  filter(!word2 %in% all_stop_words$word) %>%
  unite(bigram, word1, word2, sep=" ", remove=FALSE)

# BY ID ####
ngrams_id5 <- tidy_bigrams %>%
  count(ID, bigram, sort=TRUE) %>%
  group_by(ID) %>%
  mutate(total_bigrams = sum(n), percent=n/total_bigrams) %>%
  slice_max(order_by=n, n=5, with_ties=TRUE) %>%
  ungroup()

tfgrams_id5 <- tidy_bigrams %>%
  count(ID, bigram, sort=TRUE) %>%
  bind_tf_idf(bigram, ID, n) %>%
  group_by(ID) %>%
  slice_max(order_by=tf_idf, n=5, with_ties=TRUE) %>%
  ungroup()

p_ngramsid_top5 <- lapply(interview_ids, function(i) {
  p_ngramid <- ngrams_id5 %>%
    filter(ID==i) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(bigram, n, fill=ID)) +
    geom_col(fill="#0484fb", width=0.75) +
    geom_text(aes(label=paste0(n, "(", scales::label_percent(accuracy = 0.1)(percent), ")")), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 1.5))) +
    ggtitle(paste("ID", i, "\nFrequency")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  p_tfgramid <- tfgrams_id5 %>%
    filter(ID==i) %>%
    mutate(bigram = reorder(bigram, tf_idf)) %>%
    ggplot(aes(bigram, tf_idf, fill=ID)) +
    geom_col(fill="#7cc4a4", width=0.75) +
    geom_text(aes(label=round(tf_idf, 3)), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 1.5))) +
    ggtitle(paste("\nTF-IDF")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  pfin_ngramid <- (p_ngramid + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"))) | 
    (p_tfgramid + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt")))
  
  ggsave(pfin_ngramid, filename=file.path(output, paste0("pfin_ngramid_", i, ".svg")), height=7, width=6.5)
})

# BY TIME PERIOD ####
ngrams_time5 <- tidy_bigrams %>%
  count(Time_Bin, bigram, sort=TRUE) %>%
  group_by(Time_Bin) %>%
  mutate(total_bigrams = sum(n), percent=n/total_bigrams) %>%
  slice_max(order_by=n, n=5, with_ties=TRUE) %>%
  ungroup()

tfgrams_time5 <- tidy_bigrams %>%
  count(Time_Bin, bigram, sort=TRUE) %>%
  bind_tf_idf(bigram, Time_Bin, n) %>%
  group_by(Time_Bin) %>%
  slice_max(order_by=tf_idf, n=5, with_ties=TRUE) %>%
  ungroup()

p_ngramstime_top5 <- lapply(time_bins, function(i) {
  p_ngramtime <- ngrams_time5 %>%
    filter(Time_Bin==i) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(bigram, n, fill=Time_Bin)) +
    geom_col(fill="#0484fb", width=0.75) +
    geom_text(aes(label=paste0(n, "(", scales::label_percent(accuracy = 0.1)(percent), ")")), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 1.5))) +
    ggtitle(paste(i, "\nFrequency")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  p_tfgramtime <- tfgrams_time5 %>%
    filter(Time_Bin==i) %>%
    mutate(bigram = reorder(bigram, tf_idf)) %>%
    ggplot(aes(bigram, tf_idf, fill=Time_Bin)) +
    geom_col(fill="#7cc4a4", width=0.75) +
    geom_text(aes(label=round(tf_idf, 3)), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 1.5))) +
    ggtitle(paste("\nTF-IDF")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  pfin_ngramtime <- (p_ngramtime + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"))) | 
    (p_tfgramtime + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt")))
  
  ggsave(pfin_ngramtime, filename=file.path(output, paste0("pfin_ngramtime_", i, ".svg")), height=7, width=6.5)
})