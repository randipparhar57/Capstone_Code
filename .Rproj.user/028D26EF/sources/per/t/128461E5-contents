# TIDY DATASET ####
tidy_jamaica <- jamaica_df %>%
  unnest_tokens(word, Text) %>%
  anti_join(all_stop_words, by="word")

# BY ID ####
freq_id5 <- tidy_jamaica %>%
  count(ID, word, sort=TRUE) %>%
  group_by(ID) %>%
  mutate(total_words = sum(n), percent=n/total_words) %>%
  slice_max(n, n=5) %>%
  ungroup 

tfidf_id5 <- tidy_jamaica %>%
  count(ID, word, sort=TRUE) %>%
  bind_tf_idf(word, ID, n) %>%
  group_by(ID) %>%
  slice_max(tf_idf, n=5) %>%
  ungroup 

p_termid_top5 <- lapply(interview_ids, function(i) {
  p_freqid <- freq_id5%>%
    filter(ID==i) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill=ID)) +
    geom_col(fill="#0484fb", width=0.75) +
    geom_text(aes(label=paste0(n, "(", scales::label_percent(accuracy = 0.1)(percent), ")")), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 1))) +
    ggtitle(paste("ID", i, "\nFrequency")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  p_tfid <- tfidf_id5 %>%
    filter(ID==i) %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill=ID)) +
    geom_col(fill = "#7cc4a4", width=0.75) +
    geom_text(aes(label=round(tf_idf, 3)), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 0.5))) +
    ggtitle(paste("ID", i, "\nTF-IDF")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  pfin_termid <- (p_freqid + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"))) | 
    (p_tfid + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt")))
  
  ggsave(pfin_termid, filename=file.path(output, paste0("pfin_termid_", i, ".svg")), height=4, width=6)
})

# BY TIME ####
freq_time5 <- tidy_jamaica %>%
  count(Time_Bin, word, sort=TRUE) %>%
  group_by(Time_Bin) %>%
  mutate(total_words = sum(n), percent=n/total_words) %>%
  slice_max(n, n=5) %>%
  ungroup 

tfidf_time5 <- tidy_jamaica %>%
  count(Time_Bin, word, sort=TRUE) %>%
  bind_tf_idf(word, Time_Bin, n) %>%
  group_by(Time_Bin) %>%
  slice_max(tf_idf, n=5) %>%
  ungroup 

p_termtime_top5 <- lapply(time_bins, function(i) {
  p_freqtime <- freq_time5%>%
    filter(Time_Bin==i) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill=Time_Bin)) +
    geom_col(fill="#0484fb", width=0.75) +
    geom_text(aes(label=paste0(n, "(", scales::label_percent(accuracy = 0.1)(percent), ")")), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 0.5))) +
    ggtitle(paste("Time Period: ", i, "\nFrequency")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  p_tftime <- tfidf_time5 %>%
    filter(Time_Bin==i) %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill=Time_Bin)) +
    geom_col(fill = "#7cc4a4", width=0.75) +
    geom_text(aes(label=round(tf_idf, 3)), hjust=-0.1, size=3.5, family="Courier") +
    coord_flip() +
    scale_y_continuous(expand=expansion(mult=c(0, 0.5))) +
    ggtitle(paste("Time Period: ", i, "\nTF-IDF")) +
    theme(text=element_text(size=12, family="Courier"),
          panel.background=element_rect(fill='white'),
          axis.title=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_text(size=10))
  
  pfin_termtime <- (p_freqtime + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"))) | 
    (p_tftime + theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt")))
  
  ggsave(pfin_termtime, filename=file.path(output, paste0("pfin_termtime_", i, ".svg")), height=4, width=8)
})
