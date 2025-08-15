jamaica_dtm <- tidy_jamaica %>% 
  count(ID, word) %>%
  filter(n>=3) %>%
  cast_sparse(ID, word, n)

set.seed(123)
jamaica_stm <- stm(jamaica_dtm, K=6)
summary(jamaica_stm)

jamaica_topics <- tidy(jamaica_stm, matrix="beta")

jamaica_top_terms <- jamaica_topics %>%
  group_by(topic) %>%
  slice_max(beta, n=10) %>%
  ungroup() %>%
  arrange(topic, -beta)

p_topics <- jamaica_top_terms %>%
  mutate(term=reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic))) + 
    scale_fill_manual(values = rep(colorpal, length.out = 16)) +
    geom_col(show.legend=FALSE, width=0.75) + 
    facet_wrap(~topic, scales="free") + 
    geom_text(aes(label=round(beta,2)), size=3, hjust=-0.1, family="Courier") +
    scale_x_continuous(expand=expansion(add=c(0, 0.03))) +
    scale_y_reordered() +
    ggtitle("Top 10 Terms by Topic") +
    theme(text=element_text(size=12, family="Courier"),
        panel.background=element_rect(fill='white'),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=10))
p_topics
ggsave(p_topics, filename=file.path(output, paste0("p_topics.svg")), height=5, width=8)

jamaica_gamma <- tidy(jamaica_stm, 
                 matrix="gamma", 
                 document_names=rownames(jamaica_dfm))

p_gamma <- ggplot(jamaica_gamma, aes(gamma, fill=as.factor(topic))) + 
  scale_fill_manual(values = rep(colorpal, length.out = 16)) +
  geom_histogram(show.legend=FALSE, color="black") + 
  facet_wrap(~topic, ncol=3) +
  geom_text(stat="bin", aes(label=ifelse(after_stat(count)>0, after_stat(count), "")), size=3, vjust=-0.5, family="Courier") +
  labs(y="# of interviews") +
  scale_y_continuous(expand=expansion(add=c(0, 2))) +
  ggtitle("Document-Topic Probability Distributions") +
  theme(text=element_text(size=12, family="Courier"),
        panel.background=element_rect(fill='white'),
        axis.text.y=element_text(size=10))
p_gamma  
ggsave(p_gamma, filename=file.path(output, paste0("p_gamma.svg")), height=5, width=8)

p_gamma_time <- jamaica_gamma %>%
  left_join(relationship = "many-to-many", jamaica_df %>%
              mutate(ID=as.character(ID)) %>%
              select(Time_Bin, document=ID)) %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(gamma, topic, fill=topic)) + 
    scale_fill_manual(values = rep(colorpal, length.out = 16)) +
    geom_boxplot(alpha=0.7, show.legend=FALSE) +
    facet_wrap(vars(Time_Bin)) +
    ggtitle("Document-Topic Probability Distributions Over Time") +
    theme(text=element_text(size=12, family="Courier"),
        panel.background=element_rect(fill='white'),
        axis.text.y=element_text(size=10))
p_gamma_time  
ggsave(p_gamma_time, filename=file.path(output, paste0("p_gamma_time.svg")), height=5, width=8)
