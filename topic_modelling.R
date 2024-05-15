library(tidytext)
library(tidyverse)
library(stm)
library(reshape2)
library(ggplot2)

dataset <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRX4vtsgxgINoa_2GqAM4-4a2SI9Lp6knwI6-P0P1SAnCoARiYZYuZ0Dl_ztA1wreY9XXcipYJZvVLD/pub?output=csv')

dataset <- dataset %>%
  mutate(id_num = 1:nrow(dataset), .after = Email) %>%
  tibble()

cleaned_text <- dataset %>%
  unnest_tokens(word, Feedback) %>%
  select(id_num, word) %>%
  anti_join(stop_words)

data <- cleaned_text %>%
  count(id_num, word) %>%
  cast_sparse(id_num, word, n)


set.seed(123)
 
topic_model <- stm(data, K = 30)

topic_gamma <- tidy(
  topic_model, 
  matrix='gamma',
  document_names = rownames(data)
)

dataset <- dataset %>% mutate(id_num = as.character(id_num))

results <- topic_gamma %>%
  left_join(dataset, by = c("document" = "id_num")) %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) %>%
  select(document, topic, gamma)

results <- results %>%
  mutate(document = as.numeric(document)) %>%
  arrange(document)
  

summary_scores <- summary(topic_model)
scores <- summary_scores$score
scores <- as.data.frame(scores)

scores <- scores %>%
  mutate(topic = 1:nrow(scores), .before = V1)

trending_topics_data <- results %>%
  inner_join(scores, by = 'topic')

trending_topics <- trending_topics_data %>%
  pivot_longer(cols = names(trending_topics_data)[4:10],
               values_to = 'words') %>%
  select(words)
  
  
  
top_topics_final <- trending_topics %>%
  group_by(words) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  head(10)

top_topics_final %>%
  ggplot(aes(n, words)) +
  geom_col(fill = 'blue')
