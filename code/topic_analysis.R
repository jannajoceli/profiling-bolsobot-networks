#### Topics through time ####

pacman::p_load(tidyverse, rio, tidytext, lubridate, ggExtra, naniar)

extras <- tibble(word = c('de', 'aqui', 'aí', 'né', 'tá', 'vai', 'então', 'estou', 'estar', 'estava', 'estando', 'quem', 'como','quando',
                          'vai', 'gente', 'nós', 'eles', 'noiz', 'ela', 'lá', 'fazer', 'pode', 'mano', 'tô', 'nada', 'tô', 
                          'o', 'a', 'bora', '2', '3', 'galera', 'ainda', 'assim', 'isso', 'tava', 'estando', 'cara',
                          't.co', 'https', '.com', 'sobre', 'pra', 'para', 'dar', 'deixar', 'ver', 'falar', 'ficar', 'porque', 'saber', 'passar', 'querer', 'tão', 'todo',
                          'tanto', 'como', 'algum', 'outro', 'seu', 'sua', 'dela', 'dele', 'com', 'quando', 'quem', 'como', 'mais', 'menos', 'de', 'da', 'é'))
                 
stpw <- import('stopwords.txt', setclass = 'tibble')



raw_tweets <- list.files("all_tweets", full.names = T) %>%
  map_df(import, setclass = "tibble")

tweets <- raw_tweets %>%
  select(screen_name, body, mean_cap, sd_cap, timestamp, is_retweet, is_quote_tweet) %>%
  mutate(mean_cap = round(mean_cap, 2),
         sd_cap = round(sd_cap, 2)) %>%
  unique()

raw_tweets %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  count(mean_cap, is_reply) %>%
  group_by(mean_cap) %>%  
  mutate(total_n = sum(n)) %>%
  ungroup() %>%
  mutate(n = if_else(is_reply == "no", -n, n),
         pct_n = n/total_n) %>%
  ggplot(aes(mean_cap, pct_n)) +
  geom_line(aes(group = mean_cap)) +
  geom_point(aes(color = is_reply), size = 5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Presence of retweets on profiles",
       subtitle = "Based on their bot score",
       x = "Bot score",
       y = "Percentage of retweets",
       caption = "SMART Data Sprint",
       color = "reply") +
  scale_color_manual(values = c("#E9806E", "#78BC61"))


unnested_tw <- tweets %>%
  mutate(body = str_remove_all(tolower(body), "(@[a-z0-9_]+)") %>%
           str_remove_all("(#[a-z0-9_]+)") %>%
           str_remove_all("RT") %>%
           str_remove_all("\\\n") %>%
           str_remove_all("(https?:\\/\\/)?([[0-9a-z]\\.-]+)\\.([a-z\\.]{2,6})([\\/[0-9a-z] \\.-]*)")) %>%
  unnest_tokens(word, body) %>%
  anti_join(extras) %>%
  anti_join(stpw) %>%
  filter(nchar(word) > 2) %>%
  mutate(class = case_when(mean_cap >= 0.67 ~"high",
                           mean_cap < 0.67 & mean_cap > 0.33 ~"medium",
                           mean_cap <= 0.33 ~"low"))


get_tweet_info <- function(base, text){
  clean_tweet <- base %>%
    mutate(n_hash = str_extract_all(tolower({{text}}), "(#[a-z0-9_]+)"),
           mention = str_extract_all(tolower({{text}}), "(@[a-z0-9_]+)"),
           link = str_extract_all(tolower({{text}}), "(https?:\\/\\/)?([[0-9a-z]\\.-]+)\\.([a-z\\.]{2,6})([\\/[0-9a-z] \\.-]*)"))
  return(clean_tweet)
}

unnested_tw %>%
  select(timestamp, class, screen_name) %>%
  unique() %>%
  count(class) %>%
  mutate(total = sum(n),
         pct = n/total)

user_relation <- tweets %>%
  group_by(screen_name, body, timestamp) %>%
  get_tweet_info(body) %>%
  ungroup()


counted_hashtags <- user_relation %>%
  mutate(hashs = lengths(n_hash),
         mentions = lengths(mention),
         links = lengths(link),
         mean_cap = round(mean_cap, 1)) %>%
  group_by(mean_cap) %>%
  summarise(hashs = mean(hashs),
            mentions = mean(mentions),
            links = mean(links))

# Robôs quotam mais (uso de RTs)
  
counted_hashtags %>%
  mutate(quotes = -quotes) %>%
  pivot_longer(-1) %>%
  ggplot(aes(mean_cap, value)) +
  geom_col(aes(fill = name)) +
  coord_flip()

# Robôs retweetam mais?

tweets  %>%
  mutate(mean_cap = round(mean_cap, digits = 1)) %>%
  count(mean_cap, is_retweet) %>%
  group_by(mean_cap) %>%
  mutate(total_n = sum(n)) %>%
  mutate(n = if_else(is_retweet == "no", -n, n),
         pct_n = n/total_n) %>%
  ggplot(aes(mean_cap, pct_n)) +
  geom_line(aes(group = mean_cap)) +
  geom_point(aes(color = is_retweet), size = 5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Presence of retweets on profiles",
       subtitle = "Based on their bot score",
       x = "Bot score",
       y = "Percentage of retweets",
       caption = "SMART Data Sprint",
       color = "retweets") +
  scale_color_manual(values = c("#E9806E", "#78BC61"))

tweets %>%
  mutate(class = case_when(mean_cap >= 0.67 ~"high",
                           mean_cap < 0.67 & mean_cap > 0.33 ~"medium",
                           mean_cap <= 0.33 ~"low")) %>%
  ggplot(aes(mean_cap)) +
  geom_histogram(aes(fill = class)) +
  theme_minimal() +
  labs(title = "Number of accounts per Botometer score",
       subtitle = "Enormous concentration around 80%",
       x = "Botometer score",
       y = "Number of users",
       caption = "SMART Data Sprint")

tweets %>%
  mutate(mean_cap = round(mean_cap, digits = 1)) %>%
  count(mean_cap, is_quote_tweet) %>%
  group_by(mean_cap) %>%
  mutate(total_n = sum(n)) %>%
  mutate(n = if_else(is_quote_tweet == "no", -n, n),
         pct_n = n/total_n) %>%
  ggplot(aes(mean_cap, pct_n)) +
  geom_col(aes(fill = is_quote_tweet)) +
  coord_flip() +
  labs(title = "Presence of quoted tweets on profiles",
       subtitle = "Based on their bot score",
       x = "Bot score",
       y = "Percentage of quoted tweets",
       caption = "SMART Data Sprint",
       fill = "quoted tweet")

total_count <- tweets %>%
  mutate(mean_cap = round(mean_cap, digits = 1)) %>%
  count(mean_cap) %>%
  select(mean_cap, total_val = n)

tweets %>%
  mutate(mean_cap = round(mean_cap, digits = 1)) %>%
  filter(is_retweet == "no" & is_quote_tweet == "no") %>%
  count(mean_cap) %>%
  left_join(total_count) %>%
  mutate(pct_n = n/total_val) %>%
  ggplot(aes(mean_cap, pct_n)) +
  geom_col(fill = "#C59B76") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Original tweets by bot score",
       x = "Bot score",
       y = "Percentage of original tweets",
       caption = "SMART Data Sprint")
  
# Variedade linguística --------------------------------------------------------
big_post <- tweets %>%
  count(screen_name) %>%
  filter(n > 10)

# Quem tem o vocabulário mais variado?

unnested_tw %>%
  filter(screen_name %in% big_post$screen_name) %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(class, screen_name) %>%
  summarise(lexico = n_distinct(word),
            total_words = n(),
            pct_word = lexico/total_words) %>%
  filter(!is.na(class)) %>%
  group_by(class) %>%
  mutate(order = median(pct_word)) %>%
  ggplot(aes(reorder(class, order), pct_word)) +
  geom_jitter(aes(color = class)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Linguistic complexity by botscore group",
       subtitle = "Only users who posted more than 10 times",
       x = "class",
       y = "percentage of original words",
       color = "Class",
       caption = "SMART Data Sprint")

# Variedade sobre o tempo

unnested_tw %>%
  mutate(ano = year(timestamp)) %>%
  filter(ano >= 2018 & screen_name %in% big_post$screen_name) %>%
  group_by(class, ano) %>%
  summarise(unique_w = n_distinct(word),
         total_words = n(),
         pct_word = unique_w/total_words) %>%
  ggplot(aes(ano, pct_word)) +
  geom_point(aes(color = class)) +
  geom_line(aes(color = class, group = class))


# Unique hashtags by year


first_hashtags <- user_relation %>%
  select(timestamp, n_hash, mean_cap) %>%
  unnest(n_hash) %>%
  mutate(date = lubridate::ymd_hms(timestamp),
         ano = year(date),
         n_hash = tolower(n_hash)) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  group_by(n_hash) %>%
  mutate(original = date == min(date))

first_hashtags %>%
  mutate(ano_original = if_else(original, ano, 0)) %>%
  replace_with_na(replace = list(ano_original = 0)) %>%
  arrange(n_hash, timestamp) %>%
  fill(ano_original) %>%
  group_by(ano, ano_original) %>%
  summarise(total_hash = n()) %>%
  ggplot(aes(ano, total_hash)) +
  geom_col(aes(fill = as.factor(ano_original)), position = "dodge") +
  labs(title = "Year of origin hashtags",
       subtitle = "How these users rearange information",
       x = "year",
       y = "total hashtags",
       fill = "year of origin",
       caption = "SMART Data Sprint") +
  theme_minimal()

#ggsave("graphics/origin_hashtag.png")

hash_life <- first_hashtags %>%
  mutate(date = as.Date(date)) %>%
  group_by(n_hash) %>%
  summarise(dif = max(date) - min(date)) %>%
  arrange(-dif) %>%
  mutate(reach = case_when(dif < 30 ~"momentain",
                           dif >= 30 & dif < 90 ~"short",
                           dif >= 90 & dif < 300 ~"medium",
                           dif >= 300 & dif < 1000 ~"stable",
                           dif >= 1000 ~"ultra-long"))

hash_life %>%
  count(reach)


first_hashtags %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, n_hash) %>%
  summarise(rep_hash = n()) %>%
  group_by(n_hash) %>%
  mutate(original = min(date)) %>%
  filter(rep_hash == max(rep_hash) & rep_hash > 1000) %>%
  mutate(dif_date = date - original) %>%
  ungroup() %>%
  summarise(time_to_blow = mean(dif_date))
  

first_hashtags %>%
  mutate(date = as.Date(date),
         y_birth = min(date),
         y_death = max(date)) %>%
  ungroup() %>%
  mutate(dif = y_death - y_birth,
         mean_cap = round(mean_cap, 1)) %>%
  filter(original & dif > 1) %>%
  ggplot(aes(mean_cap, dif)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(aes(group = mean_cap)) +
  labs(title = "Average duration of hashtags by botometer score",
       subtitle = "Only hashtags that lasted more than a day",
       x = "Botometer score",
       y = "Number of days",
       caption = "SMART Data Sprint") +
  theme_minimal()
  

ggsave("graphics/hashtag_duration.png")

mean(hash_life$dif)




first_hashtags %>%
  ungroup() %>%
  filter(original) %>%
  count(ano) %>%
  group_by(ano) %>%
  mutate() 
  ggplot(aes(ano)) +
  geom_histogram()


first_hashtags %>%
  filter(original) %>%
  ungroup() %>%
  ggplot(aes(ano, mean_cap)) +
  geom_jitter(aes(color = as.factor(ano)), alpha = 0.3, show.legend = F) +
  geom_boxplot(aes(group = ano), alpha = 0.9) +
  theme_minimal() +
  labs(#title = "Number of original hashtags per year and who started them",
       #subtitle = "Each point is the first use of a hashtag",
       x = "year",
       y = "Botometer score"
       #caption = "SMART Data Sprint")
  ) +
  theme(panel.grid = element_blank())

ggsave("graphics/original_hashtags_per_year.png")

  
# Repetition of hashtags

user_relation %>%
  select(timestamp, n_hash, mean_cap) %>%
  unnest(n_hash) %>%
  mutate(ts = ymd_hms(timestamp),
         date = as.Date(ts),
         ano = as.factor(year(date)),
         n_hash = tolower(n_hash)) %>%
  group_by(n_hash) %>%
  mutate(original = if_else(date == min(date), year(date))) %>%
  arrange(n_hash, ts) %>%
  fill(original) %>%
  ungroup() %>%
  mutate(date = as.Date(date),
         mean_cap = round(mean_cap, 1)) %>%
  ggplot(aes(date, mean_cap)) +
  geom_point(aes(color = original)) +
  facet_wrap(~original)


# Quoting bots?

double_cap <- tweets %>%
  select(double_cap = mean_cap, reply_to = screen_name) %>%
  unique()

replies <- raw_tweets  %>%
  select(body, timestamp, screen_name, mean_cap, reply_to) %>%
  mutate(timestamp = as.Date(timestamp)) %>%
  unique() %>%
  filter(!is.na(reply_to)) %>%
  left_join(double_cap)

replies %>%
  mutate(dif = double_cap - mean_cap,
         class = case_when(mean_cap >= 0.67 ~"high",
                           mean_cap < 0.67 & mean_cap > 0.33 ~"medium",
                           mean_cap <= 0.33 ~"low")) %>%
  filter(!is.na(dif)) %>%
  ggplot(aes(dif)) +
  geom_density(aes(fill = class), alpha = 0.3)
  # facet_grid(class~.)

replies %>%
  filter(!is.na(double_cap)) %>%
  filter(screen_name != reply_to) %>%
  ggplot(aes(mean_cap, double_cap)) +
  geom_hex() +
  scale_fill_gradient(low = "#FADF63", high = "#B3001B") +
  geom_abline()

replies %>%
  filter(!is.na(double_cap)) %>%
  mutate(dif = double_cap - mean_cap) %>%
  group_by(mean_cap) %>%
  summarise(dif = mean(dif)) %>%
  ggplot(aes(mean_cap, dif)) +
  geom_hex() +
  scale_fill_gradient(low = "#FADF63", high = "#B3001B")

#### Create gephi to see distribution ------------------------------------------

edges_rep <- replies %>%
  filter(!is.na(double_cap)) %>%
  select(Source = screen_name, Target = reply_to)

node_rep <- replies %>%
  mutate(dif = double_cap - mean_cap,
         class = case_when(mean_cap >= 0.67 ~"high",
                           mean_cap < 0.67 & mean_cap > 0.33 ~"medium",
                           mean_cap <= 0.33 ~"low")) %>%
  select(Id = screen_name, Label = screen_name, class)


walk2(list(edges_rep, node_rep), c("gephi/edges_reply.csv", "gephi/nodes_reply.csv"), ~write_excel_csv(.x, .y))


# Understanding language ------------------------------------------------------


# Hashatag use

main_hashtags <- user_relation %>%
  unnest(n_hash) %>%
  count(n_hash) %>%
  slice_max(n, n = 30)


user_relation %>%
  unnest(n_hash) %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  count(mean_cap, n_hash) %>%
  group_by(mean_cap) %>%
  mutate(total_n = sum(n),
         pct_n = n/total_n) %>%
  ungroup() %>%
  filter(n_hash %in% main_hashtags$n_hash) %>%
#  slice_max(pct_n, n = 10) %>%
  ggplot(aes(reorder(n_hash, n), mean_cap)) +
  geom_tile(aes(fill = pct_n)) +
  coord_flip() +
  geom_text(aes(label = str_c(round(pct_n, 3)*100, "%")), color = "white") +
  scale_fill_gradient(low = "#FADF63", high = "#B3001B") +
  labs(title = "Hashtags by botometer",
       subtitle = "#bolsonaroestamoscontigo is the biggest",
       y = "botometer score",
       x = "hashtag",
       fill = "presence") +
  theme_minimal()

user_relation %>%
  unnest(n_hash) %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  group_by(mean_cap) %>%
  summarise(unique_hash = n_distinct(n_hash),
            unique_user = n_distinct(screen_name),
            pct_hash = unique_hash/unique_user) %>%
  ggplot(aes(mean_cap, pct_hash)) +
  geom_col() +
  geom_text(aes(label = unique_user))


original_hashtag <- user_relation %>%
  select(screen_name, timestamp, n_hash, mean_cap) %>%
  unnest(n_hash) %>%
  mutate(date = lubridate::ymd_hms(timestamp),
         ano = as.factor(year(date)),
         n_hash = tolower(n_hash)) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  group_by(n_hash) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(original = T)

user_relation %>%
  select(screen_name, timestamp, n_hash, mean_cap) %>%
  unnest(n_hash) %>%
  mutate(date = lubridate::ymd_hms(timestamp),
         ano = as.factor(year(date)),
         n_hash = tolower(n_hash)) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  left_join(original_hashtag) %>%
  mutate(original = if_else(is.na(original), F, T),
         date = as.Date(timestamp)) %>%
  ggplot(aes(timestamp, mean_cap)) +
  geom_point(aes(color = original))



original_hashtag %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  group_by(mean_cap) %>%
  summarise(users = n_distinct(screen_name),
            hashtags = n_distinct(n_hash),
            new_hash_user = hashtags/users) %>%
  filter(!is.na(mean_cap)) %>%
  ggplot(aes(mean_cap, new_hash_user)) +
  geom_col() +
  labs(title = "New hashtags per user",
       subtitle = "Well distributed center of creation",
       x = "Botometer score",
       y = "New hashtags per user",
       caption = "SMART Data sprint") +
  theme_minimal()


user_relation %>%
  select(screen_name, timestamp, n_hash, mean_cap) %>%
  unnest(n_hash) %>%
  mutate(date = lubridate::ymd_hms(timestamp),
         ano = as.factor(year(date)),
         n_hash = tolower(n_hash)) %>%
  filter(date >= as.Date("2017-01-01")) %>%
  group_by(n_hash) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(mean_cap = round(mean_cap, 1),
         ano = year(timestamp)) %>%
  group_by(ano, mean_cap) %>%
  summarise(users = n_distinct(screen_name),
            hashtags = n_distinct(n_hash),
            new_hash_user = hashtags/users) %>%
  filter(!is.na(mean_cap)) %>%
  ggplot(aes(mean_cap, new_hash_user)) +
  geom_col() +
  facet_wrap(~ano) +
  labs(title = "New hashtags per user",
       subtitle = "Well distributed center of creation",
       x = "Botometer score",
       y = "New hashtags per user",
       caption = "SMART Data sprint") +
  theme_bw()



# Link use

counted_hashtags %>%
  pivot_longer(-1) %>%
  ggplot(aes(mean_cap, name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "#FADF63", high = "#B3001B") +
  geom_text(aes(label = round(value, 1))) +
  labs(title = "Presence of mentions, links and hashtags per post",
       subtitle = "Divided by bot score",
       x = "Botometer score",
       y = "",
       fill = "Number",
       caption = "SMART Data Sprint") +
  theme_minimal()
  

ggsave("graphics/presence_of_mentions.png")

unnested_tw %>%
  count(class, word) %>%
  group_by(class) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup() %>%
  filter(!is.na(class)) %>%
#  filter(class %in% c("medium", "high")) %>%
  select(word, class, pct) %>%
  pivot_wider(names_from = class, values_from = pct) %>%
  pivot_longer(-(1:2)) %>%
  mutate_at(vars(high, value), ~replace_na(., 0)) %>%
  filter(high > 0.000005 & value > 0.000005) %>%
  ggplot(aes(high, value)) +
  geom_abline(lty = 2) +
  geom_jitter(alpha = 0.2, size = 2.5) +
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  facet_wrap(~name) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Words by botometer score",
       x = "high",
       y = "",
       caption = "SMART Data Sprint")


# Do TF-IDF for groups of percentages

class_tf_idf <- unnested_tw %>%
  count(class, word) %>%
  bind_tf_idf(word, class, n)

class_tf_idf %>%
  group_by(class) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, class), fill = class)) +
  geom_col(show.legend = F) +
  facet_wrap(~class, scales = "free") +
  scale_y_reordered() +
  labs(title = "Relation of tf_idf between classes",
       y = "") +
  theme_bw()

ggsave("tf_idf.pdf")

# Ver word distribution por medida

unnested_tw %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  count(mean_cap, word) %>%
  group_by(mean_cap) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  filter(pct > 0.005) %>%
  ggplot(aes(mean_cap, pct)) +
  geom_point(aes(color = word), show.legend = F) +
  geom_text(aes(label = word), check_overlap = T)



unnested_tw %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  count(mean_cap, word) %>%
  group_by(mean_cap) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  filter(pct > 0.005 & !is.na(mean_cap)) %>%
  select(-total, -n) %>%
  write_excel_csv('hashtag_mean_cap.csv')


unnested_tw %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  count(mean_cap, word) %>%
  group_by(mean_cap) %>%
  mutate(total = sum(n),
         pct = n/total) %>%
  ungroup() %>%
  filter(pct > 0.005 & !is.na(mean_cap)) %>%
  group_by(word) %>%
  mutate(how_present = n()) %>%
  ggplot(aes(fct_reorder(word, how_present), mean_cap)) +
  geom_point(aes(size = pct, color = pct)) +
  coord_flip() +
  theme_bw() +
  labs(title = "Word distribution by bolsometer score",
       y = "bolsometer score",
       x = "")


word_by_botmeter <- unnested_tw %>%
  mutate(mean_cap = round(mean_cap, 1)) %>%
  group_by(class, mean_cap, word) %>%
  summarise(n_word = n()) %>%
  ungroup() %>%
  arrange(-n_word) %>%
  mutate(mean_cap = replace_na(mean_cap, 0)) %>%
  filter(!is.na(class)) %>%
  group_by(class) %>%
  mutate(total_class = sum(n_word),
         pct_word = n_word/total_class)
  

word_by_botmeter %>%
  write_excel_csv("words_for_alluvial.csv")

major_word <- word_by_botmeter %>%
  mutate(mean_cap = ifelse(nchar(round(mean_cap, 1)*10) == 2, round(mean_cap, 1)*10, str_c("0", round(mean_cap, 1)*10))) %>%
  group_by(mean_cap) %>%
  slice_max(n_word, n = 10)

major_word %>%
  write_excel_csv("words_by_cap.csv")

  
word_group <- unnested_tw %>%
  group_by(class, word) %>%
  summarise(n_word = n()) %>%
  ungroup() %>%
  arrange(-n_word) %>%
  group_by(class) %>%
  slice_max(n_word, n = 20)


word_group %>%
  filter(!is.na(class)) %>%
  write_excel_csv("word_class.csv")

# Get tweet dates --------------------------------------------------------------

full_dataset <- list.files("all_tweets", full.names = T) %>%
  map_df(import, setclass = "tibble") %>%
  mutate(timestamp = as.Date(timestamp))

unique_words <- unnested_tw %>%
  mutate(timestamp = as.Date(timestamp),
         year = year(timestamp)) %>%
  select(year, word, class, mean_cap)

unique_words %>%
  select(-mean_cap) %>%
  count(year, word, class) %>%
  filter(!is.na(class)) %>%
  group_by(year, word) %>%
  mutate(classes = n()) %>%
  group_by(year, class) %>%
  mutate(original = classes == 1) %>%
  group_by(year, class) %>%
  summarise(pct_original = sum(original)/n()) %>%
  ungroup() %>%
  ggplot(aes(year, pct_original)) +
  geom_point(aes(color = class)) +
  geom_line(aes(group = class,
                color = class)) +
  labs(title = "Unique words by year",
       subtitle = "Progressivelly less differences",
       y = "Percentage of original words") +
  theme_bw()

ggsave("unique_words.pdf")


# What are the unique words

unique_words %>%
  select(-mean_cap) %>%
  count(year, word, class) %>%
  filter(!is.na(class)) %>%
  group_by(year, word) %>%
  mutate(classes = n()) %>%
  group_by(year, class) %>%
  mutate(original = classes == 1) %>%
  filter(original == T) %>%
  arrange(-n)



unique_words %>%
  select(-mean_cap) %>%
  count(year, word, class) %>%
  filter(!is.na(class)) %>%
  group_by(year, word) %>%
  mutate(classes = n()) %>%
  group_by(year, class) %>%
  mutate(original = classes == 1) %>%
  group_by(year, class) %>%
  summarise(pct_original = sum(original)/n()) %>%
  ungroup() %>%
  write_excel_csv("unique_words.csv")

by_year <- unnested_tw %>%
  mutate(timestamp = as.Date(timestamp),
         year = year(timestamp)) %>%
  select(word, year, mean_cap) %>%
  mutate(type = mean_cap > 0.8) %>%
  count(year, type, word) %>%
  group_by(word) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(total > 2000 & year > 2017) %>%
  pivot_wider(names_from = type, values_from = n) %>%
  select(-`NA`) %>%
  mutate_at(vars(`TRUE`, `FALSE`), ~replace_na(., 0)) %>%
  select(word, total, type) %>%
  arrange(-total) %>%
  filter(!is.na(type))

by_year %>%
  mutate_at(vars(`2018`:`2021`), ~./sum(.)) %>%
  select(-total) %>%
  pivot_longer(`2018`:`2020`) %>%
  ggplot(aes(`2021`, value)) +
  geom_abline(lty = 2) +
  geom_jitter(alpha = 0.2, size = 2.5) +
  geom_text(aes(label = word), check_overlap = T) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  facet_grid(type~name) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Words by botometer score yearly",
       x = "2021",
       y = "",
       caption = "SMART Data Sprint")



  

by_year_pct %>%
  select(-total) %>%
  pivot_longer(-1) %>%
  rename(ano = name, pct = value) %>%
  write_excel_csv("bot_words.csv")



full_dataset %>%
  ggplot(aes(timestamp)) +
  geom_histogram()
