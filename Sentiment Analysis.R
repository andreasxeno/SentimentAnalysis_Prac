
remove(list  = ls())
setwd("~/OneDrive - University of New Haven/Spring 2022/BANL 6420-Unsupervised Machine Learning/Week 9 3.21.2022")
        
        # Packages ========================================================
        pacman::p_load(tidytext, tidyverse)
        pacman::p_load(quanteda)
        pacman::p_load(ggthemes)
        pacman::p_load(scales)


#====================================== #
#The Unabomber ========================

bomber =readLines("UnabomberManifesto.txt")
head(bomber)



      bomber = str_remove_all(bomber, "^[0-9]*$")



text = bomber %>% str_remove_all("\n") %>% 
  str_trim(side = c("both")) %>% 
  str_squish() %>%
  strsplit(split = " ") %>%
  purrr::reduce(paste)


  text


text_df <- tibble(line = 1:length(text), text = text)
#text_df <- tibble(text = text)
      
          tail(text_df)
          head(text_df,30)
          str(text_df)
          

prod_tokens = text_df %>%
  unnest_tokens(word, text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>% 
  anti_join(stop_words) %>%
  distinct() 

head(prod_tokens)    
tail(prod_tokens)


bomber_tokens = cbind.data.frame(linenumber = row_number(prod_tokens$word), prod_tokens)


bomber_sentiment = 
  bomber_tokens %>%
    inner_join(get_sentiments("nrc")) %>%
      count(index = linenumber %/% 80, sentiment) %>% #breaking out in different groups, a moduar division 
        spread(sentiment, n, fill = 0) %>%
          mutate(sentiment = positive - negative)

ggplot(bomber_sentiment, aes(index, sentiment, fill = as.factor(sentiment ))) +
  geom_col(show.legend = TRUE) + 
  theme(legend.position = "bottom")


                bomber_sentiment = 
            bomber_tokens %>%
          inner_join(get_sentiments("nrc")) %>%
        count(index = linenumber %/% 200, sentiment) 
    
                  ggplot(bomber_sentiment, aes(index, n, fill = as.factor(sentiment ))) +
                    geom_col(show.legend = TRUE) + 
                    theme(legend.position = "bottom")


    bomber_sentiment =
      bomber_tokens %>%
        inner_join(get_sentiments("nrc")) %>%
          count(index = linenumber %/% 80, sentiment) %>%
            filter(sentiment == "anger"|sentiment == "disgust")
    
    ggplot(bomber_sentiment, aes(index, n, fill = as.factor(sentiment ))) +
      geom_col(show.legend = TRUE, position  = "dodge") +
      theme(legend.position = "bottom")

    
    # show a comparable graph displaying positive and negative sentiment across each segment
    
get_sentiments("afinn")
afinn_dict    

nrc_affin = afinn_dict %>% full_join(nrc_dict)

bomber_sentiment = 
  bomber_tokens %>%
  inner_join(nrc_affin) %>%
  count(index = linenumber %/% 80, sentiment) %>%
  group_by(index, sentiment) %>%
  #spread(sentiment, n, fill = 0) %>%
  mutate(max_sent = max(n))

head(bomber_sentiment)

bomber_sentiment %>% 
  dplyr::filter(sentiment != "negative", sentiment != "positive") %>%
  group_by(index) %>% 
  top_n(1, max_sent) %>%
  ggplot(aes(index, max_sent, fill = sentiment)) + 
  geom_col(show.legend = TRUE)


