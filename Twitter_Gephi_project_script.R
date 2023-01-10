
# Step 1 Install Packages for network and sentiment analysis
install.packages("dplyr")
install.packages("rtweet")
install.packages("sentimentr")
install.packages("syuzhet")
install.packages("tidyr")
install.packages("tidytext")
install.packages("rtweet")

# Step 2 Load Packages for network and sentiment analysis
library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(sentimentr)
library(syuzhet)
library(writexl)

#STEP 3: Collect Selected Hashtag Twitter data without retweets - replace selectedhashtag
com<-search_tweets("#selectedhashtag", n = 1000, include_rts = FALSE)

------------------------------------------------------------------------

#SENTIMENT 
tweets<-tibble(text = com $text)
tweets$text<-gsub("@\\w+ *", "", tweets$text)
tweets$text<-gsub("#\\w+ *", "", tweets$text)
tweets$text<-gsub("t.co/|https|http|&amp;", "", tweets$text)
tweets$text<-gsub("1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16", "", tweets$text)

#STEP 4: CLEAN DATA AND COUNT KEYWORDS, BIGRAMS, AND HASHTAGS
#COPY TWEETS TO NEW DATAFRAME & CLEAN DATA

keywords<-tweets %>% 
  unnest_tokens(word, text)%>% 
  filter(!word %in% stop_words$word) %>% 
  count(word, sort = TRUE)

bigrams<-tweets %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams<-bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams<-bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>% 
  count(word1, word2, sort = TRUE)

hashtags<-tibble(text = tolower(unlist(com$hashtags)))
hashtags<-hashtags %>% count(text, sort = TRUE)
hashtags<-na.omit(hashtags)

#STEP 5: CONDUCT SENTIMENT ANLAYSIS OF DATASET WITH DIFFERENT SENTIMENT LEXICONS
sentiment_sentimentr<-sentiment_by(com$text)
sentiment_sentimentr<-tibble(sentimentr = sentiment_sentimentr$ave_sentiment)
sentiment_afinn<-tibble(afinn = get_sentiment(com$text, method = "afinn"))
sentiment_bing<-tibble(bing = get_sentiment(com$text, method = "bing"))
sentiment_nrc<-tibble(nrc = get_sentiment(com$text, method = "nrc"))
sentiment_syuzhet<-tibble(syuzhet = get_sentiment(com$text, method = "syuzhet"))

#STEP 6: COPY SENTIMENT SCORES TO ORIGINAL DATASET, DELETE WORKING FILES
dataset<-cbind(com, sentiment_sentimentr, sentiment_afinn, sentiment_bing, sentiment_nrc, sentiment_syuzhet)
rm(sentiment_afinn, sentiment_bing, sentiment_nrc, sentiment_sentimentr, sentiment_syuzhet)

#STEP 7: RUN NRC EMOLEX SENTIMENT ANALYSIS ON @ISUREDBIRDS
com_emotions<-get_nrc_sentiment(com$text)
com_emotions$text<-com$text

#C. GET NRC SENTIMENT VALUES - INCLUDES TOTALS
nrc_sentiment<-get_nrc_sentiment(tweets$text, language = "english")
nrc_sentiment$screen_name<-com$screen_name
nrc_sentiment$text<-com$text
nrc_totals<-tibble("anger" = sum(nrc_sentiment$anger),
                   "anticipation" = sum(nrc_sentiment$anticipation),
                   "disgust" = sum(nrc_sentiment$disgust),
                   "fear" = sum(nrc_sentiment$fear),
                   "joy" = sum(nrc_sentiment$joy),
                   "sadness" = sum(nrc_sentiment$sadness),
                   "surprise" = sum(nrc_sentiment$surprise),
                   "trust" = sum(nrc_sentiment$trust),
                   "negative" = sum(nrc_sentiment$negative),
                   "positive" = sum(nrc_sentiment$positive)
)

#D. CALCULATE NRC SENTIMENT VOLUME
nrc_sentiment$net_valence <- nrc_sentiment$positive - nrc_sentiment$negative
sentiment_volume<-tibble("positive" = sum(nrc_sentiment$net_valence > 0),
                         "negative" = sum(nrc_sentiment$net_valence < 0),
                         "undetermined" = sum(nrc_sentiment$net_valence == 0)
)

#3. MAXIMUM POTENTIAL AUDIENCE
influencers_by_reach<-tibble(screen_name = com$screen_name,
                             followers = com$followers_count)
influencers_by_reach<-distinct(influencers_by_reach, screen_name, .keep_all = TRUE)
  
maximum_potential_audience<-tibble("maximum potential reach" = sum(influencers_by_reach$followers))

#Export Dataset for base tweets change "" to preferred output
#rtweet::save_as_csv(com, "preferredoutputplaceholderhere.csv")

------------------------------------------------------------------------
  
#NETWORK ANALYSIS
  
#GET SOURCE & TARGET EDGES FOR MENTIONS AND HASHTAGS 
  
#MENTIONS EDGE TABLE
  
mentions_edges<-com %>% filter(mentions_screen_name != "")
mentions_edges<-tibble(source = mentions_edges$screen_name,
                       target = mentions_edges$mentions_screen_name)
mentions_edges$target<-gsub("c\\(","", mentions_edges$target)
mentions_edges<-mentions_edges %>% 
  unnest_tokens(target, target, to_lower = FALSE)

#HASHTAGS EDGE TABLE
hashtag_edges<-com %>% filter(hashtags != "")
hashtag_edges<-tibble(source = hashtag_edges$screen_name,
                      target = hashtag_edges$hashtags)
hashtag_edges$target<-paste0("#", hashtag_edges$target)
hashtag_edges$target<-gsub("c\\(","", hashtag_edges$target)
hashtag_edges<-hashtag_edges %>% 
unnest_tokens(target, target, to_lower = FALSE)


#EXPORT DATASET (HOME FOLDER FOR MAC, DOCUMENTS FOR WINDOWS)

sheets<-list("com_results" = com,
            "com_emotions" = com_emotions,
            "com_sentiment_per_tweet" = nrc_sentiment,
            "com_sentiment_totals" = nrc_totals,
            "com_sentiment_total_volume" = sentiment_volume,
            "bigrams" = bigrams,
            "influencers_by_reach" = influencers_by_reach,
            "maximum_potential_audience" = maximum_potential_audience,
            "most_active_accounts" = most_active_accounts,
            "most_replied_to" = most_replied_to,
            "Keywords" = keywords,
            "Hashtags" = hashtags,
            "locations" = locations,
            "Network_edge_table" = edge_table,
            "Network_hashtag_edges" = hashtag_edges,
            "Network_mentions_edges" = mentions_edges
)         
          
write_xlsx(sheets, paste0(format(Sys.time(), "%d-%b-%Y_%H.%M.%S"), ".xlsx"))
