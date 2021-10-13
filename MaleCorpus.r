library(gutenbergr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(textdata)
data("stop_words")

# Easily change mirror if needed
gutenmirror <- "http://mirrors.xmission.com/gutenberg" 
# Load Male Corpus
MaleCorpus <- gutenberg_download(c(1399,1400,967,805,2638,82,36034,2833,996,17460,821,23058,
                                       1608,107,20533,53416,3155,15265,963,23727,524),
                                     mirror=gutenmirror,
                                     meta_fields = c("title", "author"))

# Clean beginning and end of all novels
MaleCorpus_cleaned <- MaleCorpus[-c(1:900, 21183:21344, 37772:37829, 49883:49931, 61282:61360, 100988:101158,
                                    137863:138070, 175056:177261, 214669:214699, 254514:254592, 274911:274918, 
                                    282986:283007, 310126:310699, 322972:323245, 334595:334740, 349087:349533,
                                    375519:375595, 390491:390500, 390960:391032, 408670:408729, 420524:420604), ]

# Tokenize with added column "line"
MCToken <- MaleCorpus_cleaned %>% 
  mutate(line = row_number()) %>% 
  unnest_tokens(word, text)

# Stop words removed
MC_SRR <- MCToken %>% anti_join(stop_words)

# ASSINGMENT ----------

## A: TOP 20 NON-STOP WORDS
MaleCorpusCount <- MC_SRR %>%
  count(word, sort=TRUE)
MaleCorpusCount[1:20,]%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab("Word")+
  ylab("No. of Occurrences")+
  ggtitle("Most Common Words, Male Corpus")

ggsave("top_twenty_non_stop_words_MALE.png", path="Images", scale = 1)

### BING LEXICON - QUESTIONS B-D
BING <- get_sentiments("bing") %>% filter(word != "miss")
MaleCorpusBING <- MC_SRR %>%
  inner_join(BING)%>%
  count(title = title, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentimentratio = 100*negative/(positive + negative)) %>%
  mutate(sentiment = positive - negative)

## B: 10 MOST NEGATIVE NOVELS
MaleCorpusBING %>% arrange(sentimentratio) %>%
  mutate(title = reorder(title, sentimentratio)) %>%
  head(10) %>%
  ggplot() +
  aes(title, sentimentratio) +
  geom_col() + 
  xlab("Title of Work") +
  ylab("Sentiment Ratio (Negative score/total score)") +
  ggtitle("10 Most Negative Novels, Male Corpus") +
  coord_flip()

ggsave("ten_most_negative_novels_MALE.png", path="Images", scale = 1, width = 8)

## C: OVERALL SENTIMENT
sum(MaleCorpusBING$sentiment)

## D: 
InfluentialWords <- MC_SRR %>%
  inner_join(BING)%>%
  count(word = word, sentiment, sort = TRUE)
PositiveInfluentialWords <- InfluentialWords %>%
  filter(sentiment == "positive")
NegativeInfluentialWords <- InfluentialWords %>%
  filter(sentiment == "negative")

PositiveInfluentialWords %>%
  mutate(word = reorder(word, n)) %>%
  head(15) %>%
  ggplot() +
  aes(word, n) +
  geom_col() + 
  xlab("Word") +
  ylab("Number of Occurences") +
  ggtitle("Influential Positive Words, Male Corpus") +
  coord_flip()

ggsave("positive_influential_words_MALE.png", path="Images", scale = 1)

NegativeInfluentialWords %>%
  mutate(word = reorder(word, n)) %>%
  head(15) %>%
  ggplot() +
  aes(word, n) +
  geom_col() + 
  xlab("Word") +
  ylab("Number of Occurences") +
  ggtitle("Influential Negative Words, Male Corpus") +
  coord_flip()

ggsave("negative_influential_words_MALE.png", path="Images", scale = 1)
  

### NRC LEXICON - QUESTIONS E&F
NRC <- lexicon_nrc()

## E: TOP 15 JOY & ANGER WORDS
NRCjoy <- NRC %>%
  filter(sentiment == "joy")
WCjoy <- MCToken %>%
  inner_join(NRCjoy)%>%
  count(word, sort = TRUE)%>%
  top_n(15)
WCjoy%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab("Word")+
  ylab("No. of Occurrences")+
  ggtitle("Top 15 Joyful Words, Male Corpus")

ggsave("top_15_joyful_words_MALE.png", path="Images", scale = 1)

NRCanger <- NRC %>%  
  filter(sentiment == "anger") %>%
  filter(word != "words")
  
WCanger <- MCToken%>%
  inner_join(NRCanger)%>%
  count(word, sort = TRUE)%>%
  top_n(15)
WCanger%>%
  mutate(word = reorder(word, n)) %>%
  ggplot()+
  aes(word,n)+
  geom_col()+
  coord_flip()+
  xlab("Word")+
  ylab("No. of Occurrences")+
  ggtitle("Top 15 Angry Words, Male Corpus")

ggsave("top_15_anger_words_MALE.png", path="Images", scale = 1)

## F: WHICH NOVEL IN CORPUS HAS HIGHEST DISGUST
NRCdisgust <- NRC %>%
  filter(sentiment == "disgust")
WCdisgust <- MCToken %>%
  inner_join(NRCdisgust) %>%
  count(title, sort=TRUE) %>%
  head(1)

### AFINN LEXICON - QUESTIONS G&H
AFINN <- lexicon_afinn()

MaleCorpusAFINN <- MCToken %>%
  inner_join(AFINN)%>%
  group_by(title) %>%
  summarise(sentiment = sum(value))

