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
  xlab(NULL)+
  ylab("No. of Occurrences")+
  ggtitle("Most Common Words in 'Love Stories by Men' Corpus")

### BING LEXICON - QUESTIONS B-D
BING <- get_sentiments("bing")
MaleCorpusBING <- MC_SRR %>%
  inner_join(BING)%>%
  count(title = title, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)

## B: 10 MOST NEGATIVE NOVELS
MaleCorpusBING %>% arrange(sentiment) %>%
  mutate(title = reorder(title, -sentiment)) %>%
  head(10) %>%
  ggplot() +
  aes(title, sentiment) +
  geom_col() + 
  xlab("Title of Work") +
  ylab("Sentiment (BING)") +
  ggtitle("10 Most Negative Novels in 'Love Stories by Men' Corpus") +
  coord_flip()

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
  xlab(NULL)+
  ylab("No. of Occurrences")+
  ggtitle("Top 15 Joyful Words, Male Corpus")

NRCanger <- NRC %>%  
  filter(sentiment == "anger")
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
  xlab(NULL)+
  ylab("No. of Occurrences")+
  ggtitle("Top 15 Angry Words, Male Corpus")

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

