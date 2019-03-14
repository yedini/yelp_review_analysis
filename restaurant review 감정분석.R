library(tidyverse)
setwd("C:/Users/dcng/Documents/비타민_프로젝트")
library(tm)
library(qdap)
library(caret)
library(tidytext)

#review, business
review <- readRDS("review1_text.rds")
review1_score <- readRDS("review1_score.rds")
business <- readRDS('business.rds')
review$business_id <- review1_score$business_id
review$stars <- review1_score$stars
rm(review1_score)

#join
review1 <- business %>% select(business_id, categories) %>%
  inner_join(review, by="business_id")

#restaurant만 filtering
restaurant <- review1 %>% filter(categories=="Restaurants")



text <- data_frame(id=1:nrow(restaurant), doc=restaurant$text)
text$doc <- mgsub(c("\\\\n","\\n","\\"),"",text$doc)

text <- readRDS("sentiment_rest_text_0312.rds")


#텍스트 나누기
text1 <- text[1:100000,]
text2 <- text[100001:300000,]
text3 <- text[300001:500000,]
text4 <- text[500001:700000,]
text5 <- text[700001:900000,]
text6 <- text[900001:1100000,]
text7 <- text[1100001:1228724,]

#text1
text_word1 <- text1 %>% unnest_tokens(word, doc)

result1 <- text_word1 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result1 <- result1[c(1,4)]
result1

#text2
text_word2 <- text2 %>% unnest_tokens(word, doc)

result2 <- text_word2 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result2 <- result2[c(1,4)]
result2


#text3
text_word3 <- text3 %>% unnest_tokens(word, doc)

result3 <- text_word3 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result3 <- result3[c(1,4)]
result3

#text4
text_word4 <- text4 %>% unnest_tokens(word, doc)

result4 <- text_word4 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result4 <- result4[c(1,4)]
result4

#text5
text_word5 <- text5 %>% unnest_tokens(word, doc)

result5 <- text_word5 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result5 <- result5[c(1,4)]
result5


#text6
text_word6 <- text6 %>% unnest_tokens(word, doc)

result6 <- text_word6 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result6 <- result6[c(1,4)]
result6

#text7
text_word7 <- text7 %>% unnest_tokens(word, doc)

result7 <- text_word7 %>% inner_join(get_sentiments('bing')) %>%
  count(word, id, sentiment) %>% spread(sentiment, n, fill=0) %>%
  group_by(id) %>% summarise(pos.sum=sum(positive), neg.sum=sum(negative), score=pos.sum-neg.sum)
result7 <- result7[c(1,4)]
result7

#text에 대한 감정분석 결과 하나로 합치기
result <- rbind(result1, result2, result3, result4, result5, result6, result7)

#restaurant에 대한 review 데이터 불러오기
review <- readRDS("join_restaurant_0312.rds")
str(review)

#join을 위해 review데이터에 인덱스 열 추가하기
review <- review %>% mutate(index=1:nrow(review))

#감정분석 결과와 review 조인하기
review <- review %>% inner_join(result, by=c("index"="id"))
review <- review %>% rename(sentiment=score)
str(review)

#rds파일로 저장
saveRDS(review, "restaurant_review.rds")

##감정점수 탐색
ggplot(review, aes(sentiment))+geom_histogram(binwidth=1)+ggtitle("sentiment 분포")
ggplot(review, aes(sentiment))+geom_histogram(binwidth=1)+ggtitle("sentiment 분포_outlier")+
  coord_cartesian(xlim=c(-50,-15), ylim=c(0,100))
ggplot(review, aes(sentiment))+geom_histogram(binwidth=1)+ggtitle("sentiment 분포_outlier")+
  coord_cartesian(xlim=c(30,90), ylim=c(0,100))


#sentiment normalization
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

score <- normalize(review$sentiment)
hist(score)
review$sentiment <- normalize(review$sentiment)
review$sentiment <- review$sentiment*4+1
