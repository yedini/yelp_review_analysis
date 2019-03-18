####recommenderlab####
install.packages("recommenderlab")
library(recommenderlab)
ex <- business %>% select(business_id, review_id, price, card, reservation, noise, alchol, star1)
head(ex)
num <- createDataPartition(y=ex$star1, p=0.001, list=FALSE)
train <- ex[num,]

train <- as.data.frame(train)
r <- as(train, "realRatingMatrix")
r <- Recommender(r, method="POPULAR")
recom <- predict(r, train, n=10)

review <- readRDS("review1_score.rds")

id <- review %>% select(business_id, user_id, review_id)

users <- review %>% select(business_id, user_id, review_id) %>%
  inner_join(ex, by="review_id", "business_id")



data <- data.frame(id=users$review_id, user=users$user_id,
                   business=users$business_id.x, rating=users$star1)

rating_df <- data %>% 
  mutate(userid=str_c("u", user),
         reviewid=str_c("m", id))


sam <- createDataPartition(rating_df$rating, p=0.0005, list=FALSE)
sample <- rating_df[sam,]

sample <- sample[-c(300, 532),]

rating_mat <- spread(sample, id, rating) %>%
  remove_rownames() %>%
  column_to_rownames(var='userid')

rating_rrm <- as(as(rating_mat, "matrix"), "realRatingMatrix")
