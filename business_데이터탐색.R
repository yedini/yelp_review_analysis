##======business_데이터탐색========

#분석에 사용하지 않을 열 삭제
business <- business %>% select(-name, -neighborhood, -address, -postal_code, -latitude, -longitude)

str(business)

##city열
length(unique(business$city))
#서비스가 많이 위치해 있는 city
business %>% group_by(city) %>% summarise(n=n()) %>% arrange(desc(n)) 
#city별 서비스 갯수의 분ㅍ
business %>% group_by(city) %>% summarise(n=n(), sum=sum(review_count)) %>% arrange(desc(n))

##state
length(unique(business$state))
#서비스가 많이 위치해있는 state
business %>% group_by(state) %>% summarise(n=n()) %>% arrange(desc(n)) #AZ-Arizona, NV-Nevada, 

#stars - 별점 분포확인
summary(business$stars)
ggplot(business, aes(stars))+geom_histogram(binwidth=0.5)

#review_count-분포확인
summary(business$review_count)
ggplot(business, aes(review_count))+geom_histogram()
business %>% arrange(desc(review_count))
#리뷰갯수가 2000개 이상인 경우만 확대
ggplot(business, aes(review_count))+geom_histogram()+coord_cartesian(xlim=c(2000,8000), ylim=c(0,100))
business %>% filter(review_count>=2000) %>% group_by(city) %>% summarise(n=n()) %>% arrange(desc(n)) 
#리뷰갯수가 2000개 이상인 서비스의 경우 las vegas가 압도적으로 많음
business %>% filter(review_count>=2000) %>% group_by(state) %>% summarise(n=n()) %>% arrange(desc(n)) 
#리뷰갯수가 2000개 이상인 서비스의 경우 NV주가 압도적으로 많음

#is_open
table(factor(business$is_open))




business1 <- business %>% transmute(parking=grepl('BusinessParking', attributes))
sum(business1$parking)/nrow(business1)



business %>% select(attributes) %>% mutate(attr_count=stri_count(business$attributes, fixed=",")) %>% arrange(desc(mutate))