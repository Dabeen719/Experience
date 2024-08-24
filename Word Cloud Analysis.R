# 작업폴더 경로 확인

getwd()

# 저장할 폴더 지정

setwd("C:/Users/hanshin07/Desktop/2021년/1학기/통계활용사례연구/워드 클라우드분석")

getwd()


# 라이브러리 불러오기

library("rvest")
library("R6")
library(tm)
library(KoNLP)
library(wordcloud)
library(wordcloud2)

# 리뷰를 저장한 공간 만들기
all.reviews <- c()

# 200페이지까지 크롤링하기 위해 200번 반복
  
  # 크롤링할 사이트 주소 만들기
  
  url <- paste('https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=187310&target=after&page=', page, sep='')
  
  # read_html 함수를 사용하여 html 페이지를 htxt 변수에 저장
  htxt <- read_html(url,encoding="CP949")
  
  # html_nodes 함수를 사용하여 list_netizen class를 table 변수에 저장
  table <- html_nodes(htxt,'.list_netizen')
  
  # html_nodes 함수를 사용하여 title class를 content 변수에 저장
  content <- html_nodes(table, '.title')
  
  # html_text 함수를 사용하여 text를 reviews 변수에 저장
  reviews <- html_text(content)
  
  # 리뷰가 없으면 더이상 반복문을 실행하지 않습니다.
  if(length(reviews)==0){break}
  
  # 리뷰 병합
  all.reviews <- c(all.reviews, reviews)
  
  # 크롤링한 페이지를 출력
  print(page)
}



# 저장된 리뷰를 write.table 함수를 사용하여 txt파일로 저장
write.table(all.reviews, 'naver_review.txt')


#저장된 파일 불러오기
tgt1 <- readLines("naver_review.txt")
tgt1 <-gsub("별점","",tgt1) #별점이란 단어 제거

#사전 정하기
useNIADic()

#파일 정리
tgt1 <- sapply(tgt1,
               extractNoun,
               USE.NAMES = F,
               autoSpacing=T)

tgt2 <- unlist(tgt1)
tgt3 <- stringr::str_replace_all(tgt2,'[^[:alpha:]]','')
tgt4 <- Filter(function(x){nchar(x)>1},tgt3)
tgt4 <- unlist(tgt4)
tgt5 <- table(tgt4)
tgt5

# 빈도 계산 함수와 출력
wordFreq<-tgt5
wordFreq
# 정렬
sort(wordFreq, decreasing=T)

# bar 그래프
barplot(wordFreq, las=2)

#글자 색 정하기
display.brewer.all()
pal <- brewer.pal(8,"Paired")

set.seed(1234)
wordcloud(
  names(tgt5),
  freq = tgt5,
  scale = c(3,1.5), #단어크기
  rot.per =0.1, # 회전비율
  min.freq = 20, # 최저 빈도수 2회이상
  max.words = 50, #최대 단어 수
  random.order = F, # 고빈도 단어 중앙배치
  random.color = F,
  colors = pal
)
