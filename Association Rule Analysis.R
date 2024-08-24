# 연관규칙 분석(장바구니분석)

library(arules)
library(arulesViz) 
library(datasets)

# 기본 data set
# R 제공 data

getwd()

setwd("C:/jsbyun/2021/통계활용사례연구_R")
setwd("RData/예제/연관규칙분석"); getwd()

ex1<- read.csv("예제한글자료1.csv")
#ex11<- read_xlsx("예제한글자료1.xlsx", sheet=1, col_names = TRUE)  # library(readxl)

ex1$품목[ex1$거래=="A"]

# 거래별 목록 구분
ex1.목록<- split(ex1$품목, ex1$거래) # split 순서에 주의
#ex11.목록<-  split(ex11$품목, ex11$거래)

# 트랜잭션 데이터로 변환
# 거래별 목록이 정리되어 있으면 read.transations 이용
ex1.tran<- as(ex1.목록, "transactions")
# ex1.tran<- as(ex1.목록, "transactions")
# 거래에 중복 구매 품목이 있는 경우
# ex1.목록<- sapply(ex1$목록, unique)
# ex1.tran<- as(ex1.목록, "transactions")

# csv 파일로 부터 직접 변환: 품목만 있는 경우(해당 예제 불가)
# header : 변수 이름 여부
# ex11.tran<- read.transactions("예제한글자료1.csv", format="basket", 
#                              sep=",", header=T, rm.duplicates=T)

summary(ex1.tran)

# 상품정보
itemInfo(ex1.tran)

# 히트 맵
image(ex1.tran)
image(head(ex1.tran))
image(head(ex1.tran, 5))

# 상품 정보 빈도와 %
item_ex<- itemFrequency(ex1.tran) # 거래별 상대빈도
item_Gr<- itemFrequency(ex1.tran, type="absolute") # absolute 빈도
sort(item_Gr, decreasing = T)

# 상위 5개 품목에 대한 상품 정보와 막대그림
itemFrequencyPlot(ex1.tran, topN=5, type="absolute")
itemFrequencyPlot(ex1.tran, topN=5, main="상위 5개 구매 품목 ")
itemFrequencyPlot(ex1.tran, support=0.3, main="지지도 0.3이상 품목 ")

########## 연관 규칙 분석 : apriori 함수를 이용
##### market_1.R 파일과 동일
##### 데이터를 Groceries --> ex1.tran 로 변경하면 동일
ex1.rules<- apriori(ex1.tran)
summary(ex1.rules)
inspect(ex1.rules[1:10])

ex1.rules<- apriori(ex1.tran, parameter = list(supp= 0.03, conf =0.5)) 
summary(ex1.rules)
inspect(ex1.rules[1:10])

##### Visualization 1
# library(arulesViz) 
plot(ex1.rules)

plot(ex1.rules, method="grouped")
plot(ex1.rules, method="graph")

ex1.rules<-sort(ex1.rules, decreasing=TRUE,by="confidence") 
inspect(ex1.rules[1:5]) 

plot(ex1.rules, method="graph") 
plot(ex1.rules[1:10], method="graph") 

x11()
plot(ex1.rules[1:20], method="graph")

##### Visualization 2
# library(d3Network) 
d3SimpleNetwork(ex1)
d3SimpleNetwork(ex1, file="ex1tran.html") # 저장 폴더에서 확인
