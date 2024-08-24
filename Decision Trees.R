#작업공간 확인 및 설정
setwd("C:/Users/hanshin07/Desktop/2021년/1학기/통계활용사례연구/의사결정트리과제"); getwd

library(tree)
nbs<-read.csv("NBS_28.csv")
nbs
str(nbs) #454개의 데이터, 18개의 변수
tail(nbs)
nrow(nbs) 
summary(nbs)

#결측갑 확인
sum(is.na(nbs))
#해당 문항에 무응답 존재하는지 확인
a<-nbs[nbs$Q2>13,]
a #무응답 등에 해당하는 사람 없음을 확인할 수 있음

nbs$Q2<-ifelse(nbs$Q2==7,"윤석열","이재명") #변수의 데이터 값 이름 변경(7:윤석열/9:이재명)
class(nbs$Q2)
str(nbs$Q2)
nbs$Q2<-as.factor(nbs$Q2)

#데이터 분할
set.seed(1234)
indata<- sample(2, nrow(nbs), replace=T, prob=c(0.7, 0.3)) 
indata
train<- nbs[indata==1,]
test<- nbs[indata==2,]

str(train)
nbs.train <- tree(Q2~ ., data=train)
summary(nbs.train)

plot(nbs.train)
text(nbs.train, cex=1)

#가지치기
data(fgl, package="MASS")
fgl.tr <- tree(type ~ ., fgl) 
plot(print(fgl.tr))
fgl.cv <- cv.tree(fgl.tr,, prune.tree) 
for(i in 2:5)  fgl.cv$dev <- fgl.cv$dev + #deviation이 너무 크거나 너무 작은것을 조정하는 작업
  cv.tree(fgl.tr,, prune.tree)$dev
fgl.cv$dev <- fgl.cv$dev/5 #값이 너무 크므로 5로 나눠줌
plot(fgl.cv)

cv.dtree<- cv.tree(nbs.train,, prune.tree) 
plot(cv.dtree) 

# 의사결정나무 모델링
prune.tree1_1<- prune.tree(nbs.train, best=5) # best=5
plot(prune.tree1_1)
text(prune.tree1_1, pretty=0, cex=1)

prune.tree1_2<- prune.tree(nbs.train, best=6) # best=6
plot(prune.tree1_2)
text(prune.tree1_2, pretty=0, cex=1)

# tree modeling 예측력 평가+분류표 해석
library(e1071)
library(tree)
library(caret)

train
trpred1_1<- predict(prune.tree1_1, train, type = "class") 
confusionMatrix(trpred1_1, train$Q2)
trpred2_1<- predict(prune.tree1_1, test, type = "class")
confusionMatrix(trpred2_1, test$Q2) 

trpred1_2<- predict(prune.tree1_2, train, type = "class")
confusionMatrix(trpred1_2, train$Q2)
trpred2_2<- predict(prune.tree1_2, test, type = "class")
confusionMatrix(trpred2_2, test$Q2) 

#####################################################################################
# party 모델 분석->가지치기 없이 자동으로 분석, 더 좋은 예측모형 뽑아낼 수 있음
library(party)
# 모형 생성
partym1<- ctree(Q2~., train) 
plot(partym1)
# 정확성 평가
partypred1<- predict(partym1, train)
confusionMatrix(trpred1_2, train$Q2)

partypred2<- predict(partym1, test)
confusionMatrix(trpred2_2, test$Q2) #가지치기를 하지 않아도 자동 조정되어 앞에 나온 결과와 동일한 결과를 얻을 수 있음

