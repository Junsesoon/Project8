# C6 Project8 =================================================================

  # 환경설정
rm(list=ls())
setwd('c:/rwork/')
options(max.print = 20)

  # 라이브러리
#install.packages("tree")
library(tree)
#install.packages("doBy")
library(doBy)
#install.packages("party")
library(party) #ls("package:party")
#install.packages("caret")
library(caret) #ls("package:caret")
#install.packages("dplyr")
library(dplyr)
#install.packages("e1071")
library(e1071)
install.packages()
library()
install.packages()
library()


## abalone 데이터 셋 ===========================================================

#귀무가설: 각종 전복의 수치로 성별을 오차범위 5%내로 구분할 수 없다.
#대립가설: 각종 전복의 수치로 성별을 오차범위 5%내로 구분할 수 있다.

# 종속변수가 범주형이고 설명변수가 연속형의 자료이므로,
# 의사결정나무에서는 CART, C5.0, QUEST기법이 활용 가능하다.

# 설명)
# Sex		          nominal			M, F, and I (infant)
# Length		      continuous	mm	Longest shell measurement
# Diameter	      continuous	mm	perpendicular to length
# Height		      continuous	mm	with meat in shell
# Whole weight	  continuous	grams	whole abalone
# Shucked weight	continuous	grams	weight of meat
# Viscera weight	continuous	grams	gut weight (after bleeding)
# Shell weight	  continuous	grams	after being dried
# Rings		        integer			+1.5 gives the age in years


### 1.데이터 전처리 ============================================================

# 1)데이터 셋 불러오기
data1 <- read.csv('abalone.csv',header = F)
names(data1) <- c('Sex','Length','Diameter','Height','WholeWeight',
                  'ShuckedWeight','VisceraWeight','ShellWeight','Rings')

# 2)데이터 추출/자료형 변환
abalone <- subset(data1, Sex != 'I') #유아기의 전복은 제외
abalone$Sex <- as.factor(abalone$Sex)
sapply(abalone,class)

# 3) train/test sets 생성
  #(1)doBy train/test sets 생성
  set.seed(1111)
  abalone_doBy_train <- sampleBy(~Sex, frac=0.7, data=abalone) #전복의 성별을 기준으로 동일한 비율로 나눔
  abalone_doBy_test <- sampleBy(~Sex, frac=0.3, data=abalone)
  abalone_doBy_train
  abalone_doBy_test
  
  #(2)caret train/test sets 생성
  set.seed(1000)
  intrain <- createDataPartition(y=abalone$Sex, p=0.7, list=FALSE) 
  abalone_caret_train <-abalone[intrain, ]
  abalone_caret_test <-abalone[-intrain, ]
  table(abalone_caret_train$Sex)
  table(abalone_caret_test$Sex)




### 2.분석 ====================================================================  
# 1)의사결정나무 ####
# party 패키지
  #(1)학습모델 생성
  treeOption1 <- ctree_control(maxdepth = 10)
  abalone_tree1 <- ctree(Sex~.,
                         data = abalone_doBy_train,
                         controls = treeOption1)
  plot(abalone_tree1, compress=TRUE)
  
  #(2)예측치 생성
  table(abalone_doBy_train$Sex, predict(abalone_tree1),dnn = c('Actual','Predicted'))
  predict(abalone_tree1,data=abalone_doBy_train)
  
  #(3)모형의 정확성 검정
  confusionMatrix(data=abalone_doBy_test$Sex,predict(abalone_tree1,abalone_doBy_test))
  
  #정확도 약54%
  
  
  
  # caret 패키지
  #(1)모델 생성 및 시각화
  treemod <- ctree(formula=Sex~.,
                   data=abalone_caret_train)
  plot(treemod)
  
  #(2)예측 및 모델 평가
  pred = predict(data=abalone_caret_test,treemod)
  table(pred)
  
  #(3)모델 평가
  confusionMatrix(treepred, abalone_caret_test$Sex)
  
  #정확도 약54%
  
  # 결론: 전복의 성별은 전복의 각종 수치로는 구별이 불가능하므로 대립가설을 기각한다.



# 전복의 각종 수치가 Rings의 크기에 미치는 영향을 파악한다.

#### 2) 다중회귀분석 ####
#stats 패키지
  #(1)학습모델 생성
  abalone_lm_model <- lm(Rings ~., data=abalone_doBy_train)
  summary(abalone_lm_model) #p-값 확인: 0.05이하이므로 독립변수들 간의 모형은 유의하다.
  vif(abalone_lm_model)
  
  #다중공선성 문제가 가장 심각한 변수를 제외한다.
  abalone_lm_model2 <- lm(Rings ~ ., data = abalone_doBy_train[,-5])
  summary(abalone_lm_model2)
  
  #결과 해석
  # 성별의 p-값이 다소 높지만 큰 영향을 주는 변수는 아니기 때문에 무시하기로 한다.
  # 그 외에 Rings에 가장 큰영향을 미치는 변수는 Shellweight, Diameter, shuckedWeight
  # 순서로 영향을 미쳤다.
  
#caret 패키지
  #(1) 학습모델 생성
  ctrl <- trainControl(method="cv", 10)
  abalone_lm_caret <- train(Rings ~ .,
                            data = abalone_caret_train[,-5],
                            na.action = na.omit,
                            method = 'lm',
                            trControl = ctrl)
  summary(abalone_lm_caret)
  #결과 해석
  # 그 외에 Rings에 가장 큰영향을 미치는 변수는 Shellweight, Height, shuckedWeight
  # 순으로 파악됐다.
  
  
  
  