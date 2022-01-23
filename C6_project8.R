# C6 Project8 OJS =============================================================

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

#기존 데이터 셋과의 비율 비교
#table(abalone$Sex)
#table(abalone_doBy_train$Sex)
#str(abalone_doBy_train)


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

#(3)모형의 정확성 검정
table(abalone_doBy_train$Sex, predict(abalone_tree1),dnn = c('Actual','Predicted'))
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
cm=confusionMatrix(abalone_caret_test$Sex,pred)
cm

#(3)모델 평가
table(abalone_caret_train$Sex, predict(abalone_tree1),dnn = c('Actual','Predicted')) #정확도 약50%
treepred <- predict(treemod, abalone_caret_test, type='response')
confusionMatrix(treepred, abalone_caret_test$Sex)

#정확도 약54%

# 결론: 전복의 성별은 전복의 각종 수치로는 구별이 불가능하므로 대립가설을 기각한다.




# 양식 ####
## xxxxx 데이터 셋 ============================================================

  #귀무가설: 
  #대립가설:
  
  # 설명
  # 종속변수가 oo형이고 설명변수가 oo형의 자료이므로,
  # oooo기법의 사용이 가능하다.
  # oooo기법에서는 CART, C5.0, QUEST방식의 기법이 활용 가능하다.


### 1.데이터 전처리(예시) ======================================================

  # 1)데이터 셋 불러오기
  data1 <- read.csv('data.csv',header = F)
  names(data1) <- c('1','2,','3','4')
  
  # 2)데이터 추출/자료형 변환
  a <- subset(data1, Sex != 'I')
  head(abalone)
  abalone$Sex <- as.factor(abalone$Sex)
  sapply(abalone,class)

  # 3) train/test sets 생성
  #(1)doby train/test set
  set.seed(1000)
  abalone_doBy_train <- sampleBy(~Sex, frac=0.7, data=abalone) #전복의 성별을 기준으로 동일한 비율로 나눔
  abalone_doBy_train
  abalone_doBy_test <- sampleBy(~Sex, frac=0.3, data=abalone)
  abalone_doBy_test

  
  #(2)caret train/test set
  set.seed(2000)
  intrain <- createDataPartition(y=abalone$Sex, p=0.7, list=FALSE) 
  abalone_train2 <-abalone[intrain, ]
  abalone_test2 <-abalone[-intrain, ]
  table(abalone_train2$Sex)
  
  
  

### 2.분석 ====================================================================
# 1)의사결정나무 ####
# 기본 패키지
  #(1)학습모델 생성(party 패키지)

    
  #(2)모델 평가



# caret 패키지
  #(1)모델 생성 및 시각화

  
  #(2)모델 평가
  
  


# 2)다중회귀분석 ####
# 기본 패키지
  #(1)학습모델 생성
  
  
  #(2)모델 평가
  
  
  
# caret 패키지
  #(1)모델 생성 및 시각화
  
  
  #(2)모델 평가




###############################
## xxxxx 데이터 셋 ============================================================
  
  #귀무가설: 
  #대립가설:
  
  # 설명
  # 종속변수가 oo형이고 설명변수가 oo형의 자료이므로,
  # oooo기법의 사용이 가능하다.
  # oooo기법에서는 CART, C5.0, QUEST방식의 기법이 활용 가능하다.
  
  
### 1.데이터 전처리(예시) ======================================================
  # 1)데이터 셋 불러오기
  data2 <- read.csv('data.csv',header = F)
  names(data2) <- c('1','2,','3','4')
  
  # 2)데이터 추출/자료형 변환
  a <- subset(data2, Sex != 'I')
  head(spam)
  spam$spam <- as.factor(abalone$Sex)
  sapply(spam,class)
  
  # 3) train/test sets 생성
  #(1)doby train/test set
  set.seed(1000)
  spam_doBy_train <- sampleBy(~Sex, frac=0.7, data=spam) #전복의 성별을 기준으로 동일한 비율로 나눔
  spam_doBy_train
  spam_doBy_test <- sampleBy(~Sex, frac=0.3, data=spam)
  spam_doBy_test
  
  
  #(2)caret train/test set
  set.seed(2000)
  intrain <- createDataPartition(y=spam$spam, p=0.7, list=FALSE) 
  spam_train2 <-spam[intrain, ]
  spam_test2 <-spam[-intrain, ]
  table(spam_train2$spam)
  
  
  
  
### 2.분석 ====================================================================
# 1)랜덤포레스트 ####
# 기본 패키지
  #(1)학습모델 생성(party 패키지)
  
  
  #(2)모델 평가
  
  
  
# caret 패키지
  #(1)모델 생성 및 시각화
  
  
  #(2)모델 평가
  
  
  
  
# 2)XGBoost ####
# 기본 패키지
  #(1)학습모델 생성
  
  
  #(2)모델 평가
  
  
  
# caret 패키지
  #(1)모델 생성 및 시각화
  
  
  #(2)모델 평가