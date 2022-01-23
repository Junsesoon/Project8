# C6 Project8 OJS =============================================================

  # 환경설정
rm(list=ls())
setwd('c:/rwork/')
options(max.print = 20)

  # 라이브러리
install.packages()
library()
install.packages()
library()


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