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




# iris 데이터 셋 ==============================================================

# 설명)
# Sepal.Length  continuous  꽃받침의 길이
# Sepal.Width   continuous  꽃받침의 폭
# Petal.Length  continuous  꽃잎의 길이
# Petal.width   continuous  꽃잎의 폭
# Species       factor      꽃의 종류


## 1.데이터 전처리 ============================================================
# 1)데이터 셋 불러오기
data3 <- iris

# 2)데이터 추출/자료형 변환
# 라벨링
iris_label <- ifelse(data3$Species == 'setosa', 0,
                     ifelse(data3$Species == 'versicolor', 1,2))
table(iris_label)
data3$label <- iris_label


sapply(data3,class)

# 3) train/test sets 생성
  #(1)doBy train/test sets 생성
  set.seed(1111)
  iris_doBy_train <- sampleBy(~Species, frac=0.7, data=data3) #전복의 성별을 기준으로 동일한 비율로 나눔
  iris_doBy_test <- sampleBy(~Species, frac=0.3, data=data3)
  
  iris_doBy_train_mat <- as.matrix(iris_doBy_train[-c(5:6)])
  iris_doBy_train_lab <- iris_doBy_train$label
  dim(iris_doBy_train_mat)
  length(iris_doBy_train_lab)
  
  #(2)caret train/test sets 생성
  set.seed(1000)
  iris_intrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE) 
  iris_caret_train <-iris[iris_intrain, ]
  iris_caret_test <-iris[-iris_intrain, ]
  table(iris_caret_train$Species)
  table(iris_caret_test$Species)




## 2.분석 =====================================================================
#### 2)앙상블(배깅) ####
  # adabag 패키지
  #(1)Bagging model 생성
  iris.bagging <- bagging(Species~., data=iris_doBy_train[1:5], mfinal=10)
  iris.bagging$importance
  
  #(2)도식화
  plot(iris.bagging$trees[[10]])
  text(iris.bagging$trees[[10]])
  
  #(3)예측값
  baggingpred <- predict(iris.bagging, newdata=iris)
  
  #(4)정오분류표
  baggingtb <- table(baggingpred$class, iris[,5])
  sum(baggingtb[row(baggingtb) == col(baggingtb)])/sum(baggingtb)  # 정분류율
  1-sum(baggingtb[row(baggingtb) == col(baggingtb)])/sum(baggingtb)  # 오분류율
  
  # Caret Package
  #(1)Caret Package 배깅 모델 생성
  ctrl <- trainControl(method = 'cv',
                       number = 10)
  gyu <- train(Species ~ . ,
               data = iris_caret_train,
               method = 'treebag',
               trControl = ctrl)
  gyu
  
  #(2)예측 분류 결과 생성
  gyu_pred <- predict(gyu, newdata = iris_caret_test)
  
  #(3)적용 분류 결과 도출
  table(gyu_pred, iris_caret_test$Species)
  
  #(4)모델 성능 평가 지표(정확도 확인)
  confusionMatrix(gyu_pred, iris_caret_test$Species)
  
  # 정확도 100%
  
  


#### 3)앙상블(부스팅) ####
# adabag 패키지
  #(1)boosting model 생성
  boo.adabag <- boosting(Species~., data = iris_doBy_train,
                         boos = TRUE,
                         mfinal = 10)
  boo.adabag$importance
  
  #(2)도식화
  plot(boo.adabag$trees[[10]])
  text(boo.adabag$trees[[10]])
  
  #(3)예측값
  pred <- predict(boo.adabag, newdata = iris_doBy_test)
  
  #(4)정오분류표
  tb <- table(pred$class, iris_doBy_test[,5])
  sum(tb[row(tb) == col(tb)])/sum(tb)  # 정분류율
  1-sum(tb[row(tb) == col(tb)])/sum(tb)  # 오분류율

# Caret Package
  #(1)Caret Package 부스팅 학습 모델 설정
  ctrl <- trainControl(method = 'cv', number = 3) ## method : 샘플링을 하는 방법을 결정
  m1 <- train(Species ~ . , data = iris_caret_train,
              method = 'AdaBoost.M1',
              trControl = ctrl)
  m1
  
  #(2)예측 분류 결과 생성
  m1_pred <- predict(m1, newdata = iris_caret_test)
  
  #(3)적용 분류 결과 도출
  table(m1_pred, iris_caret_test$Species)
  
  #(4)모델 성능 평가 지표(정확도 확인)
  confusionMatrix(m1_pred, iris_caret_test$Species)
  # 정확도 100%



