# C6 Project8 OJS =============================================================

  # 환경설정
rm(list=ls())
setwd('C:/Users/junseo/OneDrive/code/Project/project8/datasets')
options(max.print = 20)

  # 라이브러리
# install.packages('pander')

# install.packages('doBy')
library(doBy)
# install.packages("caret")
library(caret)
# install.packages("e1071") #SVM훈련
library(e1071)
# install.packages("kernlab") #caret 패키지 SVM
library(kernlab)
# install.packages("class") #KNN
library(class)
# install.packages("nnet") #인공신경망
library(nnet)

## spam 데이터 셋 ============================================================

  #귀무가설: 
  #대립가설:
  
  # 설명)
  # 불러온 spam 데이터는 4601개의 이메일에서 등장하는 단어의 종류와 관련된 58개의 변수로 구성되어있다. 
  # 58개의 변수 중 처음 48개 변수(A.1~A.48)은 총 단어수 대비 해당 단어의 출현비율을 나타내며, 
  # 6개 변수(A.49~A.54)는 총 문자수 대비 특정 문자의 출현비율을 나타내며, 
  # 3개 변수(A.55~A.57)은 연속되는 대문자 철차의 평균길이, 최대길이, 대문자의 총수를 나타낸다. 
  # 마지막 변수(spam)스팸 메일의 여부를 타나냅니다. 
  # 즉 spam 변수가 종속변수가 되며 나머지 A.1~57 변수가 예측변수가 된다.. 
  # 결측값은 없으며 전체에서 스팸메일은 1813개다.

  # 종속변수가 범주형이고 설명변수가 수치형의 자료이므로,
  # 분류기법의 사용이 가능하다.
  # 분류기법에서는 나이브베이즈, SVM, 로지스틱회귀분석, KNN, 인공신경망기법이 활용 가능하다.


### 1.데이터 전처리(예시) ======================================================

  # 1)데이터 셋 불러오기
  spam <- read.csv('spam.csv', header = T)
  str(spam)
  
  # 2) train/test sets 생성
  #(1)doby train/test set
  set.seed(9999)
  spam_train_doBy <- sampleBy(~spam, frac = 0.7, data = spam)

  # 70%에 포함된 train데이터의 행 번호를 추출하기 위한 사용자 함수
  enrow <- rownames(spam_train_doBy)
  nurow <- as.numeric(gsub('\\D','',enrow))
  spam_test_doBy <- spam[-nurow,]
  
  # 데이터 확인
  dim(spam_train_doBy)
  dim(spam_test_doBy)
  table(spam_train_doBy$spam)
  table(spam_test_doBy$spam)
  
  
  #(2)caret train/test set
  set.seed(8888)
  train_idx <- createDataPartition(spam$spam, p=0.7, list=F)
  spam_train_caret <- spam[train_idx,]
  spam_train_label_caret <- spam$spam[train_idx]
  spam_test_caret <- spam[-train_idx,]
  
  # 데이터 확인
  dim(spam_train_caret)
  dim(spam_test_caret)
  table(spam_train_caret$spam)
  table(spam_test_caret$spam)

  
  
  

### 2.분석 ====================================================================
# 1)나이브 베이즈 ####
# e1071 패키지
  #(1)학습모델 생성
  spam_nb_doby <- naiveBayes(spam ~ .,
                             data = spam_train_doBy,
                             laplace = 1)
  spam_nb_doby

    
  #(2)예측 분류 결과 생성
  spam_nb_pred_doby <- predict(spam_nb_doby,
                               newdata = spam_test_doBy,
                               type = 'class')
  
  #(3)모델 평가
  confusionMatrix(spam_nb_pred_doby, as.factor(spam_test_doBy$spam))

  ## e1071 나이브베이즈는 약 72%의 정확도로 분류 하였다.

  
# caret 패키지
  #(1)모델 생성
  ctrl <- trainControl(method="cv", 10)
  spam_nb_caret <- train(spam ~ ., data = spam_train_caret, 
                         method = 'naive_bayes',
                         trControl = ctrl)
  spam_nb_caret

  
  #(2)예측 분류 결과 생성
  spam_nb_pred_caret <- predict(spam_nb_caret, newdata = spam_test_caret)

  
  #(3)모델 평가
  confusionMatrix(spam_nb_pred_caret, as.factor(spam_test_caret$spam))
  
  ## caret의 나이브베이즈는 70%의 정확도로 분류하였다.
  

  
  
# 2)SVM(서포트 벡터 머신) ####
# 기본 패키지
  #(1)파라미터 최적값 찾기
  # tune.svm(factor(spam) ~ ., data = spam, gamma = 2^(-1:1), cost = 2^(1:4))  # 시간 오래 걸림, 결과 밑에 작성.
  #  Parameter tuning of ‘svm’:
  #  - sampling method: 10-fold cross validation 
  #  - best parameters:
  #   gamma cost
  #    0.5    4
  #  - best performance: 0.16714 
  
  
  #(2)학습모델 생성
  spam_svm_doBy <- svm(factor(spam) ~ .,
                       data = spam_train_doBy,
                       gamma = 0.5, cost = 4)
  spam_svm_doBy
  
  #(3)예측 분류 결과 생성
  spam_svm_pred_doBy <- predict(spam_svm_doBy,
                                newdata = spam_test_doBy)
  spam_svm_pred_doBy
  
  
  #(3)모델 평가
  confusionMatrix(spam_svm_pred_doBy, factor(spam_test_doBy$spam))
  
  # e1071패키지의 SVM은 정확도(0.8101). 즉, 약 81% 정확도로 분류하였다.
  
  
# caret 패키지
  #(1)모델 생성 및 시각화
  ctrl <- trainControl(method="cv", 10)
  spam_svm_caret <- train(spam ~ .,
                          data = spam_train_caret,
                          method = 'svmRadial',
                          trControl = ctrl,
                          tuneGrid = expand.grid(sigma= 0.5 , C = 4)
                          )
  
  
  #(2)예측 분류 결과 생성
  spam_svm_pred_caret<- predict(spam_svm_caret,
                                newdata = spam_test_caret)
  
  #(3)모델 평가
  confusionMatrix(spam_svm_pred_caret, factor(spam_test_caret$spam))
  
  # caret패키지 SVM은 정확도(0.8289). 즉, 83% 정확도로 분류하였다.




# 3)로지스틱 회귀분석 ####
# 기본 패키지
  #(1)학습모델 생성
  spam_glm_doBy <- glm(factor(spam) ~ .,
                       data = spam_train_doBy,
                       family = 'binomial')
  
  #(2)예측 분류 결과 생성
  spam_glm_pred_doBy <- predict(spam_glm_doBy,
                                newdata = spam_test_doBy,
                                type = 'response')
  
  #(3)모델 평가
  spam_glm_pred_doBy2 <- ifelse(spam_glm_pred_doBy > 0.5, 'spam', 'email')  # 컷오프 0.5로 설정하여 사후확률이 0.5초과이면 spam, 05이하이면 email로 예측한다.
  confusionMatrix(factor(spam_glm_pred_doBy2), factor(spam_test_doBy$spam))
  
  # glm 로지스틱 회귀분석은 약 91% 정확도로 분류하였다.
  
  
  # caret 패키지
  #(1)모델 생성 및 시각화
  ctrl <- trainControl(method="cv", 10)
  spam_glm_caret <- train(factor(spam) ~ .,
                          data = spam_train_caret,
                          method = 'glm'
                          , trControl = ctrl)
  
  #(2)예측 분류 결과 생성
  spam_glm_pred_caret <- predict(spam_glm_caret,
                                 newdata = spam_test_caret,
                                 type = 'raw')
  
  #(3)모델 평가
  confusionMatrix(factor(spam_glm_pred_caret), factor(spam_test_caret$spam))
  
  # caret패키지의 로지스틱 회귀분석은 약 91% 정확도로 분류하였다.
  
  


# 4)최근접 이웃 모델(KNN) ####
# 기본 패키지
  #(1) 정규화
  normalize <- function(x) {
    return ((x-min(x))/(max(x)-min(x)))
  }
  
  st_spam <- as.data.frame(lapply(spam[56:57], normalize))
  sub <- spam[-c(56,57,58)]
  nor_spam <- cbind(sub, st_spam)
  
  #doBy 표준화 학습,검정용 데이터 생성
  set.seed(7777)
  spam_train_doBy <- sampleBy(~spam, frac = 0.7, data = nor_spam)
  spam_train_label_doBy <- spam$spam[nurow] 
  spam_test_doBy <- nor_spam[-nurow, -58]
  spam_test_label_doBy <- spam$spam[-nurow]
  
  
  #(2)학습모델 생성
  spam_knn_doBy <- knn(train = spam_train_doBy, 
                       test = spam_test_doBy, 
                       cl = spam_train_label_doBy, 
                       k = 57)
  spam_knn_doBy
 
  #(3)모델 평가
  confusionMatrix(factor(spam_test_label_doBy), factor(spam_knn_doBy))
  
  # class패키지의 knn모델은 약 88%의 정확도로 분류함.
  
  # caret 패키지
  #(1) 정규화
  set.seed(6666)
  train_idx <- createDataPartition(spam$spam, p=0.7, list=F)
  spam_train_caret <- nor_spam[train_idx,]
  spam1 <- spam$spam[train_idx]
  spam_train_caret <- cbind(spam_train_caret,spam1)
  
  spam_test_caret <- nor_spam[-train_idx,]
  spam2 <- spam$spam[-train_idx]
  spam_test_caret <- cbind(spam_test_caret,spam2)
  
  #(2)모델 생성
  tune <- trainControl(method = 'cv', number = 10)
  spam_knn_caret <- train(spam1 ~ ., data = spam_train_caret,
                          method = 'knn',
                          tuneGrid = expand.grid(k=57),
                          trControl = tune)
  #(3)예측 분류 결과 생성
  spam_knn_pred_caret<- predict(spam_knn_caret, 
                                newdata = spam_test_caret)
  
  #(4)모델 평가
  confusionMatrix(spam_knn_pred_caret, as.factor(spam_test_caret$spam2))
  
  # caret패키지의 knn학습모델을 약 88% 정확도로 분류함.
  


  
# 5)인공신경망 ####
  # 기본 패키지
  #(1)훈련, 검증용 데이터 생성
  sub <- spam[-c(56,57)]
  nor_spam <- cbind(sub, st_spam)
  
  set.seed(5555)
  spam_train_doBy <- sampleBy(~spam, frac = 0.7, data = nor_spam)
  spam_test_doBy <- nor_spam[-nurow,]
  
  #(1)학습모델 생성
  spam_nnet_doBy <- nnet(factor(spam) ~ ., 
                         data = spam_train_doBy, 
                         size = 4, 
                         decay = 5e-04)  # 가장 정확하다는 옵션 선택. 
  spam_nnet_doBy
  
  #(2)예측 분류 결과 생성
  spam_nnet_pred_doBy <- predict(spam_nnet_doBy, 
                                 newdata = spam_test_doBy, 
                                 type = 'class')
  
  #(3)모델 평가
  confusionMatrix(factor(spam_nnet_pred_doBy), factor(spam_test_doBy$spam))
  
  # nnet패키지 인공신경망은 95% 정확도로 분류함.
  
  
  # caret 패키지
  #(1)모델 생성
  spma_nnet_caret <- train(spam1 ~ ., 
                           data = spam_train_caret, 
                           method = 'nnet', 
                           trace = F,
                           tuneGrid = expand.grid(.size= 4, .decay = 5e-04))
  #(2)예측 분류 결과 생성
  spam_nnet_pred_caret<- predict(spma_nnet_caret, 
                                 newdata = spam_test_caret)
  
  #(3)모델 평가
  confusionMatrix(spam_nnet_pred_caret, as.factor(spam_test_caret$spam2))
  
  # caret패키지 인공신경망은 95% 정확도로 분류함.
  
  
  
  


## 양식 #############################
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