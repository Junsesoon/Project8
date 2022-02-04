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
# install.packages("kernlab") #caret 패키지 SVM
library(kernlab)
# install.packages("class") #KNN
library(class)
# install.packages("nnet") #인공신경망
library(nnet)


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

  
  

# titanic 데이터 셋 ===========================================================

# 설명)
# pclass :    1, 2, 3등석 정보를 각각 1, 2, 3으로 저장
# survived :  생존 여부. survived(생존=1), dead(사망=0)
# name :      이름(제외)
# sex :       성별. female(여성), male(남성)
# age :       나이
# sibsp :     함께 탑승한 형제 또는 배우자의 수
# parch :     함께 탑승한 부모 또는 자녀의 수
# ticket :    티켓 번호(제외)
# fare :      티켓 요금
# cabin :     선실 번호(제외)
# embarked :  탑승한 곳. C(Cherbourg), Q(Queenstown), S(Southampton)
# boat     :  (제외)Factor w/ 28 levels "","1","10","11",..: 13 4 1 1 1 14 3 1 28 1 ...
# body     :  (제외)int  NA NA NA 135 NA NA NA NA NA 22 ...
# home.dest:  (제외)


## 1.데이터 전처리 ============================================================

# 1)데이터 셋 불러오기
data2 <- read.csv('titanic.csv',header = T)
titanic <- data2[,c(2,4,5,11)]
head(titanic)
colnames(titanic)

sapply(titanic,class)  #데이터 자료형 확인
shapiro.test(na.omit(titanic$age))  # 정규성 검정
# 0.05보다 작으므로 정규분포가 아니다.

# 2) train/test sets 생성
  #(1)doBy train/test sets 생성
  #샘플링
  set.seed(2124)
  titanic_doBy_train <- sampleBy(~survived, frac=0.7, data=titanic) #생존여부를 기준으로 동일한 비율로 나눔
  titanic_doBy_test <- sampleBy(~survived, frac=0.3, data=titanic)
  #자료형 변환
  titanic_doBy_train$survived <- as.factor(titanic_doBy_train$survived)
  titanic_doBy_test$survived <- as.factor(titanic_doBy_test$survived)
  #NA값 제거
  titanic_doBy_train <- subset(titanic_doBy_train,!is.na(titanic_doBy_train$age))
  titanic_doBy_test <- subset(titanic_doBy_test,!is.na(titanic_doBy_test$age))
  
  #(2)caret train/test sets 생성
  #샘플링
  set.seed(2223)
  intrain <- createDataPartition(y=titanic$survived, p=0.7, list=FALSE)
  titanic_caret_train <-titanic[intrain, ]
  titanic_caret_test <-titanic[-intrain, ]
  #자료형 변환
  titanic_caret_train$survived <- as.factor(titanic_caret_train$survived)
  titanic_caret_test$survived <- as.factor(titanic_caret_test$survived)
  #NA값 제거
  titanic_caret_train <- na.omit(titanic_caret_train)
  titanic_caret_test <- na.omit(titanic_caret_test)




## 2.분석 =====================================================================

  ### 상황1 ####
  # 생존자들의 평균나이가 28.92세 일 때, 사망한 사람들의 평균나이와 차이가 있는지 
  # 검정하기 위해 사망한 사람들을 랜덤으로 선정하여 검정을 시행한다.
  
  #귀무가설: 생존한 사람과 생존하지 못한 사람은 나이의 평균에 차이가 없다.
  #대립가설: 생존한 사람과 생존하지 못한 사람은 나이의 평균에 차이가 있다.
  
  # 종속변수가 연속형이고 설명변수가 범주형의 자료이므로,
  # 통계기반의 T-test 기법이 활용 가능하다.

#### 1)two sample T-test ####
# stats 패키지
  #(1)대응하는 두 집단 생성
  dead <- subset(titanic_doBy_train,titanic_doBy_train$survived == 0)
  
  #(2)양측 검정 - titanic객체의 기존 모집단의 평균 28.92세 비교
  t.test(dead$age, mu = 28.92)
  qqnorm(dead$age)
  qqline(dead$age, lty = 1, col = "blue")
  t.test(dead$age, mu = 28.92, alter = "two.side", conf.level = 0.95)
  
  #p-값이 유의수준 0.05보다 낮기 때문에 평균 수명에 차이가 있다고 볼 수 있다.
  
  #(3)단측 검정 - 방향성을 가짐
  t.test(dead$age, mu = 28.92, alter= "greater", conf.level = 0.95)
  
  #(4)귀무가설의 임계값 계산
  qt(0.05,427,lower.tail = F)
  
  #귀무가설을 기각할 수 있는 임계값 = 1.64843
  #검정통계량 t=2.0755, 유의확률P=0.01927



# caret 패키지
  #(1)대응하는 두 집단 생성
  dead2 <- subset(titanic_caret_train,titanic_caret_train$survived == 0)
  
  #(2)양측 검정 - titanic객체의 기존 모집단의 평균 28.92세 비교
  t.test(dead2$age, mu = 28.92)
  qqnorm(dead2$age)
  qqline(dead2$age, lty = 1, col = "blue")
  t.test(dead2$age, mu = 28.92, alter = "two.side", conf.level = 0.95)
  
  #(3)단측 검정 - 방향성을 가짐
  t.test(dead2$age, mu = 28.92, alter= "greater", conf.level = 0.95)
  
  #(4)귀무가설의 임계값 계산
  qt(0.05,434,lower.tail = F)
  
  #귀무가설을 기각할 수 있는 임계값 = 1.64837
  #검정통계량 t=2.4023, 유의확률P=0.008355
  
  # 결론: 유의수준 0.05에서 귀무가설이 기각되므로,
  #       성별은 평균생존률에 차이가 있다.



