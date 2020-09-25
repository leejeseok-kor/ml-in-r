##1. 패키지 설치, 로드
library(class)
library(kknn)
library(caret)

##2. 데이터 준비
balance_scale <- read.csv("https://ibm.box.com/shared/static/684jzm7e6fbbssg87yc2v4dy53dgkdew.txt", sep = ",")
str(balance_scale)
head(balance_scale)

##3. data rename
names(balance_scale) <- c('Class_Name','Right_Distance','Right_Weight','Left_Distance','Left_Weight')
head(balance_scale)

##4. data를 split
#훈련 -> 70% 테스트 -> 30%
data <- balance_scale
data$Right_Product <- data$Right_Distance * data$Right_Weight
data$Left_Product <- data$Left_Distance * data$Left_Weight
data$Difference <- data$Right_Product - data$Left_Product
index <- sample(nrow(data))
data_sp <- data[index,]
bound <- floor(nrow(data) * 0.7)
train_df <- data_sp[1:bound,]
test_df <- data_sp[(bound+1):nrow(data),]
cat('train data 개수, test data 개수', nrow(train_df), nrow(test_df))
colnames(train_df)
colnames(test_df)

X_train <- train_df[,c('Right_Product','Left_Product','Difference')]
y_train <- train_df[,'Class_Name']
X_test <- test_df[,c('Right_Product','Left_Product','Difference')]
y_test <- test_df[,'Class_Name']

#5. modeling using knn
model_knn <- knn(train = X_train,test = X_test, cl = y_train, k = 3)
##model_knn, y_test 정오표
table(model_knn, y_test)
##정분류 비율
x <- table(model_knn, y_test)
cat('정분류 비율 : ',sum(diag(x))/sum(x))

#6. 모델 성능 향상
##적절한 k 찾기

intrain <- createDataPartition(data$Class_Name, p = 0.7, list = FALSE)
ndf_train <- data[intrain, ]
ndf_test <- data[-intrain, ]

ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats = 2)

nn_grid <- expand.grid(k = c(3,5,7))
nn_grid

head(ndf_train)
best_knn <- train(Class_Name ~., data = ndf_train,
                  method = 'knn',
                  trControl = ctrl,
                  preProcess = c('center','scale'),
                  tuneGrid = nn_grid)
best_knn
