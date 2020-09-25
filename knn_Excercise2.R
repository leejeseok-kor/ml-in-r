##Second Language Immersion School in Canada
#names, provinces, coordinates

library(caret)
library(class)

#1. data준비
schools <- read.csv("https://ibm.box.com/shared/static/uummw8ijp41gn3nfkuipi78xnalkss4c.csv", sep = ",")
head(schools)
str(schools)

#2. data 정제
#2.1 사용할 variable 선택
schools.sub <- subset(schools, select = c(name, province.name..english,latitude, longitude))

#2.2 rename
colnames(schools.sub) <- c('name','province','latitude','longitude')
head(schools.sub)
apply(schools.sub, 2, function(x) sum(is.na(x)))

#3 knn modeling
#3.1 data split
schools.sub <- na.omit(schools.sub)
head(schools.sub)
str(schools.sub)
index <- sample(nrow(schools.sub))
data <- schools.sub
data <- data[index,]
bound <- floor(nrow(data) * 0.7)
train_df <- data[1:bound,]
test_df <- data[(bound + 1):nrow(data),]
X_train <- train_df[,3:4]
y_train <- train_df[,2]
X_test <- test_df[,3:4]
y_test <- test_df[,2]


model_knn <- knn(X_train, X_test, cl = y_train, k = 3)
tb <- table(y_test, model_knn)
cat('incorrect proportion : ', (sum(tb) - sum(diag(tb)))/sum(tb))
#상당히 높은 정확도를 보임
