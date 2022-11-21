install.packages(c("tidyverse", "corrplot", "GGally", "caret", "rpart", "rpart.plot"), dependencies = TRUE)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris        <- read.csv(url, header = FALSE)
names(iris) <- c("c_sepala", "l_sepala", "c_petala", "l_petala", "especie")
iris$especie <- as.factor(gsub("Iris-", "", iris$especie))
summary(iris)
library(corrplot)
correlacao <- cor(iris[, -5])
corrplot.mixed(correlacao, upper = "ellipse")
summary(iris)
library(tidyverse)
theme_set(theme_bw())
ggplot(iris, aes(x = c_petala, y = l_petala)) +
  geom_point()
ggplot(iris, aes(x = c_petala, y = l_petala, colour = especie)) +
  geom_point()
library(GGally)
ggpairs(iris[, -5], aes(colour = iris$especie))
library(rpart)
library(rpart.plot)
modelo <- rpart(especie ~ ., method = "class", data = iris)
prp(modelo, extra = 1)
library(caret)
trainIndex  <- createDataPartition(iris$especie, p = 0.75, list = FALSE)
iris_treino <- iris[ trainIndex, ]
iris_teste  <- iris[-trainIndex, ]
fitControl <- trainControl(method = "cv",
                           number = 5)
ajuste_iris <- train(especie ~ ., 
                     data = iris_treino, 
                     method = "rf", 
                     importance = TRUE,
                     trControl = fitControl)
ajuste_iris
predicao <- predict(ajuste_iris, iris_teste)
confusionMatrix(predicao, iris_teste$especie)
ggplot(varImp(ajuste_iris))
