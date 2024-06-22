#Ucitavanje data seta
data <- read.csv('breast_cancer_data.csv', stringsAsFactors = FALSE)
summary(data)
str(data)

#Funckija za normalizaciju podataka
normalize_max <- function(col) {
  return(col / max(abs(col)))
}

#Normalizacija podataka
apply(data[, c(1:30)], MARGIN = 2, FUN = normalize_max)

#Mesanje redova u data setu
set.seed(123)
data <- data[sample(1:nrow(data)), ]

#Kreiranje trening i test seta
library(caret)
set.seed(123)
train.indicies <-
  createDataPartition(data$X1, p = 0.65, list = FALSE)
train.data <- data[train.indicies, ]
test.data <- data[-train.indicies,]

#Treniranje neuronske mreze
library(neuralnet)

#Prva mreza
#Learning rate 0.2, broj neurona 10
nn1 <- neuralnet(
  X1 ~ .,
  data = train.data,
  hidden = 10,
  learningrate = 0.2,
  linear.output = FALSE
)

#Pravljenje predikcije
nn1.predictions <- predict(nn1, newdata = test.data)
#Racunanje srednje kvadratne greske
nn1.mse <- mean((test.data$X1 - nn1.predictions) ^ 2)



#Druga mreza
#Learning rate 0.4, broj neurona 20
nn2 <- neuralnet(
  X1 ~ .,
  data = train.data,
  hidden = 20,
  learningrate = 0.4,
  linear.output = FALSE
)

#Pravljenje predikcije
nn2.predictions <- predict(nn2, newdata = test.data)
#Racunanje srednje kvadratne greske
nn2.mse <- mean((test.data$X1 - nn2.predictions) ^ 2)


#Treca mreza
#Learning rate 0.6, broj neurona 30
nn3 <- neuralnet(
  X1 ~ .,
  data = train.data,
  hidden = 30,
  learningrate = 0.6,
  linear.output = FALSE
)

#Pravljenje predikcije
nn3.predictions <- predict(nn3, newdata = test.data)
#Racunanje srednje kvadratne greske
nn3.mse <- mean((test.data$X1 - nn3.predictions) ^ 2)


mse.compare = rbind(c('NN1', 'NN2', 'NN3'), c(nn1.mse, nn2.mse, nn3.mse))
mse.compare

#Mreza sa najmanjom greskom je NN2
#Cuvanje mreze u fajl
saveRDS(nn1, file = 'neural_net_model.rds')
