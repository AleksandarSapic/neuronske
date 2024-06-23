#Ucitavanje data seta
data <- read.csv('breast_cancer_data.csv', stringsAsFactors = FALSE)
summary(data)
str(data)

#Funckija za normalizaciju podataka
normalize_max <- function(col) {
  return(col / max(abs(col)))
}

#Normalizacija podataka
data.std = apply(data[, c(1:30)], MARGIN = 2, FUN = normalize_max)
data.std = as.data.frame(data.std)
data.std$X1 = data$X1

#Mesanje redova u data setu
set.seed(123)
data.std <- data.std[sample(1:nrow(data.std)), ]

#Kreiranje trening i test seta
library(caret)
set.seed(123)
train.indicies <-
  createDataPartition(data.std$X1, p = 0.65, list = FALSE)
train.data <- data.std[train.indicies, ]
test.data <- data.std[-train.indicies,]

#Treniranje neuronske mreze
library(neuralnet)

learning_rates <- c(0.2, 0.4, 0.6)
hidden_nodes <- c(10, 20, 30)

models <- list()
predictions <- list()
mses <- data.frame()
iteration <- 1

for (lr in learning_rates) {
  for (n in hidden_nodes) {
    nn <- neuralnet(
      X1 ~ .,
      data = train.data,
      err.fct = 'ce',
      hidden = n,
      learningrate = lr,
      linear.output = FALSE
    )
    prediction <- predict(nn, newdata = test.data)
    mse <- mean((test.data$X1 - prediction) ^ 2)
    
    models[[paste("lr", lr, "hidden", n, sep = "_")]] <- nn
    predictions[[paste("lr", lr, "hidden", n, sep = "_")]] <-
      prediction
    mses <- rbind(mses, c(iteration, mse))
    iteration <- iteration + 1
  }
}

print(mses)
colnames(mses) <- c('Iteration', 'MSE')
mses[order(mses$MSE, decreasing = FALSE), ]

#Mreza sa najmanjom greskom je mreza iz iteracije 9
#Sa learning rateom 0.6 i nodes 30
print(models$lr_0.6_hidden_30)

#Cuvanje mreze u fajl
saveRDS(models$lr_0.6_hidden_30, file = 'neural_net_model.rds')
