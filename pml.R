library(caret)
library(randomForest)
pml.data <- read.table("pml-training.csv", sep = ",", header = TRUE, na.strings = c("NA", "#DIV/0!"))

delete.info <- grep("^X$|user_name|raw_timestamp_part_1|raw_timestamp_part_2|cvtd_timestamp|new_window|num_window", colnames(pml.data))
delete.var <- grep("^(kurtosi|skewness|amplitude|max|min|var|avg|stddev)", colnames(pml.data))
delete.index <- c(delete.info, delete.var)
train.data <- pml.data[, -delete.index]

set.seed(125)
in.train <- createDataPartition(y = train.data$classe, p=0.7, list=FALSE)
training <- train.data[in.train, ]
testing <- train.data[-in.train, ]

mod.fit <- train(classe ~ ., method="rf", data = training, tuneGrid = data.frame(mtry = 20))
fancyRpartPlot(mod.fit$finalModel)

pred <- predict(mod.fit, newdata = testing)
confusionMatrix(pred, testing$classe)
