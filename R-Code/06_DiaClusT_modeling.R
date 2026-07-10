# in this script the models for the classification of the 4 clusters are created, for this purpose a multinomial regression is performed. 
# the metrics are calculated using the one-vs-rest approach. then a roc and corresponding auc-values are computed

library(nnet)
library(caret)
library(pROC)
library(dplyr)

# df contains clustering result + column of triglycerides measurement
# first visit of a patient is used

set.seed(123)

## cluster == factor with fixed levels
df$cluster <- factor(df$cluster, levels = c(1, 2, 3, 4))

n_iter <- 100

overall_results <- data.frame()
cluster_results <- data.frame()

for (i in seq_len(n_iter)) {
  
##----------------------------------------------------
## 70/30 split
##----------------------------------------------------
train_index <- createDataPartition(df$cluster,p = 0.7, list = FALSE)
  
training_ds <- df[train_index, ]
test_ds     <- df[-train_index, ]
  
##----------------------------------------------------
##  multinomial model
##----------------------------------------------------
model <- multinom(cluster ~ gender + age + mean_BMI + HbA1c + TRIG,
    data = training_ds,trace = FALSE)
  
##----------------------------------------------------
## prediction
##----------------------------------------------------
  pred <- predict(model, newdata = test_ds)
  
  probs <- predict(model, newdata = test_ds, type = "probs")
  
  if (is.null(dim(probs))) {
    probs <- matrix(probs, nrow = 1)
    colnames(probs) <- levels(df$cluster)
  }
  
##----------------------------------------------------
## create confusion matrix
##----------------------------------------------------
  cm <- confusionMatrix(
    factor(pred, levels = levels(df$cluster)),
    factor(test_ds$cluster, levels = levels(df$cluster))
  )
  
##----------------------------------------------------
## model metrics
##----------------------------------------------------
  overall_results <- rbind(
    overall_results,
    data.frame(
      iteration = i,
      accuracy = unname(cm$overall["Accuracy"]),
      kappa    = unname(cm$overall["Kappa"]),
      sensitivity = unname(cm$byClass["Sensitivity"]),
      specificity = unname(cm$byClass["Specificity"]),
      precision   = unname(cm$byClass["Precision"]),
      F1          = unname(cm$byClass["F1"])
    )
  )
  
##----------------------------------------------------
## cluster metrics
##----------------------------------------------------
  tmp <- as.data.frame(cm$byClass)
  tmp$cluster <- levels(df$cluster)
  tmp$iteration <- i
  
##----------------------------------------------------
## AUC for each cluster
##----------------------------------------------------
  tmp$AUC <- NA_real_
  
  for (k in seq_along(levels(df$cluster))) {
    
    cl <- levels(df$cluster)[k]
    
    if (cl %in% colnames(probs)) {
      tmp$AUC[k] <- as.numeric(
        roc(response = test_ds$cluster == cl, predictor = probs[, cl],quiet = TRUE)$auc
      ) 
    }
  }
  cluster_results <- rbind(cluster_results, tmp)  
}

##----------------------------------------------------
## overall model performance
##----------------------------------------------------
overall_summary <- overall_results %>%
  summarise(
    accuracy_mean = mean(accuracy),
    accuracy_sd   = sd(accuracy),
    kappa_mean    = mean(kappa),
    sensitivity   = mean(cm$byClass[, "Sensitivity"], na.rm = TRUE),
    specificity   = mean(cm$byClass[, "Specificity"], na.rm = TRUE),
    precision     = mean(cm$byClass[, "Precision"], na.rm = TRUE),
    F1            = mean(cm$byClass[, "F1"], na.rm = TRUE),
  )
overall_summary

##----------------------------------------------------
## performance by cluster
##----------------------------------------------------
cluster_summary <- cluster_results %>%
  group_by(cluster) %>%
  summarise(
    sensitivity = mean(Sensitivity, na.rm = TRUE),
    specificity = mean(Specificity, na.rm = TRUE),
    precision   = mean(Precision, na.rm = TRUE),
    F1          = mean(F1, na.rm = TRUE),
    balancedAcc = mean(`Balanced Accuracy`, na.rm = TRUE),
    AUC         = mean(AUC, na.rm = TRUE)
  )

cluster_summary

#  saveRDS(model, file="XXX.rds")  
