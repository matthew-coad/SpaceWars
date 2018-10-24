

responseVariable <- "Standard.Salary"
predictorVariables <- model_predictors()
survey_df <- survey_assemble()

# split data into training and testing chunks
set.seed(1234)
splitIndex <- caret::createDataPartition(survey_df$Standard.Salary, p = .75, list = FALSE, times = 1)
trainDF <- survey_df[splitIndex,]
testDF <- survey_df[-splitIndex,]

# create caret trainControl object to control the number of cross-validations performed
objControl <- caret::trainControl(method = 'cv', number = 5, returnResamp = 'none')

# run model
objModel <- caret::train(trainDF[, predictorVariables], trainDF[, responseVariable], method = 'glmnet', metric = "RMSE", trControl = objControl)

# get predictions on your testing data
predictions <- predict(object = objModel, testDF[, predictorsNames])

library(pROC)
auc <- roc(testDF[, outcomeName], predictions)
print(auc$auc)

postResample(pred = predictions, obs = testDF[, outcomeName])

# find out variable importance
summary(objModel)
plot(varImp(objModel, scale = F))

# find out model details
objModel

# display variable importance on a +/- scale 
vimp <- varImp(objModel, scale = F)
results <- data.frame(row.names(vimp$importance), vimp$importance$Overall)
results$VariableName <- rownames(vimp)
colnames(results) <- c('VariableName', 'Weight')
results <- results[order(results$Weight),]
results <- results[(results$Weight != 0),]

par(mar = c(5, 15, 4, 2)) # increase y-axis margin. 
xx <- barplot(results$Weight, width = 0.85,
              main = paste("Variable Importance -", outcomeName), horiz = T,
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE,
              col = ifelse((results$Weight > 0), 'blue', 'red'))
axis(2, at = xx, labels = results$VariableName, tick = FALSE, las = 2, line = -0.3, cex.axis = 0.6)



df <- survey_assemble()

salaryFormula <- salary_formula()
x = model.matrix(salaryFormula, df)[, -1]
y = df$Standard.Salary
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

mean((mean(y[train]) - y.test) ^ 2)

grid = 10 ^ seq(10, -2, length = 100)
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
sd1am = cv.out$lambda.1se

lasso.bestpred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.bestpred - y.test) ^ 2)

lasso.sd1pred = predict(lasso.mod, s = sd1am, newx = x[test,])
mean((lasso.sd1pred - y.test) ^ 2)

out = glmnet(x, y, alpha = 1)

head(broom::tidy(out))
lasso.bestcoef = predict(out, type = "coefficients", s = bestlam)
dim(lasso.bestcoef)
lasso.bestcoef

lasso.sd1coef = predict(out, type = "coefficients", s = sd1am)
lasso.sd1coef <- lasso.sd1coef[1:nrow(lasso.sd1coef),]
terms <- names(lasso.sd1coef)[abs(lasso.sd1coef) > 0.00000001]
variables <- stri_replace_last_regex(terms, "TRUE$", "")
? glmnet


glmnet_classifier <- function(df, responseVariable, predictorVariables) {

    # split data into training and testing chunks
    set.seed(1234)
    splitIndex <- createDataPartition(df[[responseVariable]], p = .75, list = FALSE, times = 1)
    trainDF <- df[splitIndex,]
    testDF <- df[-splitIndex,]

    control <- trainControl(method = 'cv', number = 5, returnResamp = 'none')
    tuneGrid = 10 ^ seq(10, -2, length = 100)

    # run model
    objModel <- train(x = trainDF[, predictorVariables], trainDF[[responseVariable]], method = 'glmnet', metric = "RMSE", trControl = control, tuneGrid = tuneGrid)

    # get predictions on your testing data
    predictions <- predict(object = objModel, testDF[, predictorVariables])

    auc <- roc(testDF[[responseVariable]], predictions)
    print(auc$auc)

    postResample(pred = predictions, obs = testDF[, outcomeName])

    # find out variable importance
    summary(objModel)
    plot(varImp(objModel, scale = F))

    # find out model details
    objModel

    # display variable importance on a +/- scale 
    vimp <- varImp(objModel, scale = F)
    results <- data.frame(row.names(vimp$importance), vimp$importance$Overall)
    results$VariableName <- rownames(vimp)
    colnames(results) <- c('VariableName', 'Weight')
    results <- results[order(results$Weight),]
    results <- results[(results$Weight != 0),]

    par(mar = c(5, 15, 4, 2)) # increase y-axis margin. 
    xx <- barplot(results$Weight, width = 0.85,
              main = paste("Variable Importance -", outcomeName), horiz = T,
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE,
              col = ifelse((results$Weight > 0), 'blue', 'red'))
    axis(2, at = xx, labels = results$VariableName, tick = FALSE, las = 2, line = -0.3, cex.axis = 0.6)

    control <- caret::trainControl(method = "cv", number = 10)
    seed <- 7
    metric <- "Accuracy"
    set.seed(7)
    fit.glmnet <- train(formula, data = df, method = "glmnet", metric = metric, preProc = c("center", "scale"), trControl = control)


    # Linear Regresson Models

    # Linear regression
    set.seed(seed)
    fit.lm <- train(Employed ~ ., data = dataset, method = "lm", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Robust Linear Model
    set.seed(seed)
    fit.rlm <- train(Employed ~ ., data = dataset, method = "rlm", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Partial Least Squares
    set.seed(seed)
    fit.pls <- train(Employed ~ ., data = dataset, method = "pls", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Elasticnet
    set.seed(seed)
    fit.enet <- train(Employed ~ ., data = dataset, method = "enet", metric = metric, preProc = c("center", "scale"), trControl = control)
    # glmnet


}


# dim(df)

df <- survey_assemble()

salaryFormula <- salary_formula()
x = model.matrix(salaryFormula, df)[, -1]
y = df$Standard.Salary
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

mean((mean(y[train]) - y.test) ^ 2)

grid = 10 ^ seq(10, -2, length = 100)
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
sd1am = cv.out$lambda.1se

lasso.bestpred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.bestpred - y.test) ^ 2)

lasso.sd1pred = predict(lasso.mod, s = sd1am, newx = x[test,])
mean((lasso.sd1pred - y.test) ^ 2)

out = glmnet(x, y, alpha = 1)

head(broom::tidy(out))
lasso.bestcoef = predict(out, type = "coefficients", s = bestlam)
dim(lasso.bestcoef)
lasso.bestcoef

lasso.sd1coef = predict(out, type = "coefficients", s = sd1am)
lasso.sd1coef <- lasso.sd1coef[1:nrow(lasso.sd1coef),]
terms <- names(lasso.sd1coef)[abs(lasso.sd1coef) > 0.00000001]
variables <- stri_replace_last_regex(terms, "TRUE$", "")
? glmnet
