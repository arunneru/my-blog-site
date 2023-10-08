y---
title: "Non-negative matrix factorization with gene expression data"
date: 
draft: true
theme: PaperMod
math: true
---

Non-negative matrix factorization is an ensemble of methods that approximate the data ( in a matrix form ) as a product of two simple, interpretable matrices. 

It gained prominence after Lee & Seung developed and used an algorithm that automatically identified local features like eyes and noses in facial image datasets. 

It differs from other factorization methods such as SVD, PCA and ICA in one crucial aspect that gives this method its name. The input matrix it operates on and the two simple factor matrices it yields are all non-negative matrices: None of their elements are negative – they are either zero or positive numbers.

In the facial image dataset, images are initially stored as rectangular array of numbers each carrying the grey intensity of individual pixels. These rectangular array corresponding to an image is then unwrapped to form a long, one-dimensional vector.  The vectors so formed from the many images are then combined column-wise to form the input matrix X. As the intensity of the pixels are non-negative numbers, non-negative matrix factorization could be applied to this input matrix X.  The output of the factorization yields two non-negative matrices W and H that when multiplied yield an approximation to the original matrix. This loss of information due to approximation is more than compensated by the fewer number of columns in W (and equal number of rows in H); This fewer basis vectors (as the columns of W are called) combined with the non-negative nature of these matrices allow for comprehension and interpretation of the hidden features of the originally incomprehensible, complex data.

Many sophisticated algorithms for non-negative factorization exist that concern themselves with various aspects like speed, accuracy, sparsity of the basis vectors or the h vectors and so on. Here, we will use a method called sparse non-negative matrix factorization due to Park et. al, 2007, that was implemented in Nimfa python library. I will decsribe in brief how we downloaded the raw counts for the RNAseq data and prepared it into a matrix form suitable for factorization by the Nimfa library. 




 ![ImOne](/image/correlation_plot.png)

Fig.1. Correlation matrix of all the variables. Gross Domestic Product (GDP) shows high positive correlation with population size while happiness index shows high positive correlation with variables like access\_electricity, human development indices (hdi\_2017 and hdi_2018), and life expectancies

```{r Linear regression."}

standardize = function(x){
  
  z <- (x-mean(x))/sd(x)
  return(z)
  }

set.seed(4)
train <- sample(1:nrow(x), ceiling(2*nrow(x)/3))
test <- (-train)


blacksheep = function(x){
  
  as.numeric(x)
  }

final_dat_stand <- apply(final_dat_complete, 2, blacksheep)
#final_dat_stand <- as.data.frame(scale(final_dat_stand))

dat_train <- final_dat_stand[train,]
dat_test <- final_dat_stand[test,]
dat_train <- as.data.frame(scale(dat_train))
dat_test <- as.data.frame(scale(dat_train))

x <- apply(x, 2, standardize)
y <- standardize(y)

lm_fit <- lm(gdp~., data=final_dat_stand)


#lasso_pred <- sapply(seq_along(cv.out$lambda), function(i){predict(lasso_fit, s=cv.out$lambda[i], newx = x_test)})
#l_err <- apply(lasso_pred, 2, function(x){sum((x-y_test)^2)})
#plot(ln(grid),l_err)
```




## Splitting the data into training and test batch and fitting the lasso ##

```{r cross-validation, fig.cap = ""We plotted the Average Mean Squared Errors (MSEs) against the tuning parameter, which controlled the extent of the sparsity constraint within the loss function, during the bootstrapping procedure. The vertical bars, straddling individual data points, represent the standard deviation of the errors."}


library(glmnet)
standardize = function(x){
  
  z <- (x-mean(x))/sd(x)
  return(z)
  }

library(SciViews)
x <- data.matrix(final_dat_complete[,predictors])
y <- data.matrix(final_dat_complete[,response])

set.seed(4)

train <- sample(1:nrow(x), ceiling(2*nrow(x)/3))
test <- (-train)

x_train <- x[train,]
y_train <- y[train]
x_train <- apply(x_train, 2, standardize)
y_train <- standardize(y_train)

x_test <- x[test,]
y_test <- y[test]

x_test <- apply(x_test, 2, standardize)
y_test <- standardize(y_test)

x <- apply(x, 2, standardize)
y <- standardize(y)

#lasso_fit <- glmnet(x[train, ], y[train], alpha=1) ## alpha=1 --> the lasso, alpha=0 --> ridge regression, 0<alpha<1 --> elastic net

lasso_fit <- glmnet(x_train, y_train, alpha=1)
#set.seed(1)
cv.out <- cv.glmnet(x_train, y_train, alpha=1)
plot(cv.out)

bestlam <- cv.out$lambda.min
# lasso_pred <- predict(lasso_fit, s=bestlam, newx = x_test) #
# mean((lasso_pred - y_test)^2) #
#  #
grid <- cv.out$lambda
out <- glmnet(x_train, y_train, alpha=1, lambda = grid)
lasso_coef <- predict(out, type = "coefficients", s=bestlam)


#lasso_pred <- sapply(seq_along(cv.out$lambda), function(i){predict(lasso_fit, s=cv.out$lambda[i], newx = x_test)})
#l_err <- apply(lasso_pred, 2, function(x){sum((x-y_test)^2)})
#plot(ln(grid),l_err)
```

![ImTwo](/image/cross_validation.png)

Fig.2. We plotted the Average Mean Squared Errors (MSEs) against the tuning parameter, which controlled the extent of the sparsity constraint within the loss function, during the bootstrapping procedure. The vertical bars, straddling individual data points, represent the standard deviation of the errors.

```{r variable-non-zero-coef, fig.cap = "In the linear model with GDP as the response variable and the minimum cross-validation error, the coefficients of significant variables are retained. Conversely, the lasso method introduces additional sparsity constraints, reducing the coefficients of other variables to zero."}

lasso_coef_df <- as.data.frame(matrix(lasso_coef))
lasso_coef_df$predictors <- rownames(lasso_coef)
colnames(lasso_coef_df) <- c("size_of_the_coefficients", "predictors")


p <- ggplot(lasso_coef_df, aes(x=predictors, y=size_of_the_coefficients)) + geom_col(width=0.5)
p + coord_flip()#+
  #labs(title="Important variables (with non-zero coefficients)",
  #subtitle="Response: GDP")
```
![ImThree](/image/coef_variableNonZero_gdp.png)

Fig.3. In the linear model with GDP as the response variable and the minimum cross-validation error, the coefficients of significant variables are retained. Conversely, the lasso method introduces additional sparsity constraints, reducing the coefficients of other variables to zero. 


## Happiness Index as the response variable ##

```{r}


response <- "happiness_index"
predictors <- c("gdp",
                "life_exp_at_birth", 
                "life_exp_at_birth_male", 
                "life_exp_at_birth_female", 
                "co2_emi", 
                "income_per_capita",
                "population_size",
                "inflation_consumer_prices",
                "mortality_rate",
                "infant_mortality_rate",
                "access_electricity",
                "hdi_2017",
                "hdi_2018",
                "gini_coefficient",
                "privCap_2_gdp", 
                "publCap_2_gdp", 
                "debt_2_gdp")
```


## Splitting the data into training and test batch and fitting the lasso - for happiness ##
```{r cross-validation-happiness, fig.cap = "We plotted the Average Mean Squared Errors (MSEs) against the tuning parameter, which controlled the extent of the sparsity constraint within the loss function, during the bootstrapping procedure. The vertical bars, straddling individual data points, represent the standard deviation of the errors."}
library(glmnet)
standardize = function(x){
  
  z <- (x-mean(x))/sd(x)
  return(z)
}

library(SciViews)
x <- data.matrix(final_dat_complete[,predictors])
y <- data.matrix(final_dat_complete[,response])

set.seed(4)

train <- sample(1:nrow(x), ceiling(2*nrow(x)/3))
test <- (-train)

x_train <- x[train,]
y_train <- y[train]
x_train <- apply(x_train, 2, standardize)
y_train <- standardize(y_train)

x_test <- x[test,]
y_test <- y[test]

x_test <- apply(x_test, 2, standardize)
y_test <- standardize(y_test)

x <- apply(x, 2, standardize)
y <- standardize(y)

#lasso_fit <- glmnet(x[train, ], y[train], alpha=1) ## alpha=1 --> the lasso, alpha=0 --> ridge regression, 0<alpha<1 --> elastic net

lasso_fit <- glmnet(x_train, y_train, alpha=0)
#set.seed(1)
cv.out <- cv.glmnet(x_train, y_train, alpha=0)
plot(cv.out)

bestlam <- cv.out$lambda.min
# lasso_pred <- predict(lasso_fit, s=bestlam, newx = x_test) #
# mean((lasso_pred - y_test)^2) #
#  #
grid <- cv.out$lambda
out <- glmnet(x_train, y_train, alpha=0,lambda = grid)
lasso_coef <- predict(out, type = "coefficients", s=bestlam)


#lasso_pred <- sapply(seq_along(cv.out$lambda), function(i){predict(lasso_fit, s=cv.out$lambda[i], newx = x_test)})
#l_err <- apply(lasso_pred, 2, function(x){sum((x-y_test)^2)})
#plot(ln(grid),l_err)
```

```{r variables-non-zero-coef, fig.cap = "The coefficients of the important variables in the linear model (with the minimum cross-validation error) with Happiness Index as the response variable. Notice new predictors that predict happiness index (like access to electricity ) and some opposite trends (like population size) ."}

lasso_coef_df <- as.data.frame(matrix(lasso_coef))
lasso_coef_df$predictors <- rownames(lasso_coef)
colnames(lasso_coef_df) <- c("size_of_the_coefficients", "predictors")


p <- ggplot(lasso_coef_df, aes(x=predictors, y=size_of_the_coefficients)) + geom_col(width=0.5)
p + coord_flip()
```

![ImThree](/image/coef_variableNonZero_happiness.png)

Fig.4. The coefficients of the important variables in the linear model (with the minimum cross-validation error) with Happiness Index as the response variable. Notice new predictors that predict happiness index (like access to electricity ) and some opposite trends (like population size).
