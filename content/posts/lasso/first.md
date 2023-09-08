---
title: "Growth, Development and Happiness - the lasso with some socio-economic indicators"
date: 
draft: false
theme: PaperMod
math: true
---

The lasso is a modified regression method that gained in popularity after the works of Tibshirani. It is like linear regression in that it minimizes the deviation of the observed responses from the predictions of a linear model. It differs from linear regression in that it picks important predictors/variables (i.e, does variable selection) from the available predictors. The linear model produced by linear regression has non-zero coefficients for all the variables. This might be disadvantageous if there are a large number of variables that do not influence or modify the outcome or response in any major way. For example, we might be interested in predicting gdp (gross domestic product) from the values of many socio-economic factors/predictors. In this case, a few variables like per-capita income of a country might correlate with gdp while many other variables like the healthy life expectancy might not correlate with it. If we have many such indicators/factors/variables that are not relevant and are not correlated with the desired outcome variable, the way of the linear regression (i.e, including all the variables in the model) would seem less efficient way to come up with a predictive model. The lasso alleviates this problem by constraining the sum of absolute values of the coefficients to be lower than otherwise ( The loss function includes both the sum of squared errors as well as the sum of L1 norms of the coefficients). This has the desired effect of bringing some coefficients (those of non-relevant variables) all the way to zero thereby eliminating the corresponding variables completely from the linear model. 


In this article, we will apply the lasso method on a set socio-economic indicators (All from the year 2017 unless stated otherwise) of countries sampled from World Bank, International Monetary Fund and other similar sources and we will use the GDP (gross domestic product) as a response variable. Some of the indicators extracted from these sources are known to be highly correlated with the gross domestic product while others, infamously, not so. We will see if the lasso eliminates these variables in its attempt to satisfy the sparsity constraints. 


We would also like to know if these socio-economic factors can be used to predict the world happiness index, which was, like many similar indices, developed in part to address the shortcomings of GDP. The world happiness index, inspired from a similar index due to the Bhutan government, was developed by academics from Harvard University and is estimated from surveys conducted across the globe. 



### Downloading the relevant data ###


### Some world bank development indicators ###

First, we will download some important indicators from world bank through the WDI (World Development Indicators) package in R. You can find many online resources on how to find and download any relevant indicators. Here is a link for one: https://worldpoliticsdatalab.org/tutorials/data-wrangling-and-graphing-world-bank-data-in-r-with-the-wdi-package/


```{r }

#WDI::languages_supported()
#WDIsearch('gdp')[1:10,]

dat = WDI(indicator=c("NY.GDP.MKTP.CD",# gdp -# the gross domestic product
                    "SP.DYN.LE00.IN", ## Life expectancy at birth (For all)
                    "SP.DYN.LE00.MA.IN", # Life expectancy at birth (For males)
                    "SP.DYN.LE00.FE.IN", # Life expectancy at birth (For females)
                    "EN.ATM.CO2E.PC", # Co2 emission in metric tonne
                    "NY.GDP.PCAP.KD", # Per capita income
                    "SP.POP.TOTL", # Population size ## in thousands
                    "FP.CPI.TOTL.ZG", # Inflation_consumer_prices ## percentage
                    "SP.DYN.CDRT.IN", # Mortality_rate ## for every 1000
                    "SP.DYN.IMRT.IN", # Infant_mortality_rate ## for every thousand births
                    "EG.ELC.ACCS.ZS"), # access_to_electricity ## percent of the pop.
                  start=2017, end=2017)
```



## Human development index data ##

The file was manually downloaded from the website ( [https://hdr.undp.org/data-center] (https://hdr.undp.org/data-center)) and the development indices for 2017 and 2018 were extracted and copied to a separate file. 

```{r}

dat_dev_ind <- read_csv("human_development_index_2017_18.csv")
dat_dev_ind

```

## World happiness index ##

World happiness index for 2018 is downloaded from world happiness report 2018 website [https://worldhappiness.report/ed/2018/] (https://worldhappiness.report/ed/2018/) .

```{r}
dat_hap_ind <- read_excel("happiness_index.xlsx")
```




I have also downloaded data from IMF (International Monetary Fund) on some more economic factors, which are stored in separate csv files. We will download them into R dataframe in our local workspace.


```{r}
d2g <- read_excel("debt_2_gdp.xlsx")
prcap <- read_excel("private_cap_data.xlsx")
pucap <- read_excel("public_cap_data.xlsx")
sub <- read_excel("subsidies_data.xlsx")
tax <- read_excel("taxRevenue_data.xlsx")

d2g <- subset(d2g, select=-c(...1))
colnames(d2g) <- c("debt_2_gdp", "iso2c")
prcap <- subset(prcap, select=-c(...1))
pucap <- subset(pucap, select=-c(...1))
sub <- subset(sub, select=-c(...1))
colnames(sub) <- c("iso2c", "subsidies")
tax <- subset(tax, select=-c(...1))
colnames(tax) <- c("iso2c", "tax_revenue")
```



## helper file from countrycode package ##

We need to combine all these separate data sets into a single one. Though we can combine them based on the country names, a more efficient way would be to use iso-3/iso-2 code or imf code (From International monetary fund). The library "countrycode" offers these codes for all the countries. 


```{r}

dat_with_hap <- dat %>% inner_join(dat_hap_ind, by="iso3c")
dat_hap_dev <- dat_with_hap %>% inner_join(dat_dev_ind, by="iso3c")

```



```{r}

library(countrycode)
data(codelist)
country_set <- codelist
country_set <- country_set %>%
  select(country.name.en, iso2c, iso3c, imf, continent, region) %>% filter(!is.na(imf) & !is.na(iso2c))

prcap$imf <- as.numeric(prcap$imf)
pr_dat <- country_set %>% inner_join(prcap, by="imf")
pucap$imf <- as.numeric(pucap$imf)
prNpu_dat <- pr_dat %>% inner_join(pucap, by="imf")
prNpuDe_dat <- prNpu_dat %>% inner_join(d2g, by="iso2c")

final_dat <- dat_hap_dev %>% inner_join(prNpuDe_dat, by="iso2c")
final_dat <- subset(final_dat, select=-c( country.name.en,iso3c.y, country.y, country.x))
colnames(final_dat)
```


Giving some readable column names to the new dataframe
```{r}
colnames(final_dat) <- c("iso2c", 
                         "iso3c",
                         "year",
                         "gdp",
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
                         "happiness_index",
                         "country",
                         "hdi_2017",
                         "hdi_2018",
                         "imf", 
                         "continent", 
                         "region", 
                         "privCap_2_gdp", 
                         "publCap_2_gdp", 
                         "debt_2_gdp") 

```



We are using the median gini coefficient of a country over the years instead of gini coefficient for 2017 as there are lot of missing values if we restrict ourselves to just 2017. 

```{r}

# fifth_dat <- na.omit(fifth_dat) #
#fifth_dat

sa_da <- WDI(indicator = c("SI.POV.GINI"))

give_gini = function(iso_code){
sa_ve <- unique(sa_da[sa_da$iso3c==iso_code,]$SI.POV.GINI)
return(median(sa_ve[!is.na(sa_ve)]))
}

l_iso3 <- final_dat$iso3c
l_gini <- sapply(l_iso3, give_gini)

final_dat$gini_coefficient <- l_gini


final_dat_numeric <- final_dat %>% select(gdp, life_exp_at_birth, life_exp_at_birth_male, life_exp_at_birth_female, co2_emi, income_per_capita, population_size, inflation_consumer_prices, mortality_rate, infant_mortality_rate, access_electricity,happiness_index,hdi_2017,hdi_2018, gini_coefficient, privCap_2_gdp, publCap_2_gdp, debt_2_gdp)

```


We will use the mice package to fill in the missing values; More specifically, the lasso method from mice package to fill in the missing values.

```{r}
library(mice)
#md.pattern(final_dat_numeric)
final_dat_complete <- complete(mice(final_dat_numeric, method="lasso.norm"))
```


# Using GDP as a response variable #
s
```{r}

response <- "gdp"
predictors <- c("life_exp_at_birth", 
                "life_exp_at_birth_male", 
                "life_exp_at_birth_female",
                "co2_emi", 
                "income_per_capita",
                "population_size",
                "inflation_consumer_prices",
                "mortality_rate",
                "infant_mortality_rate",
                "access_electricity",
                "happiness_index",
                "hdi_2017",
                "hdi_2018",
                "gini_coefficient",
                "privCap_2_gdp", 
                "publCap_2_gdp", 
                "debt_2_gdp")

predictors_plt <- c("gdp","life_exp_at_birth", "life_exp_at_birth_male", "life_exp_at_birth_female", "co2_emi", "income_per_capita", "population_size", "inflation_consumer_prices","mortality_rate","infant_mortality_rate","access_electricity","happiness_index","hdi_2017","hdi_2018", "privCap_2_gdp", "publCap_2_gdp", "debt_2_gdp")
#fifth_dat[,response]
x_plt <- data.matrix(final_dat_complete[,predictors_plt])



```


## Plotting the correlation matrix with histogram and scatterplots ##

```{r correlation-plot, fig.cap = "Correlation matrix of all the variables. Gross Domestic Product (GDP) shows high positive correlation with population size while happiness index shows high positive correlation with variables like access_electricity, human development indices (hdi_2017 and hdi_2018), and life expectancies."}
library(corrplot)
M = cor(x_plt)
corrplot(M, method='color', order='alphabet')#+
#labs(title="Color plot of the Correlation matrix of all the variables")
# library("PerformanceAnalytics") #
# chart.Correlation(x_plt, histogram=TRUE, pch=19) #

```

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
```{r cross-validation, fig.cap = "Average Mean Squared Errors (MSEs) plotted against the tuning parameter -- that controlled the extent of the sparsity constraint in the loss function -- in the bootstrapping procedure with the standard deviation of the errors shown as vertical bars straddling individual data points."}
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



```{r variable-non-zero-coef, fig.cap = "The coefficients of the important variables in the linear model (with the minimum cross-validation error) with GDP as the response variable. The coefficients of other variables are reduced all the way to zero by the additional sparsity contraints introduced by the lasso method."}
lasso_coef_df <- as.data.frame(matrix(lasso_coef))
lasso_coef_df$predictors <- rownames(lasso_coef)
colnames(lasso_coef_df) <- c("size_of_the_coefficients", "predictors")


p <- ggplot(lasso_coef_df, aes(x=predictors, y=size_of_the_coefficients)) + geom_col(width=0.5)
p + coord_flip()#+
  #labs(title="Important variables (with non-zero coefficients)",
  #subtitle="Response: GDP")
```


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
```{r cross-validation-happiness, fig.cap = "Average Mean Squared Errors (MSEs) plotted against the tuning parameter -- that controlled the extent of the sparsity constraint in the loss function -- in the bootstrapping procedure with the standard deviation of the errors shown as vertical bars straddling individual data points."}
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

