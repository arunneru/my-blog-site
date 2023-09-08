---
title: "Growth and Happiness: the Lasso"
date: 
draft: false
theme: PaperMod
math: true
---

# Growth, Development and Happiness - the lasso with some socio-economic indicators #

The lasso is a modified regression method that gained in popularity after the works of Tibshirani. It is like linear regression in that it minimizes the deviation of the observed responses from the predictions of a linear model. It differs from linear regression in that it picks important predictors/variables (i.e, does variable selection) from the available predictors. The linear model produced by linear regression has non-zero coefficients for all the variables. This might be disadvantageous if there are a large number of variables that do not influence or modify the outcome or response in any major way. For example, we might be interested in predicting gdp (gross domestic product) from the values of many socio-economic factors/predictors. In this case, a few variables like per-capita income of a country might correlate with gdp while many other variables like the healthy life expectancy might not correlate with it. If we have many such indicators/factors/variables that are not relevant and are not correlated with the desired outcome variable, the way of the linear regression (i.e, including all the variables in the model) would seem less efficient way to come up with a predictive model. The lasso alleviates this problem by constraining the sum of absolute values of the coefficients to be lower than otherwise ( The loss function includes both the sum of squared errors as well as the sum of L1 norms of the coefficients). This has the desired effect of bringing some coefficients (those of non-relevant variables) all the way to zero thereby eliminating the corresponding variables completely from the linear model. 


In this article, we will apply the lasso method on a set socio-economic indicators (All from the year 2017 unless stated otherwise) of countries sampled from World Bank, International Monetary Fund and other similar sources and we will use the GDP (gross domestic product) as a response variable. Some of the indicators extracted from these sources are known to be highly correlated with the gross domestic product while others, infamously, not so. We will see if the lasso eliminates these variables in its attempt to satisfy the sparsity constraints. 


We would also like to know if these socio-economic factors can be used to predict the world happiness index, which was, like many similar indices, developed in part to address the shortcomings of GDP. The world happiness index, inspired from a similar index due to the Bhutan government, was developed by academics from Harvard University and is estimated from surveys conducted across the globe. 



## Downloading the relevant data ##


## Some world bank development indicators ##

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
