---
title: "Principle Component Analysis: An Overview"
date: 2022-07-24T15:23:28+01:00
draft: false
theme: PaperMod
math: true
---



# Introduction

PCA is one of the most widely/often used methods in data science/machine learning. It is mainly used for reducing the dimension of a high dimensional data that comes handy when extracting important features relevant for further data analysis/machine learning algorithms. For example, inn linear regression there is a benefit of no-correlation betweeen principle components that is a bit of headache when dealing with original variables, not to mention the reduced computational cost from dealing with a few relevant principle components. The lower-dimension, to which the data is projected to, is chosen so that a certain desired fraction of the full extent of the original data is retained in the projected lower-dimensional space. 


# Within-groups sum of squared distances (WSS): #

$$
WSS_k =  \sum_{l=1}^k{\sum_{x_i \in {C_l}}{d^2(x_i, \overline{x_l})}}
$$

where, $k$ is the number of clusters and within the l-th cluster $C_l$, $x_l$ is the centre of mass.
We are interested in finding the *elbow* where there is a sudden drop in WSS_k as k is increased. 

```{r}
library("dplyr")
simdat <- lapply(c(0, 8), function(mx) {
    lapply(c(0, 8), function(my) {
        tibble(
            x = rnorm(100, mean = mx, sd = 2),
            y = rnorm(100, mean = my, sd = 2),
            class = paste(mx, my, sep = ":")
        )
    }) %>% bind_rows()
}) %>% bind_rows()

simdat
```


