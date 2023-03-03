---
title: "Principle Component Analysis: An Overview"
date: 2022-07-24T15:23:28+01:00
draft: false
theme: PaperMod
math: true
---


PCA is one of the most widely used methods in data science/machine learning. It is mainly used for reducing the dimension of a high dimensional data that comes handy when extracting important features relevant for further data analysis/machine learning algorithms. For example, in linear regression not having correlation betweeen principle components alleviate the bit of headache when dealing with original variables that are often correlated and give inconsistent p-values depending on which order they were included in estimating the coefficients, not to mention the reduced computational cost from dealing with a few relevant principle components as opposed to the full-extent of the original variables. The lower-dimension, to which the data is projected to, is chosen so that a certain desired fraction of the full extent of the original data is retained in the projected lower-dimensional space. 



To illustrate the application and the workings of PCA, we will start with a simple fake-data with individual entries recording values of two features/variables. Many such entries make up a rectangular format of data with each row corresponding to an entry and each column corresponding to a feature/variable. Many statistical packages/environments specialize in manipulating such rectangular data (often called dataframes) and the rectangular array of numbers lend themselves to myriads of matrix-decomposition techniques of which singular-value-decomposition (SVD) is one, the backbone behind PCA. We will look at the inner workings of SVD in a later post. For now we will focus mainly on interpreting the results of PCA (SVD) for the practical benefits of an analyst/a data-scientist.

 
![HaHa][/posts/pca/fig1_pca_scatter.png]



Fig.1 shows a scatterplot of points representing the data. A projection of these points to a line is shown in Fig.1a. It is not any line but a line that is closest to all the points in the data. The points, upon being projected to this line, moved the least they could when projected to any line we can imagine. This line is the first principal component axis. 

In some lucky cases, the points would have hardly moved at all. In these cases, we can use the projected points as a proxy for the original points. Instead of storing, let’s say, twenty numbers for an original data point, we need just one number locating it along the principal component axis. This first principal component retains the maximum information about the original data. In other words, it explains the most variance present in the original data. Now, consider all the lines that run perpendicular to the first principal component axis. Of all those lines, one is closest to the data points. It is, as you may have guessed, the second principal component. The second principal component explains most of the variance left unexplained by the first principal component. We can extend this process to get at more principal components until all the variance is captured by all of the principal components we iteratively found. We should be able to faithfully reconstruct the original data from the sum of projected data along these principal component axes.






## Within-groups sum of squared distances (WSS): #

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


