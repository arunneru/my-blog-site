---
title: "First"
date: 2022-07-24T15:23:28+01:00
draft: false
theme: PaperMod
math: true
---



# Need for validation of clusters

Clustering algorithms are designed such that they come out with a given number of clusters even if the underlying data is devoid of any such clusters.
We will see a criterion to assess the credibility of the clusters produced by any clustering algorithm.

## Within-groups sum of squared distances (WSS): 

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


