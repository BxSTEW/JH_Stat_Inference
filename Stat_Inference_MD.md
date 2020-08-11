---
title: "Stat_Inference_Project"
author: "Brian Stewart"
date: "8/9/2020"
output:
  html_document:
    df_print: paged
---

# Part 1: Simulation 

## Overview

In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.  

## Instructions

Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials. You should

1. Show the sample mean and compare it to the theoretical mean of the distribution.
2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
3. Show that the distribution is approximately normal.  

## Create Data Enviroment

Loading in ggplot2 for the visualizations and stats for variance.  
```{r}
library(ggplot2)
library(stats)
```

setting each variable/object to the desired number per the overview, and setting the seed for reproducibility.  

```{r}
n <- 40
lambda <- 0.2
nsim <- 1000
set.seed(123)
```

Now we need to create a matrix for the simulation. The calculate the averages across the exponential distribution.   

```{r}
sim <- matrix(rexp(n * nsim, rate = lambda), nsim)

sim_means <- rowMeans(sim)
```


## Comparing the Means (Theorectical vs. Sample)

To calculate the sample mean we need to get the average of the means we created before and saved to sim_means.  

```{r}
sample_mean <- mean(sim_means)

sample_mean
```


Next we need to determine the theorectical mean by dividing 1 by lambda.  

```{r}
theory_mean <- 1/lambda

theory_mean
```

The means are almost identical which supports the central limit theorem. But let's see it visually.  

```{r}
means <- vector()
for (i in 1:1000) {
    means <- c(means, mean(rexp(n, lambda)))
}
hist(means, breaks=40)
lines(density(means))
abline(v=1/lambda, col="red")
```


## Comparing Variance (Theorectical vs. Sample)  

Variance must be calcuated to see the difference in the two distribution.  

First the sample variance:  

```{r}
sample_var <- var(sim_means)

sample_var
```


Next is the theorectical variance:  

```{r}
theory_var <- (1 / lambda) ^ 2 / n

theory_var
```

Much like the means of the distribution the variance is also very similar between the two distributions again supporting the CLT.  

Next, the standard deviation needs to be looked at to fully support the CLT of the distribution.  

Again starting with the sample.  

```{r}
sample_sd <- sd(sim_means)

sample_sd
```


An now for the theoretical.  

```{r}
theory_sd <- 1/(lambda * sqrt(n))

theory_sd
```

Judging by the three tests run in this simulation the data supports the CLT because all sample values and theorectical values are almost identical.  


We can visulize this with the following graph.  
```{r}
plotdata <- data.frame(sim_means)
g <- ggplot(plotdata, aes(x =sim_means))
g <- g + geom_histogram(aes(y=..density..), colour="black",
                        fill = "lightblue")
g <- g + labs(title = "Distribution of averages of 40 Samples", x = "Mean of 40 Samples", y = "Density")
g <- g + geom_vline(aes(xintercept = sample_mean))
g <- g + geom_vline(aes(xintercept = theory_mean))
g <- g + stat_function(fun = dnorm, args = list(mean = sample_mean, sd = sample_sd), color = "blue", size = 1.0)
g <- g + stat_function(fun = dnorm, args = list(mean = theory_mean, sd = theory_sd), colour = "red", size = 1.0)
g
```



Lastly, the confidence intervals for each distribution needs to be assessed.  

Fist the sample.  
```{r}
sample_CI <- round(mean(sim_means) + c(-1,1)*1.96*sd(sim_means)/sqrt(n),3)

sample_CI
```

Now the Theoretical
```{r}
theory_CI <- theory_mean + c(-1,1) * 1.96 * sqrt(theory_var)/sqrt(n)

theory_CI
```


Again these support the CLT and prove that we have a normal distribution of data.  

## Conclusion
Just a restatement of the previous analysis the sample taken shows normal distribution statistics and if it were a true sample would be valid for testing. The Central Limit Theorem was supported and the graph is not skewed one way or the other requiring data wrangling or accounting for outliers.